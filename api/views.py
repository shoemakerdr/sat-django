from floorplans.models import FloorPlan, Location
from .serializers import FloorPlanSerializer, LocationCreateSerializer, LocationUpdateSerializer
from rest_framework import generics, status
from rest_framework.permissions import IsAuthenticated
from .permissions import IsOwnerOrFloorPlanIsPublic
from rest_framework.views import APIView
from rest_framework.response import Response
from sat.utils import partition



class FloorPlanDetail(generics.RetrieveUpdateAPIView):
    """
        name : 'api-floorplan'
        GET : FloorPlan instance, includes list of Locations related to the FloorPlan
        PUT : Update FloorPlan instance, cannot update Locations from this route
        POST : Update FloorPlan and associated locations
    """
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer
    permission_classes = (IsOwnerOrFloorPlanIsPublic,)

    ####
    #                             --------------
    #                             ---- POST ----
    #                             --------------
    #     This method does A LOT:
    #         It takes a full dict of floorplan data with locations
    #         and updates floorplan and all locations-- creating locations not
    #         already in the database.
    ####
    def post(self, request, pk, format=None):
        # get floorplan and location instances for serializers
        floorplan = FloorPlan.objects.get(pk=pk)
        locations = Location.objects.filter(floorplan=floorplan)

        # partition new and old locations
        (to_be_updated, to_be_created) = partition(request.data['locations'], lambda loc: loc.get('id', False))

        # instantiate serializers
        floorplan_serializer = FloorPlanSerializer(data=request.data)
        update_location_serializer = LocationUpdateSerializer(locations, data=to_be_updated, many=True)
        create_location_serializer = LocationCreateSerializer(data=to_be_created, many=True)

        # check if given data is valid
        floorplan_is_valid = floorplan_serializer.is_valid()
        update_location_is_valid = update_location_serializer.is_valid()
        create_location_is_valid = create_location_serializer.is_valid()

        # if valid, perform updates and creates
        if floorplan_is_valid and update_location_is_valid and create_location_is_valid:
            # update floorplan
            floorplan_serializer.update(floorplan, floorplan_serializer.validated_data)
            # update locations
            update_location_serializer.update(locations, update_location_serializer.validated_data)
            # create locations
            create_location_serializer.create(create_location_serializer.validated_data)
            # get updated data
            fp = FloorPlan.objects.get(pk=pk)
            fp_serializer = FloorPlanSerializer(floorplan)
            return Response(fp_serializer.data)

        errors = {
            'floorplan_errors': floorplan_serializer.errors,
            'update_location_errors': update_location_serializer.errors,
            'create_location_errors': create_location_serializer.errors
        }
        return Response(errors, status=status.HTTP_400_BAD_REQUEST)


class LocationsByFloorPlan(APIView):
    """
        name : 'api-locations'
        GET : List of Location instances related to FloorPlan
        POST : Create a list of Locations (must be in list)
        PUT : Update a list of Location instances
    """
    permission_classes = (IsAuthenticated,)

    def get_queryset(self, pk):
        floorplan = self.get_floorplan(pk)
        return Location.objects.filter(floorplan=floorplan, is_trashed=False)

    def get_floorplan(self, pk):
        try:
            return FloorPlan.objects.get(pk=pk)
        except FloorPlan.DoesNotExist:
            raise Http404

    def specify_floorplan_key(self, item, pk):
        item['floorplan'] = pk
        return item

    def get(self, request, pk, format=None):
        locations = self.get_queryset(pk)
        serializer = LocationUpdateSerializer(locations, many=True)
        return Response(serializer.data)

    def post(self, request, pk, format=None):
        print(request.data)
        data = [self.specify_floorplan_key(item, pk) for item in request.data]
        serializer = LocationCreateSerializer(data=data, many=True)
        if serializer.is_valid():
            serializer.create(serializer.validated_data)
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

    def put(self, request, pk, format=None):
        data = request.data
        locations = self.get_queryset(pk)
        serializer = LocationUpdateSerializer(locations, data=data, many=True)
        if serializer.is_valid():
            serializer.update(locations, serializer.validated_data)
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

