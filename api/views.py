from floorplans.models import FloorPlan, Location
from .serializers import FloorPlanSerializer, LocationSerializer, LocationsByFloorPlanSerializer
from rest_framework import generics, status
from rest_framework.views import APIView
from rest_framework.response import Response


class FloorPlanDetail(generics.RetrieveUpdateAPIView):
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer


class LocationsByFloorPlan(APIView):
    serializer_class = LocationsByFloorPlanSerializer

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

    def specify_trashed(self, item):
        item['is_trashed'] = True
        return item

    def get(self, request, pk, format=None):
        locations = self.get_queryset(pk)
        serializer = LocationsByFloorPlanSerializer(locations, many=True)
        return Response(serializer.data)

    def post(self, request, pk, format=None):
        data = [self.specify_floorplan_key(item, pk) for item in request.data]
        serializer = LocationSerializer(data=data, many=True)
        if serializer.is_valid():
            serializer.create(serializer.validated_data)
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

    def put(self, request, pk, format=None):
        # /api/floorplans/2/locations/?trash=true
        if request.query_params['trash']:
            data = [self.specify_trashed(item) for item in request.data]
        else:
            data = request.data
        locations = self.get_queryset(pk)
        serializer = LocationsByFloorPlanSerializer(locations, data=data, many=True)
        if serializer.is_valid():
            serializer.update(locations, serializer.validated_data)
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

