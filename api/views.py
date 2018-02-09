from floorplans.models import FloorPlan, Location
from .serializers import FloorPlanSerializer, LocationSerializer
from rest_framework import generics

class FloorPlanList(generics.ListCreateAPIView):
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer


class FloorPlanDetail(generics.RetrieveUpdateDestroyAPIView):
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer


class LocationList(generics.ListCreateAPIView):
    queryset = Location.objects.all()
    serializer_class = LocationSerializer


class LocationDetail(generics.RetrieveUpdateDestroyAPIView):
    queryset = Location.objects.all()
    serializer_class = LocationSerializer
