from floorplans.models import FloorPlan
from .serializers import FloorPlanSerializer
from rest_framework import mixins
from rest_framework import generics

class FloorPlanList(generics.ListCreateAPIView):
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer


class FloorPlanDetail(generics.RetrieveUpdateDestroyAPIView):
    queryset = FloorPlan.objects.all()
    serializer_class = FloorPlanSerializer
