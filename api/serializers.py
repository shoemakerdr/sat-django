from rest_framework import serializers
from floorplans.models import User, FloorPlan, Location, LOCATION_TYPES

class FloorPlanSerializer(serializers.ModelSerializer):
    class Meta:
        model = FloorPlan
        fields = ('id', 'name', 'image', 'width_ratio', 'height_ratio', 'last_updated')


class LocationSerializer(serializers.ModelSerializer):
    class Meta:
        model = FloorPlan
        fields = ('id', 'name', 'loc_type', 'details', 'extension', 'position_x', 'position_y', 'last_updated')

