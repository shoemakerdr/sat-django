from rest_framework import serializers
from floorplans.models import User, FloorPlan, Location
from django.contrib.auth.models import User

class LocationSerializer(serializers.ModelSerializer):
    class Meta:
        model = Location
        fields = ('id', 'name', 'loc_type', 'details', 'extension', 'position_x', 'position_y', 'last_updated')


class FloorPlanSerializer(serializers.ModelSerializer):
    locations = LocationSerializer(many=True, read_only=True)

    class Meta:
        model = FloorPlan
        fields = ('id', 'user', 'name', 'image', 'width_ratio', 'height_ratio', 'locations', 'last_updated')


class UserSerializer(serializers.ModelSerializer):
    floorplans = FloorPlanSerializer(many=True, read_only=True)

    class Meta:
        model = User
        fields = ('id', 'username', 'floorplans')

