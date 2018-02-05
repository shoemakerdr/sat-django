from rest_framework import serializers
from floorplans.models import User, FloorPlan, Location, LOCATION_TYPES

class FloorPlanSerializer(serializers.Serializer):
    id = serializers.IntegerField(read_only=True)
    name = serializers.CharField(required=True, allow_blank=False, max_length=100)
    image = serializers.ImageField(max_length=None, allow_empty_file=False)
    width_ratio = serializers.FloatField()
    height_ratio = serializers.FloatField()
    last_updated = serializers.DateTimeField()

    def create(self, validated_data):
        return FloorPlan.objects.create(**validated_data)


    def update(self, instance, validated_data):
        instance.name = validated_data.get('name', instance.name)
        instance.image = validated_data.get('image', instance.image)
        instance.width_ratio = validated_data.get('width_ratio', instance.width_ratio)
        instance.height_ratio = validated_data.get('height_ratio', instance.height_ratio)
        instance.save()
        return instance

class LocationSerializer(serializers.Serializer):
    id = serializers.IntegerField(read_only=True)
    name = serializers.CharField(required=True, allow_blank=False, max_length=100)
    loc_type = serializers.ChoiceField(choices=LOCATION_TYPES)
    details = serializers.CharField(required=False, allow_blank=True, max_length=250)
    extension = serializers.IntegerField()
    position_x = serializers.FloatField(required=True)
    position_y = serializers.FloatField(required=True)
    last_updated = serializers.DateTimeField()


    def create(self, validated_data):
        return Location.objects.create(**validated_data)


    def update(self, instance, validated_data):
        instance.name = validated_data.get('name', instance.name)
        instance.loc_type = validated_data.get('loc_type', instance.loc_type)
        instance.details = validated_data.get('details', instance.details)
        instance.extension = validated_data.get('extension', instance.extension)
        instance.position_x = validated_data.get('position_x', instance.position_x)
        instance.position_y = validated_data.get('position_y', instance.position_y)
        instance.save()
        return instance
