from rest_framework import serializers
from floorplans.models import User, FloorPlan, Location


class LocationsByFloorPlanListSerializer(serializers.ListSerializer):
    def update(self, instances, validated_data):
        # Maps for id->location instance and id->data item.
        location_mapping = {location.id: location for location in instances}
        data_mapping = {item['id']: item for item in validated_data}

        ret = []
        for location_id, data in data_mapping.items():
            location = location_mapping.get(location_id, None)
            if location is not None:
                ret.append(self.child.update(location, data))

        return ret


class LocationsByFloorPlanSerializer(serializers.ModelSerializer):
    id = serializers.IntegerField()

    class Meta:
        model = Location
        list_serializer_class = LocationsByFloorPlanListSerializer
        fields = (
            'id',
            'name',
            'floorplan',
            'loc_type',
            'details',
            'extension',
            'position_x',
            'position_y',
            'is_trashed',
            'last_updated'
        )

class LocationListSerializer(serializers.ListSerializer):
    def create(self, validated_data):
        ret = []
        for location in validated_data:
            ret.append(self.child.create(location))

        return ret


class LocationSerializer(serializers.ModelSerializer):
    class Meta:
        model = Location
        list_serializer_class = LocationListSerializer
        fields = (
            'id',
            'name',
            'floorplan',
            'loc_type',
            'details',
            'extension',
            'position_x',
            'position_y',
            'is_trashed',
            'last_updated'
        )


class FloorPlanSerializer(serializers.ModelSerializer):
    locations = LocationSerializer(many=True, read_only=True)
    image = serializers.ImageField(required=False)

    class Meta:
        model = FloorPlan
        fields = (
            'id', 
            'owner', 
            'name', 
            'image', 
            'aspect_ratio', 
            'locations', 
            'is_trashed',
            'is_public',
            'last_updated'
        )

