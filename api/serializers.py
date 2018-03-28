from rest_framework import serializers
from floorplans.models import User, FloorPlan, Location


class LocationUpdateListSerializer(serializers.ListSerializer):
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


class LocationUpdateSerializer(serializers.ModelSerializer):
    id = serializers.IntegerField()

    class Meta:
        model = Location
        list_serializer_class = LocationUpdateListSerializer
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

class LocationCreateListSerializer(serializers.ListSerializer):
    def create(self, validated_data):
        ret = []
        for location in validated_data:
            ret.append(self.child.create(location))

        return ret


class LocationCreateSerializer(serializers.ModelSerializer):
    class Meta:
        model = Location
        list_serializer_class = LocationCreateListSerializer
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
    locations = LocationCreateSerializer(many=True, read_only=True)
    image = serializers.ImageField(read_only=True, required=False)
    aspect_ratio = serializers.SerializerMethodField()
    owner_name = serializers.SerializerMethodField()

    def get_aspect_ratio(self, obj):
        fp = FloorPlan.objects.get(pk=obj.id)
        return fp.aspect_ratio()

    def get_owner_name(self, obj):
        return User.objects.get(pk=obj.owner.id).username

    class Meta:
        model = FloorPlan
        fields = (
            'id', 
            'owner', 
            'owner_name',
            'name', 
            'image', 
            'aspect_ratio', 
            'locations', 
            'is_trashed',
            'is_public',
            'last_updated'
        )

