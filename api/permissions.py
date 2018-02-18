from rest_framework import permissions


class IsOwnerOrFloorPlanIsPublic(permissions.BasePermission):
    def has_object_permission(self, request, view, obj):
        return obj.is_public or obj.owner == request.user
