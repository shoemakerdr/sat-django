from django.urls import path, re_path
from . import views

urlpatterns = [
    ####
    # floorplans/<pk>/ ->
    #   get FloorPlan instance and list of its Locations
    ####
    re_path('floorplans/(?P<pk>[0-9]+)/$',
            views.FloorPlanDetail.as_view(),
            name='api-floorplan'),
    ####
    # floorplans/<pk>/locations/ ->
    #   get, create, update list of Locations for FloorPlan instance
    ####
    re_path('floorplans/(?P<pk>[0-9]+)/locations/$',
            views.LocationsByFloorPlan.as_view(),
            name='api-locations'),
]
