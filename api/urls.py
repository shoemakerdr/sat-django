from django.urls import path, re_path
from . import views

urlpatterns = [
    re_path('floorplans/(?P<pk>[0-9]+)/$', views.FloorPlanDetail.as_view()),
    re_path('floorplans/(?P<pk>[0-9]+)/locations/$', views.LocationsByFloorPlan.as_view()),
]
