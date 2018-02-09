from django.urls import path, re_path
from . import views

urlpatterns = [
    path('floorplans/', views.FloorPlanList.as_view()),
    re_path('floorplans/(?P<pk>[0-9]+)/$', views.FloorPlanDetail.as_view()),
    path('locations/', views.LocationList.as_view()),
    re_path('locations/(?P<pk>[0-9]+)/$', views.LocationDetail.as_view()),
]
