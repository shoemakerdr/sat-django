from django.urls import path, re_path
from . import views

urlpatterns = [
    path('floorplans/', views.floorplan_list),
    re_path('floorplans/(?P<pk>[0-9]+)/$', views.floorplan_detail),
]
