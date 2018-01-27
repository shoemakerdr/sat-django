from django.urls import path

from . import views

urlpatterns = [
    #  path('', views.index, name='floorplans_dashboard'),
    path('dashboard/', views.index, name='floorplans_dashboard'),
]
