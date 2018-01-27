from django.urls import path
from django.contrib.auth.views import LoginView, LogoutView

from .views import index, dashboard, view_floorplan, edit_floorplan

urlpatterns = [
    path('', index, name='floorplans_index'),
    path('dashboard/', dashboard, name='floorplans_dashboard'),
    path('login/',
         LoginView.as_view(template_name='floorplans/login_form.html'),
         name='floorplans_login'),
    path('logout',
         LogoutView.as_view(),
         name='floorplans_logout'),
    path('floorplan/view/<int:floorplan_id>',
         view_floorplan,
         name='floorplans_view'),
    path('floorplan/edit/<int:floorplan_id>',
         edit_floorplan,
         name='floorplans_edit'),
]
