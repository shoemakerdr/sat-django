from django.shortcuts import render, redirect
from django.http import HttpResponse
from django.contrib.auth.decorators import login_required



def index(request):
    return render(request, 'floorplans/index.html')


@login_required
def dashboard(request):
    return render(request, 'floorplans/dashboard.html')


def view_floorplan(request, floorplan_id):
    return render(request, 'floorplans/view_floorplan.html')


@login_required
def edit_floorplan(request, floorplan_id):
    return render(request, 'floorplans/edit_floorplan.html')
