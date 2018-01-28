from django.shortcuts import render, redirect
from django.http import HttpResponse
from django.contrib.auth.decorators import login_required

from floorplans.models import FloorPlan, Location


def index(request):
    return render(request, 'floorplans/index.html')


@login_required
def dashboard(request):
    floorplans = FloorPlan.objects.filter(user=request.user)
    context = {
        'floorplans': floorplans
    }
    return render(request,
                  'floorplans/dashboard.html',
                  context)


def view_floorplan(request, floorplan_id):
    return render(request, 'floorplans/view_floorplan.html')


@login_required
def edit_floorplan(request, floorplan_id):
    return render(request, 'floorplans/edit_floorplan.html')
