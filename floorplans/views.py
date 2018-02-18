from django.shortcuts import render, redirect
from django.http import HttpResponse
from django.contrib.auth.decorators import login_required
import json

from floorplans.models import FloorPlan, Location
from api.serializers import FloorPlanSerializer


def index(request):
    return render(request, 'floorplans/index.html')


@login_required
def dashboard(request):
    floorplans = FloorPlan.objects.filter(owner=request.user)
    context = {
        'floorplans': floorplans
    }
    return render(request,
                  'floorplans/dashboard.html',
                  context)


# TODO: permissions -> Should not be able to see this unless owner or floorplan is public
def view_floorplan(request, floorplan_id):
    floorplan = FloorPlan.objects.get(pk=floorplan_id)
    serializer = FloorPlanSerializer(floorplan)
    data = json.dumps(serializer.data)
    return render(request,
                  'floorplans/view_floorplan.html'
                  , { 'floorplan' : data })


@login_required
def edit_floorplan(request, floorplan_id):
    return render(request, 'floorplans/edit_floorplan.html')
