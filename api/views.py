from django.http import HttpResponse, JsonResponse
from django.views.decorators.csrf import csrf_exempt
from rest_framework.renderers import JSONRenderer
from rest_framework.parsers import JSONParser
from floorplans.models import FloorPlan
from .serializers import FloorPlanSerializer

@csrf_exempt
def floorplan_list(request):
    """
    List all code floorplans, or create a new floorplan.
    """
    if request.method == 'GET':
        floorplans = FloorPlan.objects.all()
        serializer = FloorPlanSerializer(floorplans, many=True)
        return JsonResponse(serializer.data, safe=False)

    elif request.method == 'POST':
        data = JSONParser().parse(request)
        serializer = FloorPlanSerializer(data=data)
        if serializer.is_valid():
            serializer.save()
            return JsonResponse(serializer.data, status=201)
        return JsonResponse(serializer.errors, status=400)

@csrf_exempt
def floorplan_detail(request, pk):
    """
    Retrieve, update or delete a code floorplan.
    """
    try:
        floorplan = FloorPlan.objects.get(pk=pk)
    except FloorPlan.DoesNotExist:
        return HttpResponse(status=404)

    if request.method == 'GET':
        serializer = FloorPlanSerializer(floorplan)
        return JsonResponse(serializer.data)

    elif request.method == 'PUT':
        data = JSONParser().parse(request)
        serializer = FloorPlanSerializer(floorplan, data=data)
        if serializer.is_valid():
            serializer.save()
            return JsonResponse(serializer.data)
        return JsonResponse(serializer.errors, status=400)

    elif request.method == 'DELETE':
        floorplan.delete()
        return HttpResponse(status=204)
