from django.db import models
from django.contrib.auth.models import User

# Create your models here.


LOCATION_TYPES = (
    ('DESK','Desk'),
    ('OFFICE','Office'),
    ('CONFR','Conference Room'),
    ('COMMON','Common Area'),
    ('RESTROOM','Restroom'),
    ('PUBLIC','Public Area'),
    ('PRIVATE','Private Area'),
    ('MISC','Miscellaneous'),
)


class FloorPlan(models.Model):
    name = models.CharField(max_length=100)
    user = models.ForeignKey(User, related_name='floorplans', on_delete=models.CASCADE)
    image = models.ImageField(upload_to='floorplans')
    width_offset = models.FloatField(default=1.0)
    height_offset = models.FloatField()
    created_at = models.DateTimeField(auto_now_add=True)
    last_updated = models.DateTimeField(auto_now=True)


class Location(models.Model):
    name = models.CharField(max_length=100)
    loc_type = models.CharField(max_length=50, choices=LOCATION_TYPES)
    details = models.CharField(max_length=250, blank=True)
    ext = models.IntegerField(blank=True, null=True)
    floorplan = models.ForeignKey(FloorPlan, related_name='locations', on_delete=models.CASCADE)
    position_x = models.FloatField()
    position_y = models.FloatField()
    created_at = models.DateTimeField(auto_now_add=True)
    last_updated = models.DateTimeField(auto_now=True)
