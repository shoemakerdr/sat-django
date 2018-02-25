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
    owner = models.ForeignKey(User, related_name='floorplans', on_delete=models.CASCADE)
    width = models.FloatField(default=1.0)
    height = models.FloatField(default=1.0)
    image = models.ImageField(upload_to='floorplans', width_field='width', height_field='height')
    is_trashed = models.BooleanField(default=False)
    is_public = models.BooleanField(default=False)
    created_at = models.DateTimeField(auto_now_add=True)
    last_updated = models.DateTimeField(auto_now=True)

    def __str__(self):
        return self.name

    def aspect_ratio(self):
        return self.height / self.width


class Location(models.Model):
    name = models.CharField(max_length=100)
    loc_type = models.CharField(max_length=50, choices=LOCATION_TYPES)
    details = models.CharField(max_length=250, blank=True)
    extension = models.IntegerField(blank=True, null=True)
    floorplan = models.ForeignKey(FloorPlan, related_name='locations', on_delete=models.CASCADE)
    position_x = models.FloatField()
    position_y = models.FloatField()
    is_trashed = models.BooleanField(default=False)
    created_at = models.DateTimeField(auto_now_add=True)
    last_updated = models.DateTimeField(auto_now=True)

    def __str__(self):
        return self.name
