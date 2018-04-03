from django import forms
from django.forms import modelformset_factory

from .models import FloorPlan



class BaseFloorPlanFormSet(forms.BaseModelFormSet):
    def __init__(self, *args, **kwargs):
        super(BaseFloorPlanFormSet, self).__init__(*args, **kwargs)
        self.queryset = FloorPlan.objects.all()


FloorPlanFormSet = modelformset_factory(FloorPlan, fields=('name', 'image', 'is_public'), formset=BaseFloorPlanFormSet)
