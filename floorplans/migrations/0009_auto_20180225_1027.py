# Generated by Django 2.0.1 on 2018-02-25 10:27

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('floorplans', '0008_auto_20180225_1012'),
    ]

    operations = [
        migrations.AlterField(
            model_name='floorplan',
            name='image',
            field=models.ImageField(height_field='height', upload_to='floorplans', width_field='width'),
        ),
    ]
