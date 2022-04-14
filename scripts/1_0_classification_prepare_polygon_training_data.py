# ------------------------------------------- #
# ------ PREPARE POLYGON TRAINING DATA ------ #
# ------------------------------------------- #
# Creates field to uniquely identify polygons
# Converts PFT names to categorical integer field

import os
import sys
from osgeo import gdal, ogr
from UAV_fxns import *

# 1. ========== SET PARAMETERS ==========

###########################################################
# SET OUTPUT DIRECTORY
###########################################################

outDir = '*/0_UAV_final/data/'

os.chdir(outDir) # Sets the working directory

trainingDir = '*/0_UAV_final/data/'
trainingName = 'all_training_polys_final.shp' # Name of training data file, with file extension

SHPdriver = ogr.GetDriverByName('ESRI Shapefile') # Specify shapefile driver

gdal.UseExceptions() # Tell GDAL to throw Python exceptions

classDictionary = {'BRYOPHYTES': 1, # Dictionary for recoding categorical class field to integer
                   'DECIDUOUS SHRUBS':2,
                   'EVERGREEN SHRUBS':3,
                   'FORBS':4,
                   'GRAMINOIDS':5,
                   'LICHENS':6,
                   'NON VEGETATED':7,
                   'SHADOW':8,
                   'TREES':9}

# 2. ========== DATA PREPARATION ==========

# 2.1 ---------- Load training points ----------

# Open training data
polysPath = os.path.join(trainingDir, trainingName)
polysDataSource = ogr.Open(polysPath, 1)
polys = polysDataSource.GetLayer()
print('The total number of training/testing polygons is: ' + str(polys.GetFeatureCount()))
print('\n')

# 2.2 ---------- Create derivative fields: only need to do once ----------

# Create ID field
polyID = ogr.FieldDefn('polyID', ogr.OFTInteger)
polys.CreateField(polyID)

for feat in polys:
    id = int(feat.GetFID())
    print('The polygon ID is: ' + str(id))
    feat.SetField('polyID', id)
    polys.SetFeature(feat) 

# Convert categorical class field to integer
categoricalFieldToInteger(polys, classDictionary, 'class', 'class_ID')