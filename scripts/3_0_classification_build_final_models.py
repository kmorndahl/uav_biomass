# ------------------------------------------------------------- #
# ------ RANDOM FOREST CLASSIFICATION BUILD FINAL MODELS ------ #
# ------------------------------------------------------------- #

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
import numpy
from osgeo import gdal, ogr, osr
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from UAV_fxns import *
import joblib
from collections import Counter
parsers = __import__("0_createParsers")

'''
usage: python 2_4_buildFinalRFmodels_Classification.py site
positional arguments:
    :param site: site
'''

def main(site):

    """
    Main function
    :param site: site
    """
    
    # Report site from bash script
    print('========== SITE: ', site, ' ==========')
    print('\n')

    # 1. ========== PARAMETERS ==========

    ###########################################################
    # SET OUTPUT DIRECTORY
    ###########################################################

    outDir = '*/UAV_classification/results/'

    os.chdir(outDir) # Sets the working directory

    SHPdriver = ogr.GetDriverByName('ESRI Shapefile') # Specify shapefile driver

    gdal.UseExceptions() # Tell GDAL to throw Python exceptions

    trainingDir = '*/0_UAV_final/data/'
    stackDir = '*/UAV_classification/stacks/'

    trainingName = 'all_training_polys_final.shp' # Name of training data file, with file extension
    stack_suffix = '_RGBmsStack.tif' # Suffix for identifying the correct tifs within the stack directory, including file extension

    # 2. ========== DATA PREPARATION ==========

    # 2.1 ---------- Load data ----------

    # Set up stack path
    stackPath = os.path.join(stackDir, site + stack_suffix)
    print('The current site is: ' + site)
    print('\n')

    # Open training data
    polysPath = os.path.join(trainingDir, trainingName)
    polysDataSource = ogr.Open(polysPath, 1)
    polys = polysDataSource.GetLayer()

    # 3. ========== RUN RANDOM FOREST ==========

    # 3.1 ---------- Extract a single site from training points ----------

    selectByAttribute(polys, 'site_code', site)
    print('--- Training data subset to include only site ' + site + ' ---')
    print('\n')

    # 3.2 ---------- Project training data shapefile to match spatial reference of UAV data ----------

    projPath = str(site + '_proj.shp')

    # Get the spatial reference from the UAV data
    uav = gdal.Open(stackPath)
    spatialRefUAV = osr.SpatialReference()
    spatialRefUAV.ImportFromWkt(uav.GetProjectionRef())
    print('The UAV spatial reference is: ' + str(spatialRefUAV.GetAttrValue('projcs')))
    print('\n')

    # Get the spatial reference from the training points
    spatialRefSHP = polys.GetSpatialRef()
    print('The training points spatial reference is: ' + str(spatialRefSHP.GetAttrValue('projcs')))
    print('\n')

    # Project if needed
    if(str(spatialRefUAV.GetAttrValue('projcs')) != str(spatialRefUAV.GetAttrValue('projcs'))):
        print('Spatial references do not match, proceeding with projection...')
        projectShapefile(polys, projPath, spatialRefSHP, spatialRefUAV)
        sitePolysDataSource = ogr.Open(projPath, 1)
        sitePolys = sitePolysDataSource.GetLayer()
        print('--- Training data reprojection complete --')
        print('\n')
    else:
        sitePolys = polys
        print('Spatial references match, projection not needed')
        print('\n')

    # 3.3 ---------- Convert training data to raster ----------

    polyRasterPath = str(site + '_polyRaster.tif')
    vectorToRaster(sitePolys, 'class_ID', stackPath, polyRasterPath)

    # 3.4 ---------- Process training data raster and convert to array ----------

    polyRaster = resampleRaster(polyRasterPath, stackPath)

    intersection = calculateRasterIntersection(stackPath, polyRaster)
    print('The minimum intersection is: ' + str(intersection))
    print('\n')

    polyRaster = clipRasterWithCoords(intersection, polyRaster)

    polyRasterArray = polyRaster.ReadAsArray()
    polyRasterArray = polyRasterArray[numpy.newaxis, :, :] # Add third dimension
    print('--- Training data raster ingested and converted to array ---')
    print(polyRasterArray)
    print('Training data shape: ' + str(polyRasterArray.shape))
    print('\n')

    # 3.5 ---------- Convert multiband raster to numpy array ----------

    stackSrc = gdal.Open(stackPath)
    stackArray = stackSrc.ReadAsArray()
    print('--- Raster stack ingested and converted to array ---')
    print(stackArray)
    print('Raster stack shape: ' + str(stackArray.shape))
    print('\n')

    # 3.6 ---------- Gather predictor and response datasets ----------

    stackArray[numpy.isnan(stackArray)] = -10000 # Replace NaN, NaN values not allowed in sklearn functions

    X = stackArray[:, polyRasterArray[0] > 0] # Select data only where we have training pixels, all bands -- also reshapes to 2 dimensional
    y = polyRasterArray[:, polyRasterArray[0] > 0][0] # Select only non zero training pixels -- also reshapes to 2 dimensional

    X = numpy.transpose(X)
    y = numpy.transpose(y)

    print('--- Predictor and response datasets created ---')
    print('Predictor data shape: ' + str(X.shape))
    print('Response data shape: ' + str(y.shape))
    print('Original dataset shape %s' % Counter(y))
    print('\n')

    # 3.7 ---------- Balance training/testing data ----------

    # Oversample sparse clases to equalize imbalanced groups
    # https://beckernick.github.io/oversampling-modeling/
    smote = SMOTE(random_state = 1908)
    X, y = smote.fit_resample(X, y)
    print('--- Data over sampled with SMOTE to equalize imbalanced groups ---')
    print('Predictor data shape: ' + str(X.shape))
    print('Response data shape: ' + str(y.shape))
    print('Resampled dataset shape %s' % Counter(y))
    print('\n')
    
    # 3.8 ---------- Train the Random Forest model ----------

    # Initialize our model with 500 trees
    rf = RandomForestClassifier(n_estimators = 500, min_samples_leaf = 1, max_features = 'auto', bootstrap = True, oob_score = True, n_jobs = -1, random_state = 1908, max_samples = None)
    print('--- Random Forest model initialized ---')
    print('\n')

    # Fit our model to training data
    rf = rf.fit(X, y)
    print('--- Random Forest model fit ---')
    print('\n')

    # Save the model
    modelPath = str(site + '_modelRF_SMOTE.pkl')
    joblib.dump(rf, open(modelPath, 'wb'))

    # 3.9 ---------- Clean up ----------

    SHPdriver.DeleteDataSource(os.path.join(outDir, projPath))

if __name__ == '__main__':

    args = parsers.buildRF_ClassificationParser().parser.parse_args()

    main(args.site)