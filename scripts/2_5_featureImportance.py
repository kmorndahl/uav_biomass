import os
import sys
import pandas
import numpy
from osgeo import gdal, ogr, osr
from sklearn.utils import resample
from imblearn.over_sampling import SMOTE
import shap
from UAV_fxns import *
import joblib
from collections import Counter
parsers = __import__("0_createParsers")

'''
usage: python 2_5_featureImportance.py site
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

    trainingDir = '*/0_UAV_final/data/'
    stackDir = '*/UAV_classification/stacks/'
    modelDir = '*/UAV_classification/results/'

    stack_suffix = '_RGBmsStack.tif' # Suffix for identifying the correct tifs within the stack directory, including file extension
    model_suffix = '_modelRF_SMOTE.pkl' # Suffix for identifying the correct model pkl file, with file extension
    trainingName = 'all_training_polys.shp' # Name of training data file, with file extension

    # 2. ========== DATA PREPARATION ==========

    # 2.1 ---------- Load data ----------

    # Set up stack path
    stackPath = os.path.join(stackDir, site + stack_suffix)

    # Import raster stack
    stackSrc = gdal.Open(stackPath)
    stackArray = stackSrc.ReadAsArray()
    print('--- Raster stack ingested and converted to array ---')
    print(stackArray)
    print('Raster stack shape: ' + str(stackArray.shape))
    print('\n')

    # Set up model path
    modelPath = os.path.join(modelDir, site + model_suffix)

    # Import Random Forest model
    model = joblib.load(modelPath)
    print('--- Random Forest model imported ---')
    print('\n')

    # Import training data
    polysPath = os.path.join(trainingDir, trainingName)
    polysDataSource = ogr.Open(polysPath, 1)
    polys = polysDataSource.GetLayer()

    # 2.2 ---------- Prepare training polygon data ----------

    # Select current site
    selectByAttribute(polys, 'site_code', site)
    print('--- Training data subset to include only site ' + site + ' ---')
    print('\n')

    # 2.3 ---------- Project training data shapefile to match spatial reference of UAV data ----------

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

    # 2.4 ---------- Convert training data to raster ----------

    polyRasterPath = str(site + '_polyRaster.tif')
    vectorToRaster(sitePolys, 'class_ID', stackPath, polyRasterPath)

    # 2.5 ---------- Process training data raster and convert to array ----------

    # Resample
    polyRaster = resampleRaster(polyRasterPath, stackPath)

    # Calculate intersection
    intersection = calculateRasterIntersection(stackPath, polyRaster)
    print('The minimum intersection is: ' + str(intersection))
    print('\n')

    # Clip
    polyRaster = clipRasterWithCoords(intersection, polyRaster)

    # Convert to array
    polyRasterArray = polyRaster.ReadAsArray()
    polyRasterArray = polyRasterArray[numpy.newaxis, :, :] # Add third dimension
    print('--- Training data raster ingested and converted to array ---')
    print(polyRasterArray)
    print('Training data shape: ' + str(polyRasterArray.shape))
    print('\n')

    # 2.6 ---------- Gather predictor and response datasets ----------

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

    # 2.7 ---------- Balance training/testing data ----------

    # Oversample sparse clases to equalize imbalanced groups
    # https://beckernick.github.io/oversampling-modeling/
    smote = SMOTE(random_state = 1908)
    X, y = smote.fit_resample(X, y)
    print('--- Data over sampled with SMOTE to equalize imbalanced groups ---')
    print('Predictor data shape: ' + str(X.shape))
    print('Response data shape: ' + str(y.shape))
    print('Resampled dataset shape %s' % Counter(y))
    print('\n')
    
    # 2.8 ---------- Create background dataset ----------

    background_data = resample(X, n_samples = 500, replace = False, random_state = 1908)

    # 3. ========== FEATURE IMPORTANCE ==========

    # 3.1 ---------- Calculate shapely values ----------

    # Calculate SHAP predictor importance -- interventional
    explainer = shap.TreeExplainer(model, data = background_data, feature_perturbation = 'interventional') 
    shap_values = explainer.shap_values(X, approximate = True)
    print('--- Shapely values calculated ---')
    print('\n')

    # 3.2 ---------- Format shapely values ----------

    # Initialize dataframe to track predictor importance
    predictor_importance_all = pandas.DataFrame()

    # Define predictor names
    predictor_names = ['Red1cm', 'Green1cm', 'Blue1cm', 'NDVI', 'Blue', 'Green', 'NIR', 'Red Edge', 'Red', 'CHM', 'Texture']

    # Get class names (PFTs)
    prediction_classes = model.classes_

    # Format SHAP values for export
    for x in range(len(shap_values)):

        # Extract appropriate data
        prediciton_class = prediction_classes[x] # Get current prediction class (PFT)
        importance_values = abs(shap_values[x]).mean(0) # Get current class shap values, take the absolute value, then average to get one shap value per predictor

        # Aggregate predictor importances with predictor names
        predictor_importance = pandas.DataFrame(list(zip(predictor_names, importance_values)), columns=['predictor_name', 'predictor_importance_value'])

        # Tidy
        predictor_importance.sort_values(by=['predictor_importance_value'], ascending=False, inplace=True) # Sort
        predictor_importance = predictor_importance.assign(site = site) # Add site column
        predictor_importance = predictor_importance.assign(prediciton_class = prediciton_class) # Add class column
        predictor_importance = predictor_importance[['site', 'prediciton_class', 'predictor_name', 'predictor_importance_value']] # Reorder columns

        # Append to main dataframe
        predictor_importance_all = predictor_importance_all.append(predictor_importance)

    # 3.3 ---------- Save shapely values ----------

    # Save predictor importance values to csv
    predictor_importance_all.to_csv(os.path.join(outDir, site + '_predictorImportance_SMOTE.csv'))

    # 3.4 ---------- Clean up ----------

    SHPdriver.DeleteDataSource(os.path.join(outDir, projPath))

if __name__ == '__main__':

    args = parsers.featureImportance_ClassificationParser().parser.parse_args()

    main(args.site)