# ------------------------------------------- #
# ------ PERFORM HYPERPARAMETER TUNING ------ #
# ------------------------------------------- #
# Hyperparamter tuning is performed on a subset of sites selected to represent the range of vegetation community types in the dataset

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
import time
import pandas
import numpy
import re
from collections import Counter
from osgeo import gdal, ogr, osr
from sklearn import metrics
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import GroupShuffleSplit
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from UAV_fxns import *
parsers = __import__("0_createParsers")

'''
usage: python 2_1_hyperparameterTuning_nestedCV_Classification.py site $SLURM_ARRAY_TASK_ID
positional arguments:
    :param site: site
    :param cv_split_num: cv_split_num
'''

def main(site, cv_split_num):

    """
    Main function
    :param site: site
    :param cv_split_num: cv_split_num
    """

    # Report site from bash script
    print('========== SITE: ', site, ' ==========')
    print('\n')

    # Report outer CV split from bash script
    print('========== OUTER CV SPLIT: ', cv_split_num, ' ==========')
    print('\n')

    # 1. ========== SET UP  ==========

    # 1.1 ---------- Directories ----------

    ###########################################################
    # SET OUTPUT DIRECTORY
    ###########################################################

    outDir = '*/UAV_classification/hyperparameter_tuning/'

    os.chdir(outDir) # Sets the working directory

    trainingDir = '*/0_UAV_final/data/'
    stackDir = '*/UAV_classification/stacks/'

    # 1.2 ---------- Parameters ----------

    trainingName = 'all_training_polys_final.shp' # Name of training data file, with file extension
    stack_suffix = '_RGBmsStack.tif' # Suffix for identifying the correct tifs within the stack directory, including file extension
    cvName = 'outerCVsplits_n10.csv' # Name of outer CV splits file, with file extension

    SHPdriver = ogr.GetDriverByName('ESRI Shapefile') # Specify shapefile driver

    gdal.UseExceptions() # Tell GDAL to throw Python exceptions

    splits_inner = 5 # Set number of inner cross validation splits

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
    print('The total number of training/testing polygons is: ' + str(polys.GetFeatureCount()))
    print('\n')

    # 2.2 ---------- Extract the current site from training points ----------

    polys.SetAttributeFilter(None) # Remove previous site code filter

    selectByAttribute(polys, 'site_code', site) # Select by attribute
    print('--- Training data subset to include only site ' + site + ' with ' + str(polys.GetFeatureCount()) + ' polygons ---')
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
        sitePolys = polysDataSource.GetLayer()
        print('--- Training data reprojection complete --')
        print('\n')
    else:
        sitePolys = polys
        print('Spatial references match, projection not needed')
        print('\n')

    # 2.4 ---------- Load outer CV splits ----------

    splitsPath = os.path.join(trainingDir, cvName)
    outer_splits = pandas.read_csv(splitsPath)

    # 2.5 ---------- Extract the current site from outer CV splits ----------

    outer_splits = outer_splits[outer_splits['site'] == site]
    print('The outer cross validation splits are:')
    print(outer_splits)
    print('\n')

    # 2.6 ---------- Get appropriate outer CV split ----------

    # Use job array number to select correct split
    split_polyID = outer_splits.iloc[[cv_split_num - 1]] # Pandas is zero indexed, job arrays are 1 indexed

    # 2.7 ---------- Get training and testing polygon IDs ----------

    # Separate indexes into training and testing
    train_polyID = split_polyID.loc[:, 'train'].values[0]
    test_polyID = split_polyID.loc[:, 'test'].values[0]

    # Parse pandas dataframe objects into lists of integers
    train_polyID = re.findall('[0-9]+', train_polyID)
    train_polyID = [int(i) for i in train_polyID]
    test_polyID = re.findall('[0-9]+', test_polyID)
    test_polyID = [int(i) for i in test_polyID]

    print('Train polygon IDs:')
    print(train_polyID)
    print('\n')

    print('Test polygon IDs:')
    print(test_polyID)
    print('\n')

    # 2.8 ---------- Convert training data to raster ----------

    sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

    trainPolyRasterPath = str(site + '_trainPolyRaster_n' + str(cv_split_num) + '.tif')
    testPolyRasterPath = str(site + '_testPolyRaster_n' + str(cv_split_num) + '.tif')
    polyIDRasterPath = str(site + '_polyIDRaster_n' + str(cv_split_num) + '.tif')

    # Training set
    selectByAttributeList(sitePolys, 'polyID', train_polyID)
    vectorToRaster(sitePolys, 'class_ID', stackPath, trainPolyRasterPath)
    sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

    # Testing set
    selectByAttributeList(sitePolys, 'polyID', test_polyID)
    vectorToRaster(sitePolys, 'class_ID', stackPath, testPolyRasterPath)
    sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

    # Polygon ID
    selectByAttributeList(sitePolys, 'polyID', train_polyID)
    vectorToRaster(sitePolys, 'polyID', stackPath, polyIDRasterPath)
    sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

    # 2.9 ---------- Process training/testing data rasters and convert to array ----------

    # Resample
    trainRaster = resampleRaster(trainPolyRasterPath, stackPath)
    testRaster = resampleRaster(testPolyRasterPath, stackPath)
    polyIDRaster = resampleRaster(polyIDRasterPath, stackPath)

    # Get intersection
    trainIntersection = calculateRasterIntersection(stackPath, trainRaster)
    print('The training minimum intersection is: ' + str(trainIntersection))
    print('\n')

    # Get intersection
    testIntersection = calculateRasterIntersection(stackPath, testRaster)
    print('The testing minimum intersection is: ' + str(testIntersection))
    print('\n')

    # Get intersection
    polyIDIntersection = calculateRasterIntersection(stackPath, polyIDRaster)
    print('The polygon ID minimum intersection is: ' + str(polyIDIntersection))
    print('\n')

    # Clip
    trainRaster = clipRasterWithCoords(trainIntersection, trainRaster)
    testRaster = clipRasterWithCoords(testIntersection, testRaster)
    polyIDRaster = clipRasterWithCoords(polyIDIntersection, polyIDRaster)

    # Convert to array
    trainRasterArray = trainRaster.ReadAsArray()
    trainRasterArray = trainRasterArray[numpy.newaxis, :, :] # Add third dimension
    print('--- Training data raster ingested and converted to array ---')
    print(trainRasterArray)
    print('Training data shape: ' + str(trainRasterArray.shape))
    print('\n')

    # Convert to array
    testRasterArray = testRaster.ReadAsArray()
    testRasterArray = testRasterArray[numpy.newaxis, :, :] # Add third dimension
    print('--- Testing data raster ingested and converted to array ---')
    print(testRasterArray)
    print('Testing data shape: ' + str(testRasterArray.shape))
    print('\n')

    # Convert to array
    polyIDRasterArray = polyIDRaster.ReadAsArray()
    polyIDRasterArray = polyIDRasterArray[numpy.newaxis, :, :] # Add third dimension
    print('--- Polygon ID data raster ingested and converted to array ---')
    print(polyIDRasterArray)
    print('Polygon ID data shape: ' + str(polyIDRasterArray.shape))
    print('\n')

    # 2.10 ---------- Convert multiband raster to numpy array ----------

    stackSrc = gdal.Open(stackPath)
    stackArray = stackSrc.ReadAsArray()
    print('--- Raster stack ingested and converted to array ---')
    print(stackArray)
    print('Raster stack shape: ' + str(stackArray.shape))
    print('\n')

    # 2.11 ---------- Gather predictor and response datasets ----------

    stackArray[numpy.isnan(stackArray)] = -10000 # Replace NaN, NaN values not allowed in sklearn functions

    train_X = numpy.transpose(stackArray[:, trainRasterArray[0] > 0]) # Select data only where we have training pixels, all bands
    train_y = numpy.transpose(trainRasterArray[:, trainRasterArray[0] > 0][0]) # Select only non zero training pixels

    train_ID = numpy.transpose(polyIDRasterArray[:, polyIDRasterArray[0] > 0][0]) # Select only non zero polygon ID pixels
    print('--- Training predictor and response datasets created ---')
    print('Training predictors shape: ' + str(train_X.shape))
    print('Training response shape: ' + str(train_y.shape))
    print('Polygon ID shape: ' + str(train_ID.shape))
    print('Original dataset shape %s' % Counter(train_y))
    print('\n')

    test_X = numpy.transpose(stackArray[:, testRasterArray[0] > 0]) # Select data only where we have training pixels, all bands
    test_y = numpy.transpose(testRasterArray[:, testRasterArray[0] > 0][0]) # Select only non zero training pixels
    print('--- Testing predictor and response datasets created ---')
    print('Testing predictors shape: ' + str(test_X.shape))
    print('Testing response shape: ' + str(test_y.shape))
    print('\n')  

    # Temporarily add polygon ID band to training predictor set for SMOTE oversampling
    train_ID = train_ID[:, numpy.newaxis] # Add second dimension
    train_X = numpy.append(train_X, train_ID, axis = 1)
    print('Training predictors + polygon ID temporary shape: ' + str(train_X.shape))
    print('\n')

    # 3. ========== MODELING ==========

    # Nested CV
    # https://machinelearningmastery.com/nested-cross-validation-for-machine-learning-with-python/
    # https://scikit-learn.org/stable/auto_examples/model_selection/plot_nested_cross_validation_iris.html#sphx-glr-auto-examples-model-selection-plot-nested-cross-validation-iris-py

    # 3.1 ---------- Balance training/testing data ----------

    # Oversample sparse clases to equalize imbalanced groups
    # https://beckernick.github.io/oversampling-modeling/
    smote = SMOTE(random_state = 1908, n_jobs = -1)
    train_X, train_y = smote.fit_resample(train_X, train_y)
    train_ID = train_X[:, -1] # Get resampled polygon ID data, last row
    train_X = train_X[:, :-1] # Get resampled training predictor data, all but last row
    print('--- Training data over sampled with SMOTE to equalize imbalanced groups ---')
    print('Training predictors shape: ' + str(train_X.shape))
    print('Training response shape: ' + str(train_y.shape))
    print('Polygon ID shape: ' + str(train_ID.shape))
    print('Resampled dataset shape %s' % Counter(train_y))
    print('\n')
    
    # 3.2 ---------- Set up reporting file  ----------

    # Open text file to write results to
    resultsPath = str(site + '_results' + '_' + str(cv_split_num) + '_SMOTE.txt')
    outResults = open(resultsPath, 'a')

    # Report plant functional type
    print('========== SITE: ', site, ' ==========')
    print('\n')
    print('========== SITE: ', site, ' ==========', file = outResults)
    print('\n', file = outResults)

    # Report OUTER split number
    print('---------- OUTER FOLD SPLIT ', str(cv_split_num), ' of ', str(outer_splits.shape[0]), ' ----------')
    print('\n')
    print('---------- OUTER FOLD SPLIT ', str(cv_split_num), ' of ', str(outer_splits.shape[0]), ' ----------', file = outResults)
    print('\n', file = outResults)

    # 3.3 ---------- Configure grid search parameters ----------

    model_params_grid = {
        'max_features': ['auto', 'log2'], # Number of predictors to use for each tree
        'min_samples_leaf': [1, 3, 5], # Minimum number of samples allowed in a leaf
        'max_samples': [0.8, 0.9, None], # Number of observations to use for each tree
    }
    
    # 3.4 ---------- Configure INNER splitting: performs hyperparameter tuning within each outer split ----------

    # Create grouped 5-fold cross-validation splits
    cv_inner = GroupShuffleSplit(test_size = .20, n_splits = splits_inner, random_state = 1908)
    print('--- INNER K-fold splits created ---')
    print('\n')

    # 3.5 ---------- Define the model ----------

    model = RandomForestClassifier(n_estimators = 500, bootstrap = True, oob_score = True, n_jobs = -1, random_state = 1908)
    print('--- Random Forest base model initialized ---')
    print('\n')

    # 3.6 ---------- Define the random search cross-validation (INNER cross-validation) ----------

    # Use classification accuracy for scoring
    # Refit best model on full dataset
    search = GridSearchCV(estimator = model, param_grid = model_params_grid, scoring = 'accuracy', refit = True, cv = cv_inner, verbose = 2, return_train_score = False, n_jobs = -1)
    print('--- Grid cross-validation hyperparameter search initialized ---')
    print('\n')

    # Execute random search cross-validation
    start = time.time()
    result = search.fit(train_X, train_y, groups = train_ID)
    print('--- Cross-validation Random Forest models fit ---')
    end = time.time()
    print('Time to finish: %s seconds' % (end - start))
    print('\n')

    # 3.7 ---------- Report parameter comparison ----------

    # Report best parameters
    print('Best parameters: ' + str(result.best_params_), file = outResults)
    print('\n', file = outResults)

    # Report all paramaters
    print('All parameters results, inner CV, non-nested:', file = outResults)
    results_df = pandas.DataFrame(result.cv_results_)
    results_df = results_df.reindex(columns = ['params', 'mean_train_score', 'mean_test_score', 'rank_test_score']) # Grab relevant columns
    print(results_df.to_string(), file = outResults)
    print('\n', file = outResults)

    print('--- Parameter comparison reported ---')
    print('\n')

    # 3.8 ---------- Report results ----------

    # Get the best performing model fit on the whole OUTER fold training set
    # GridSearchCV refits on full dataset, which in this case is the full training data set from this iteration of the OUTER cross-validation loop
    best_model = result.best_estimator_

    # Evaluate model on the OUTER fold hold out dataset
    yhat = best_model.predict(test_X)

    # Evaluate the model
    accuracy = metrics.accuracy_score(test_y, yhat)

    # Report progress
    print('Current OUTER split test set accuracy: %.3f' % (accuracy), file = outResults)
    print('\n', file = outResults)

    print('--- Results reported ---')
    print('\n')

    # 3.9 ---------- Clean up ----------

    os.remove(os.path.join(outDir, trainPolyRasterPath))
    os.remove(os.path.join(outDir, testPolyRasterPath))

if __name__ == '__main__':

    args = parsers.runCV_ClassificationParser().parser.parse_args()

    main(args.site, args.cv_split_num)