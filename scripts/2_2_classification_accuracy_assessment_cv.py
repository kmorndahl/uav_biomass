# ------------------------------------------------------------------------- #
# ------ FIT RANDOM FOREST CLASSIFICATION MODELS AND REPORT ACCURACY ------ #
# ------------------------------------------------------------------------- #

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
import pandas
import numpy
import re
from osgeo import gdal, ogr, osr
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
from imblearn.over_sampling import SMOTE
from collections import Counter
from UAV_fxns import *
parsers = __import__("0_createParsers")

'''
usage: python 2_2_accuracyAssessment_CV_Classification.py site $SLURM_ARRAY_TASK_ID
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

    outDir = '*/UAV_classification/results/'

    os.chdir(outDir) # Sets the working directory

    trainingDir = '*/0_UAV_final/data/'
    stackDir = '*/UAV_classification/stacks/'

    # 1.2 ---------- Parameters ----------

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

    trainingName = 'all_training_polys_final.shp' # Name of training data file, with file extension
    stack_suffix = '_RGBmsStack.tif' # Suffix for identifying the correct tifs within the stack directory, including file extension
    cvName = 'outerCVsplits_n10.csv' # Name of outer CV splits file, with file extension

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

    selectByAttribute(polys, 'site_code', site)
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
        sitePolys = sitePolysDataSource.GetLayer()
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

    # 2.6 ---------- Set up reporting file ----------

    overall_accuracy = pandas.DataFrame(columns = ['site', 'split_num', 'accuracy', 'balanced_accuracy'])
    pft_accuracy = pandas.DataFrame(columns = ['site', 'split_num', 'pft', 'precision', 'recall', 'f1-score', 'support'])

    for i in range(cv_split_num):

        # 2.7 ---------- Get appropriate outer CV split ----------

        # Use job array number to select correct split
        split_polyID = outer_splits.iloc[[i]]

        # Assign split number
        current_split_num = i + 1

        print('========== OUTER FOLD SPLIT ', str(current_split_num), ' of ', str(outer_splits.shape[0]), ' ==========')
        print('\n')

        # 2.8 ---------- Get training and testing polygon IDs ----------

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

        # 2.9 ---------- Convert training data to raster ----------

        sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

        trainPolyRasterPath = str(site + '_trainPolyRaster_n' + str(current_split_num) + '.tif')
        testPolyRasterPath = str(site + '_testPolyRaster_n' + str(current_split_num) + '.tif')

        # Training set
        selectByAttributeList(sitePolys, 'polyID', train_polyID)
        vectorToRaster(sitePolys, 'class_ID', stackPath, trainPolyRasterPath)
        sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

        # Testing set
        selectByAttributeList(sitePolys, 'polyID', test_polyID)
        vectorToRaster(sitePolys, 'class_ID', stackPath, testPolyRasterPath)
        sitePolys.SetAttributeFilter(None) # Ensure attribute filter is clear

        # 2.10 ---------- Process training/testing data rasters and convert to array ----------

        # Resample
        trainRaster = resampleRaster(trainPolyRasterPath, stackPath)
        testRaster = resampleRaster(testPolyRasterPath, stackPath)

        # Calculate intersection
        trainIntersection = calculateRasterIntersection(stackPath, trainRaster)
        print('The training minimum intersection is: ' + str(trainIntersection))
        print('\n')

        # Calculate intersection
        testIntersection = calculateRasterIntersection(stackPath, testRaster)
        print('The testing minimum intersection is: ' + str(testIntersection))
        print('\n')

        # Clip
        trainRaster = clipRasterWithCoords(trainIntersection, trainRaster)
        testRaster = clipRasterWithCoords(testIntersection, testRaster)

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

        # 2.11 ---------- Convert multiband raster to numpy array ----------

        stackSrc = gdal.Open(stackPath)
        stackArray = stackSrc.ReadAsArray()
        print('--- Raster stack ingested and converted to array ---')
        print(stackArray)
        print('Raster stack shape: ' + str(stackArray.shape))
        print('\n')

        # 2.12 ---------- Gather predictor and response datasets ----------

        stackArray[numpy.isnan(stackArray)] = -10000 # Replace NaN, NaN values not allowed in sklearn functions

        train_X = numpy.transpose(stackArray[:, trainRasterArray[0] > 0]) # Select data only where we have training pixels, all bands
        train_y = numpy.transpose(trainRasterArray[:, trainRasterArray[0] > 0][0]) # Select only non zero training pixels
        print('--- Training predictor and response datasets created ---')
        print('Training predictors shape: ' + str(train_X.shape))
        print('Training response shape: ' + str(train_y.shape))
        print('Original dataset shape %s' % Counter(train_y))
        print('\n')

        test_X = numpy.transpose(stackArray[:, testRasterArray[0] > 0]) # Select data only where we have training pixels, all bands
        test_y = numpy.transpose(testRasterArray[:, testRasterArray[0] > 0][0]) # Select only non zero training pixels
        print('--- Testing predictor and response datasets created ---')
        print('Testing predictors shape: ' + str(test_X.shape))
        print('Testing response shape: ' + str(test_y.shape))
        print('\n')  

        # 3. ========== MODELING ==========

        # 3.1 ---------- Balance training/testing data ----------

        # Oversample sparse clases to equalize imbalanced groups
        # https://beckernick.github.io/oversampling-modeling/
        smote = SMOTE(random_state = 1908)
        train_X, train_y = smote.fit_resample(train_X, train_y)
        print('--- Training data over sampled with SMOTE to equalize imbalanced groups ---')
        print('Training predictors shape: ' + str(train_X.shape))
        print('Training response shape: ' + str(train_y.shape))
        print('Resampled dataset shape %s' % Counter(train_y))
        print('\n')

        # 3.2 ---------- Define the model ----------

        model = RandomForestClassifier(n_estimators = 500, min_samples_leaf = 1, max_features = 'auto', bootstrap = True, oob_score = True, n_jobs = -1, random_state = 1908, max_samples = None)
        print('--- Random Forest base model initialized ---')
        print('\n')

        # 3.3 ---------- Fit the model on training data ----------

        fitted_model = model.fit(train_X, train_y)
        print('--- Random Forest model fit ---')
        print('\n')

        # 3.4 ---------- Report accuracy results ----------

        # Evaluate model on the OUTER fold hold out dataset
        yhat = fitted_model.predict(test_X)

        # Calculate the overall accuracy
        current_overall_accuracy = metrics.accuracy_score(test_y, yhat)
        current_balanced_accuracy = metrics.balanced_accuracy_score(test_y, yhat)

        # Convert overall accuracy to pandas dataframe
        current_overall_accuracy_df = pandas.DataFrame({
            'site': [site],
            'split_num': [current_split_num],
            'accuracy': [current_overall_accuracy],
            'balanced_accuracy': [current_balanced_accuracy]
        })

        # Report the overall accuracy
        print('The overall accuracy report is:')
        print(current_overall_accuracy_df)
        print('\n')

        # Calculate the per PFT accuracy
        current_pft_accuracy = metrics.classification_report(test_y, yhat, output_dict = True) # Generate report

        # Convert per PFT accuracy to pandas dataframe and format
        current_pft_accuracy = pandas.DataFrame(current_pft_accuracy).transpose() # Convert to pandas dataframe
        current_pft_accuracy.index.name = 'pft' # Convert row names to column
        current_pft_accuracy.reset_index(inplace=True) # Convert row names to column
        current_pft_accuracy = current_pft_accuracy.assign(site = site) # Add site column
        current_pft_accuracy = current_pft_accuracy.assign(split_num = current_split_num) # Add split number column
        current_pft_accuracy = current_pft_accuracy[['site', 'split_num', 'pft', 'precision', 'recall', 'f1-score', 'support']] # Reorder columns

        # Report the per PFT accuracy
        print('The per PFT accuracy report is:')
        print(current_pft_accuracy)
        print('\n')

        # Report confusion matrix
        print('Confusion matrix:')
        conf_mat = pandas.DataFrame() # Setup a dataframe
        conf_mat['truth'] = test_y # Truth are our known response values
        conf_mat['predict'] = yhat # Predict are our response values predicted by the Random Forest model
        print(str(classDictionary) + '\n')
        print(pandas.crosstab(conf_mat['truth'], conf_mat['predict'], margins=True)) # Cross-tabulate predictions

        print('--- Results reported ---')
        print('\n')

        # 3.5 ---------- Append accuracy results ----------

        overall_accuracy = overall_accuracy.append(current_overall_accuracy_df)
        pft_accuracy = pft_accuracy.append(current_pft_accuracy)

        # 3.6 ---------- Clean up ----------

        os.remove(os.path.join(outDir, trainPolyRasterPath))
        os.remove(os.path.join(outDir, testPolyRasterPath))

    # 4 ========== SAVE ACCURACY RESULTS ==========

    print('The final overall accuracy report is:')
    print(overall_accuracy)
    print('\n')

    print('The final per PFT accuracy report is:')
    print(pft_accuracy)
    print('\n')

    overall_accuracy.to_csv(os.path.join(outDir, site + '_overallAccuracyResults_SMOTE.csv'))
    pft_accuracy.to_csv(os.path.join(outDir, site + '_PFTaccuracyResults_SMOTE.csv'))

if __name__ == '__main__':

    args = parsers.runCV_ClassificationParser().parser.parse_args()

    main(args.site, args.cv_split_num)