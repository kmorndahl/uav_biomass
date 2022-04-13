import os
import sys
import numpy
from osgeo import gdal, ogr, osr
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import OneHotEncoder
import itertools
import joblib
from UAV_fxns import *
parsers = __import__("0_createParsers")

'''
Script to fill existing CHM holes with predictions from random forest model
usage: python 5_1_createCHMs_RF.py site_name
positional arguments:
  site_name         site name
'''

def main(site):

    """
    Main function
    :param site: site name
    """
    
    ###########################################################
    # SET OUTPUT DIRECTORY
    ###########################################################
    
    out_dir = '*/UAV_chm/results/'

    os.chdir(out_dir) # Sets the working directory

    # ========== PARAMETERS ==========

    SHPdriver = ogr.GetDriverByName('ESRI Shapefile') # Specify shapefile driver

    gdal.UseExceptions() # Tell GDAL to throw Python exceptions

    chm_dir = out_dir
    stack_dir = '*/UAV_classification/stacks/'
    class_dir = '*/UAV_classification/results/'

    chmList = []
    stackList = []
    classifiedList = []

    # ========== GET LISTS ==========

    # ---------- Create list of CHMs ----------

    for subdir, dirs, files in os.walk(chm_dir):
        for file in files:
            if (file.endswith('_CHM.tif')):  # Grab only tifs
                chmList.append(os.path.join(subdir, file))

    # ---------- Create list of raster stacks ----------

    for subdir, dirs, files in os.walk(stack_dir):
        for file in files:
            if (file.endswith('_RGBmsStack.tif')):  # Grab only tifs
                stackList.append(os.path.join(subdir, file))

    # ---------- Create list of classified rasters ----------

    for subdir, dirs, files in os.walk(class_dir):
        for file in files:
            if (file.endswith('classified_final.tif')):  # Grab only tifs
                classifiedList.append(os.path.join(subdir, file))

    # ========== GET RASTERS ==========

    print('The current site is: ' + site)
    print('\n')

    chm = [chm for chm in chmList if site in chm][0]
    stack = [stack for stack in stackList if site in stack][0]
    classified = [classified for classified in classifiedList if site in classified][0]

    print('The CHM is: ' + str(chm))
    print('The raster stack is: ' + str(stack))
    print('The classified raster is: ' + str(classified))
    print('\n')

    # ========== ALIGN RASTERS ==========

    rasterList = [chm, stack, classified]

    # Calculate minimum extent of all rasters
    left = []
    top = []
    right = []
    bottom = []
    for pair in itertools.combinations(rasterList, 2): # Get all pairwise combinations from rasterList
        intersect = calculateRasterIntersection(pair[0], pair[1])
        left.append(intersect[0])
        top.append(intersect[1])
        right.append(intersect[2])
        bottom.append(intersect[3])
    intersect = [max(left), min(top), min(right), max(bottom)]
    print('The minimum intersection is: ' + str(intersect))
    print('\n')

    # Assign the reference raster
    ref = chm

    # Apply the minimum intersection to reference raster
    ref = clipRasterWithCoords(intersect, ref)
    print('Reference raster shape: (' + str(ref.RasterYSize) + ', ' + str(ref.RasterXSize) + ')')
    print('\n')

    # Get reference raster information
    gtRef = ref.GetGeoTransform()
    projRef = osr.SpatialReference(wkt=ref.GetProjection())
    epsgRef = projRef.GetAttrValue('AUTHORITY',1)
    xResRef = gtRef[1]
    yResRef = -gtRef[5]
    print('The reference geotransform is: ' + str(gtRef))
    print('The reference EPSG code is: ' + str(epsgRef))
    print('The reference X resolution is: ' + str(xResRef))
    print('The reference Y resolution is: ' + str(yResRef))

    for i in range(len(rasterList)): # Loop through rasters in list

        raster = rasterList[i]

        print('CURRENT RASTER: ' + str(raster))
        print('\n')

        # Denote categorical or continuous
        if 'classified' in raster:
            categorical = True
            print('Raster is categorical')
            print('\n')
        else:
            categorical = False
            print('Raster is continuous')
            print('\n')

        # Open raster
        src = gdal.Open(raster)

        # Get current raster information
        gt = src.GetGeoTransform()
        proj = osr.SpatialReference(wkt=src.GetProjection())
        epsg = proj.GetAttrValue('AUTHORITY',1)
        xRes = gt[1]
        yRes = -gt[5]
        print('The reference geotransform is: ' + str(gt))
        print('The reference EPSG code is: ' + str(epsg))
        print('The reference X resolution is: ' + str(xRes))
        print('The reference Y resolution is: ' + str(yRes))

        # Check to see if the raster needs to be reprojected, if so reproject it
        if(epsgRef != epsg):
            src = reprojectRaster(src, ref)
            print('--- Raster reprojected ---')
            print('\n')
        else:
            print('Raster does not require reprojection')
            print('\n')

        # Check to see if the raster needs to be resampled, if so resample it
        if(xResRef != xRes):
            print('Mismatch in resolution, resampling raster ...')
            print('\n')
            src = resampleRaster(src, ref)
            print('--- Raster resampled ---')
            print('\n')
        else:
            print('Raster does not require resampling')
            print('\n')

        # Crop raster to minimum extent
        src = clipRasterWithCoords(intersect, src)

        # Report final raster shape
        print('Final shape: (' + str(src.RasterYSize) + ', ' + str(src.RasterXSize) + ')')
        print('\n')

        # Add aligned raster back to list
        rasterList[i] = src

    print('The final raster list is: ' + str(rasterList))
    print('\n')

    # Read in rasters
    chm = rasterList[0]
    stack = rasterList[1]
    classified = rasterList[2]
    
    # ========== CONVERT RASTERS TO ARRAY ==========
    
    chmArray = chm.ReadAsArray().astype(numpy.float32)
    chmArray = chmArray[numpy.newaxis, :, :] # Add third dimension
    chmArray[chmArray < 0] = numpy.nan # NoData cells are being read in as highly negative value placeholder, convert to nan
    refArray = chmArray # Save array for future reference
    print('CHM shape: ' + str(chmArray.shape))
    print('--- CHM raster ingested and converted to array ---')
    print('\n')

    classifiedArray = classified.ReadAsArray().astype(numpy.uint8)
    classifiedArray = classifiedArray[numpy.newaxis, :, :] # Add third dimension
    print('Classified shape: ' + str(classifiedArray.shape))
    print('--- Classified raster ingested and converted to array ---')
    print('\n')

    stackArray = stack.ReadAsArray()
    stackArray[stackArray < 0] = numpy.nan # NoData cells are being read in as highly negative value placeholder, convert to nan
    print('Raster stack shape: ' + str(stackArray.shape))
    print('--- Raster stack ingested and converted to array ---')
    print('\n')

    # ========== RESHAPE ARRAYS ==========

    # Arrays from rasters arranged as (bands, rows, columns)
    # First, reshape to flatten to 2D as (bands, cells)
    # Then, transpose to (cells, bands) for use in sklearn functions

    chmArray = chmArray.reshape(chmArray.shape[0], -1).transpose()
    print('CHM reshape: ' + str(chmArray.shape))
    print('--- CHM array reshaped ---')
    print('\n')

    classifiedArray = classifiedArray.reshape(classifiedArray.shape[0], -1).transpose()
    print('Classified reshape: ' + str(classifiedArray.shape))
    print('--- Classified array reshaped ---')
    print('\n')

    stackArray = stackArray.reshape(stackArray.shape[0], -1).transpose()
    print('Raster stack reshape: ' + str(stackArray.shape))
    print('--- Raster stack array reshaped ---')
    print('\n')

    # ========== IMPUTE MISSING VALUES ==========

    # Continuous
    imputer_cont = SimpleImputer(missing_values = numpy.nan, strategy = 'median')

    chmArray = imputer_cont.fit_transform(chmArray)

    print('--- CHM missing values imputed with median ---')
    print('\n')

    stackArray = imputer_cont.fit_transform(stackArray)

    print('--- Raster stack missing values imputed with median ---')
    print('\n')

    # Categorical
    imputer_cat = SimpleImputer(missing_values = numpy.nan, strategy = 'most_frequent')

    classifiedArray = imputer_cat.fit_transform(classifiedArray)

    print('--- Classified missing values imputed with most common value ---')
    print('\n')

    # ========== ONE HOT ENCODE CATEGORICAL VARIABLES ==========

    onehot_encoder = OneHotEncoder(sparse = False)
    classifiedArray = onehot_encoder.fit_transform(classifiedArray)
    print('Classified array one hot encoded shape: ' + str(classifiedArray.shape))
    print('--- Classified array one hot encoded ---')
    print('\n')

    # ========== AGGREGATE PREDICTORS ==========

    predictors = numpy.concatenate([stackArray, classifiedArray], axis = 1) # axis = 1 joins by columns e.g. x.shape = (100, 2) y.shape = (100, 3), xy.shape = (100, 5)
    print('Predictor array: ' + str(classifiedArray.shape))
    print('--- Predictor array finalized ---')
    print('\n')

    # ========== TRAIN RANDOM FOREST ==========
    
    # ---------- Gather predictor and response datasets ----------

    X = predictors
    y = chmArray[:, 0] # To go from shape (cells, bands) to shape (cells,)

    print('--- Predictor and response datasets created ---')
    print('Final X shape: ' + str(X.shape))    
    print('Final y shape: ' + str(y.shape))
    print('\n')

    # ---------- Split the datasets into training and testing ----------

    # Stratified by classification
    # X and y are entire rasters containing tens of millions of pixel values
    # 0.003 and 0.001 give training/testing sets on the order of tens to hundreds of thousands
    train_X, test_X, train_y, test_y = train_test_split(X, y, train_size=0.003, test_size=0.001, random_state=1908, stratify=X[:, -1])
    print('--- Predictor and response datasets divided into stratified training and testing data ---')
    print('\n')

    print('Predictor training set:')
    print(train_X.shape)
    print('Predictor test set:')
    print(test_X.shape)
    print('Response training set:')
    print(train_y.shape)
    print('Response test set:')
    print(test_y.shape)
    print('\n')

    # ---------- Train the Random Forest model ----------

    # Initialize our model with 500 trees
    rf = RandomForestRegressor(n_estimators = 500, random_state = 1908, oob_score = False)
    print('--- Random Forest model initialized ---')
    print('\n')

    # Fit our model to training data
    rf = rf.fit(train_X, train_y)
    print('--- Random Forest model fit ---')
    print('\n')

    # Save the model
    modelPath = str(site + '_CHMmodelRF.pkl')
    joblib.dump(rf, open(modelPath, 'wb'))

    # ========== REPORT METRICS FROM TEST SET  ==========

    # Open text file to write results to
    resultsPath = str(site + '_CHMresultsRF.txt')
    outResults = open(resultsPath, 'a')
    print('Site: ' + site + '\n', file = outResults)

    # Check band importance
    print('---------- Band importance ----------', file = outResults)
    bands = ['Red1cm', 'Green1cm', 'Blue1cm', 'NDVI', 'Blue', 'Green', 'NIR', 'Red Edge', 'Red', 'CHM', 'Texture', 'Classification']
    for band, imp in zip(bands, rf.feature_importances_):
        print(band + ' importance: ' + str(imp), file = outResults)
    print('\n', file = outResults)

    # Predict on the test dataset
    predictionsTest = rf.predict(test_X)

    # Calculate model accuracy for the test set
    print('---------- Test set accuracy ----------', file = outResults)
    mseTest = metrics.mean_squared_error(test_y, predictionsTest)
    print('Test mean squared error: ' + str(mseTest) + '\n', file = outResults)

    # Close results file
    outResults.close()

    # ========== PREDICT  ==========

    # Predict for each pixel
    predicted = rf.predict(X)
    print('--- Predictions made ---')
    print('\n')

    # Reshape our predictied array
    predicted = predicted.reshape(refArray[0, :, :].shape)
    print('--- Array returned to original shape ---')
    print('\n')

    # Convert predicted array back to raster
    chmPath = str(site + '_CHM_RF.tif')
    ArrayToRaster(chm, predicted, chmPath, 'float')
    print('--- Predicted raster written to disk ---')
    print('\n')

if __name__ == '__main__':

    args = parsers.fillCHMParser().parser.parse_args()

    main(args.site_name)