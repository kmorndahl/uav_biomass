# --------------------------------------------------- #
# ------ PREPARE OUTER CROSS VALIDATION SPLITS ------ #
# --------------------------------------------------- #
# Splits are grouped by polygon ID
# Splits are stratified by PFT

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
import pandas
import numpy
from collections import Counter
from osgeo import ogr
from sklearn.model_selection import StratifiedShuffleSplit
from UAV_fxns import *

# 1. ========== SET UP  ==========

# 1.1 ---------- Directories ----------

###########################################################
# SET OUTPUT DIRECTORY
###########################################################

workingDir = '*/0_UAV_final/data/'

os.chdir(workingDir) # Sets the working directory

trainingDir = '*/0_UAV_final/data/'
stackDir = '*/UAV_classification/stacks/'

trainingName = 'all_training_polys_final.shp' # Name of training data file, with file extension

site_list = [] # List of sites to include, if you want to include all sites, leave this as an empty list

file_suffix = '_RGBmsStack.tif' # Suffix for identifying the correct tifs within the stack directory, including file extension

splits_outer = 10 # Set number of outer cross validation splits

# 2. ========== DATA PREPARATION ==========

# 2.1 ---------- Load training points ----------

# Open training data
polysPath = os.path.join(trainingDir, trainingName)
polysDataSource = ogr.Open(polysPath, 1)
polys = polysDataSource.GetLayer()
print('The total number of training/testing polygons is: ' + str(polys.GetFeatureCount()))
print('\n')

# 2.2 ---------- Create list of raster stacks ----------

stackList = []

# Get all tifs
for subdir, dirs, files in os.walk(stackDir):
    for file in files:
        if site_list: # If sites provided, grab only tifs that match suffix and have site in name
            if (file.endswith(file_suffix) and any(substring in file for substring in site_list)):  # Grab only tifs
                stackList.append(os.path.join(subdir, file))
        else: # If no sites provided, grab all tifs that match suffix
            if (file.endswith(file_suffix)):  # Grab only tifs
                stackList.append(os.path.join(subdir, file))

# 3. ========== PREPARE OUTER CROSS-VALIDATION DATA SPLITS ==========

# Nested CV
# https://machinelearningmastery.com/nested-cross-validation-for-machine-learning-with-python/
# https://scikit-learn.org/stable/auto_examples/model_selection/plot_nested_cross_validation_iris.html#sphx-glr-auto-examples-model-selection-plot-nested-cross-validation-iris-py

df = pandas.DataFrame(columns = ['site', 'split_num', 'train', 'test'])

row_count = 0

for stackPath in stackList: # Loop through raster stacks

    # 3.1 ---------- Prep data ----------

    polys.SetAttributeFilter(None) # Remove previous site code filter

    site = stackPath.split('/')[-1].split('_')[0]
    print('===== The current site is: ' + site + ' =====')
    print('\n')

    # 3.2 ---------- Extract a single site from training points ----------

    selectByAttribute(polys, 'site_code', site)
    print('--- Training data subset to include only site ' + site + ' with ' + str(polys.GetFeatureCount()) + ' polygons ---')
    print('\n')

    # 3.3 ---------- Extract polygon IDs and classes ----------

    polyID = getColumn(polys, 'polyID')
    PFTclass = getColumn(polys, 'class')
    print('--- Polygon IDs and PFT classes extracted ---')
    print('\n')

    # 3.4 ---------- Create splits based on polygon IDs ----------

    cv_outer = StratifiedShuffleSplit(test_size = .20, n_splits = splits_outer, random_state = 1908) # Create splitter
    splits = cv_outer.split(polyID, PFTclass) # Perform split, stratifying by PFT class

    # Split results are location in polyID list, need to grab actual polygon IDs
    split_num = 1
    for train_index, test_index in splits:

        print('===== Split ' + str(split_num) + ' of ' + str(splits_outer) + '=====')
        print('\n')

        train_ID = numpy.array(polyID)[train_index] # Grab actual polygon IDs
        test_ID = numpy.array(polyID)[test_index] # Grab actual polygon IDs

        train_classes = numpy.array(PFTclass)[train_index] # Grab the corresponding PFT classes
        test_classes = numpy.array(PFTclass)[test_index] # Grab the corresponding PFT classes

        # Check to make sure the same classes are in training and test set
        # Because StratifiedShuffleSplit seeks to preserve correct training/test set ratios, sometimes end up with classes missing from the test set (the smaller set)
        # https://github.com/scikit-learn/scikit-learn/issues/8913
        train_classes_unique = numpy.unique(train_classes) # Get all unique classes
        test_classes_unique = numpy.unique(test_classes) # Get all unique classes
        missing_classes = numpy.setdiff1d(train_classes_unique, test_classes_unique)

        if numpy.setdiff1d(test_classes_unique, train_classes_unique).size > 0:
            print('Class missing from training set, halting execution...')
            sys.exit()

        if missing_classes.size > 0:

            print('Classes missing from test set: ' + str(missing_classes))
            print('\n')

            # Convert to lists for indexing
            train_ID = list(train_ID)
            test_ID = list(test_ID)
            train_classes = list(train_classes)
            test_classes = list(test_classes)

            # Redistribute missing classes
            for cls in missing_classes:
               
                print('The current missing class is: ' + cls)
                print('\n')

                idx = train_classes.index(cls) # Get index of first match of missing class

                test_classes.append(train_classes.pop(idx)) # Remove missing class from training set and add to test set
                test_ID.append(train_ID.pop(idx)) # Remove missing class polygon ID from training set and add to test set

            # Confirm redistribution
            if len(train_ID) != len(train_classes):
                print('Training set IDs and classes are different lengths, halting execution')
                sys.exit()
            if len(test_ID) != len(test_classes):
                print('Test set IDs and classes are different lengths, halting execution...')
                sys.exit()
            if Counter(numpy.unique(train_classes)) != Counter(numpy.unique(test_classes)):
                print('Different classes between training and test set, halting execution...')
                sys.exit()

            print('--- Missing classes redistributed ---')
            print('\n')
        
        # Report splitting results
        print('Training set polygon IDs:')
        print(train_ID)
        print('The number of training set polygon IDs is: ' + str(len(train_ID)))
        print('Testing set polygon IDs:')
        print(test_ID)
        print('The number of testing set polygon IDs is: ' + str(len(test_ID)))
        print('Training set PFT classes:')
        print(train_classes)
        print('The number of training set PFT classes is: ' + str(len(train_classes)))
        print('Testing set PFT classes:')
        print(test_classes)
        print('The number of testing set PFT classes is: ' + str(numpy.unique(test_classes)))
        print('Unique PFT classes in training set: ' + str(numpy.unique(train_classes)))
        print('Unique PFT classes in testing set: ' + str(numpy.unique(train_classes)))
        print('\n')

        # Append to dataframe
        df.loc[row_count, 'site'] = site
        df.loc[row_count, 'split_num'] = split_num
        df.loc[row_count, 'train'] = list(train_ID)
        df.loc[row_count, 'test'] = list(test_ID)

        split_num += 1
        row_count += 1

    print('--- SPLITTING COMPLETE FOR SITE ' + site, ' ---')
    print('\n')

# 4. ========== SAVE SPLITS ==========

print('The outer cross validation splits are:')
print(df)
print('\n')

df.to_csv(workingDir + 'outerCVsplits_n' + str(splits_outer) + '.csv')

