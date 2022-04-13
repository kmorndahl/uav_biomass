import os
import sys
import numpy
import pandas
from osgeo import ogr
from UAV_fxns import *

# 1. ========== PARAMETERS ==========

###########################################################
# SET OUTPUT DIRECTORY
###########################################################

resultsDir = '*/UAV_classification/results'

os.chdir(resultsDir)

dataDir = '*/0_UAV_final/data'

site_list = [] # List of sites to include, if you want to include all sites, leave this as an empty list

file_suffix = '_classified_final.tif' # Suffix for identifying the correct tifs within the data directory, including file extension

allQuadsPath = os.path.join(dataDir, "UAV_quadrats.shp")
fieldDataPath = os.path.join(dataDir, "classification_field_data.csv")

SHPdriver = ogr.GetDriverByName('ESRI Shapefile') # Specify shapefile driver

classDictionary = {'BRYOPHYTES': 1, # Dictionary for recoding categorical class field to integer
                   'DECIDUOUS SHRUBS':2,
                   'EVERGREEN SHRUBS':3,
                   'FORBS':4,
                   'GRAMINOIDS':5,
                   'LICHENS':6,
                   'NON VEGETATED':7,
                   'SHADOW':8,
                   'TREES':9}

PFTdictionary = {'BARE GROUND': 'NON VEGETATED', # Dictionary for remapping PFTs
                 'CLUBMOSSES': 'FORBS',
                 'GRASSES': 'GRAMINOIDS',
                 'HORSETAILS': 'FORBS',
                 'LITTER': 'NON VEGETATED',
                 'MUSHROOM': 'NON VEGETATED',
                 'ROCKS': 'NON VEGETATED',
                 'RUSHES': 'GRAMINOIDS',
                 'SCAT': 'NON VEGETATED',
                 'SEDGES': 'GRAMINOIDS',
                 'TUSSOCK': 'GRAMINOIDS',
                 'WATER': 'NON VEGETATED'
                 }

# 2. ========== READ IN AND TIDY DATA ==========

# 2.1 ---------- Create list of rasters ----------

classifiedList = []

# Get all tifs
for subdir, dirs, files in os.walk(resultsDir):
    for file in files:
        if site_list: # If sites provided, grab only tifs that match suffix and have site in name
            if (file.endswith(file_suffix) and any(substring in file for substring in site_list)):  # Grab only classified tifs in site list
                classifiedList.append(os.path.abspath(os.path.join(subdir, file)))
        else: # If no sites provided, grab all tifs that match suffix
            if (file.endswith(file_suffix)):  # Grab only classified tifs
                classifiedList.append(os.path.abspath(os.path.join(subdir, file)))

# 2.2 ---------- Format paths ----------

classifiedList = [path.replace(os.sep, '/').replace('////', '//') for path in classifiedList]
allQuadsPath = allQuadsPath.replace('\\', '/')
fieldDataPath = fieldDataPath.replace('\\', '/')

print('The quadrats file is: ', allQuadsPath)
print('The field data file is: ', fieldDataPath)
print('The list of classified rasters is: ', str(classifiedList))
print('\n')

# 3. ========== COMPARE CLASSIFIED RASTER TO FIELD DATA ==========

for classified in classifiedList: # Loop through classified rasters

    # 3.1 ---------- Get site quadrats ----------

    # Specify site
    site = classified.split('/')[-1].split('_')[0]
    print('The current site is: ', site)
    print('\n')

    # Open all quadrats shapefile
    allQuadsDataSource = ogr.Open(allQuadsPath, 1)
    allQuadsLyr = allQuadsDataSource.GetLayer()

    # Select site quadrats
    quadLyr = selectByAttribute(allQuadsLyr, 'site_code', site)
    print('--- Quadrat data subset to include only site ' + site + ' ---')
    print('\n')

    # Push site quadrats layer to shapefile
    # Must do this for .RasterizeLayer function within ZonalStatisticsCategorical within loopZonalStatisticsCategorical to work ... for some reason ...
    quadPath = site + '_quadrats.shp'
    outds = SHPdriver.CreateDataSource(quadPath)
    outlyr = outds.CopyLayer(quadLyr, site + "_quadrats")
    del allQuadsDataSource, allQuadsLyr, quadLyr, outds, outlyr

    # 3.2 ---------- Format dataset ----------

    # Read in data
    fieldData = pandas.read_csv(fieldDataPath)
    print('Field data:')
    print(fieldData)
    print('\n')

    # Subset data
    siteFieldData = fieldData[fieldData.site_code == site][['site_code', 'quadrat_num', 'cover_percent', 'PFT_fine']] # Select site
    siteFieldData = siteFieldData[siteFieldData.PFT_fine != 'TOTAL'] # Exclude totals
    print('Subset field data:')
    print(siteFieldData)
    print('\n')

    # Remap PFT values
    siteFieldData = siteFieldData.replace({'PFT_fine': PFTdictionary})
    print('Remapped field data:')
    print(siteFieldData)
    print('\n')

    # Sum cover values by site, quadrat and PFT
    PFTtotals = siteFieldData.groupby(['site_code', 'quadrat_num', 'PFT_fine'], as_index=False)['cover_percent'].sum()
    print('PFT summed:')
    print(PFTtotals)
    print('\n')

    # Add column for Random Forest PFT predictions
    PFTtotals['cover_predicted'] = numpy.nan
    print('PFT summed with Random Forest field:')
    print(PFTtotals)
    print('\n')

    # Get list of all required classes
    requiredClasses = list(classDictionary.keys())
    requiredClasses.remove('SHADOW') # Remove SHADOW class since this will never be in our dataset PFTs
    print('List classes:')
    print(requiredClasses)
    print('\n')

    # For each quadrat, get a list of all PFTs present in that quadrat, according to the dataset
    PFTunique = PFTtotals.groupby(['quadrat_num'])['PFT_fine'].unique()
    print('PFTs present in each quadrat:')
    print(PFTunique)
    print('\n')

    # Compare the PFT lists from the dataset to the complete list, find PFTs that are missing from the dataset, and enter them as 0% cover
    for x in range(0, len(PFTunique)):
        quadrat = PFTunique.iloc[[x]].index[0] # Get the current quadrat number
        datasetPFTs= PFTunique[x] # Get list of PFTs from dataset
        missingPFTs = list(set(requiredClasses)-set(datasetPFTs)) # Get list of PFTs which are missing from the dataset
        print('The PFTs that are missing from quadrat ' + quadrat + ' are: ' + str(missingPFTs))
        for PFT in missingPFTs: # For each PFT that is missing from the current quadrat...
            PFTtotals = PFTtotals.append({'site_code': site, 'quadrat_num': quadrat, 'PFT_fine': PFT, 'cover_percent': 0, 'cover_predicted': numpy.nan}, ignore_index=True) # Add it to the dataset with 0% cover

    print('Zeros entered for PFTs not present in a particular quadrat:')
    print(PFTtotals)
    print('\n')

    # 3.3 ---------- Get Raster Zonal Statistics ----------

    # Grab quadrat data and perform zonal statistics
    quadPath = os.path.abspath(os.path.join(resultsDir, quadPath))
    stats = loopZonalStatisticsCategorical(quadPath, classified, 'quadrat') # Must pass quadrats as path to shapefile NOT layer for some reason ...
    stats.pop('89.5m', None) # Remove WICKERSHAM extra quadrat if applicable

    print('Zonal statistics:')
    print(stats)
    print('\n')

    numClasses = 9

    # Convert pixel based numbers to percentages
    for quadKey in stats:
        quadStats = stats[quadKey] # Pull appropriate pixel values from dictionary
        print('Calculating percentages for quadrat ' + quadKey)
        pixels = list(quadStats.values()) # Get all of the pixel counts as a list
        quadPixels = sum(pixels) # Get the total number of pixels in the quadrat
        for y in range(1, numClasses + 1):
            if(y not in list(quadStats.keys())): # If a certain class is not present at this quadrat, skip it
                pass
            else: # Otherwise proceed with percent calculation
                classPixels = quadStats[y] # Get number of pixels for current class
                classPercent = round((classPixels / quadPixels) * 100, 2) # Calculate the percentage of the total pixels in the quadrat
                stats[quadKey][y] = classPercent # Replace the pixel count value in the dictionary with the percentage

    print('Zonal statistics as percentages:')
    print(stats)
    print('\n')

    # 3.4 ---------- Merge Raster Data and Field Data ----------

    # Compare quadrat shapefile and field data, remove from field data any quadrats not present in quadrat shapefile
    shpQuads = list(stats.keys()) # Get quadrats from shapefile
    fieldQuads = PFTtotals.quadrat_num.unique() # Get quadrats from field data
    missingQuads = list(set(fieldQuads)-set(shpQuads)) # Get list of quadrats which are missing from the shapefile
    print('The following quadrats are not present in the quadrat shapefile, removing from field data ... ' + str(missingQuads))
    PFTtotals = PFTtotals[~PFTtotals.quadrat_num.isin(missingQuads)] # Keep only observations NOT in the missing quadrats list
    print('Missing quadrats dropped:')
    print(PFTtotals)
    print('\n')

    # Loop through each row in the PFT totals dataset
    # Each row is a unique site/quadrat/PFT combination
    for x in range(0, len(PFTtotals)):

        # Grab information from the dataset
        quadrat = PFTtotals.iloc[[x]]['quadrat_num'].values[0]
        PFT = PFTtotals.iloc[[x]]['PFT_fine'].values[0]
        PFTkey = classDictionary[PFT]

        # Use the information from the dataset to index the dictionary and pull the appropriate predicted PFT percent
        try :
            coverPredicted = stats[quadrat][PFTkey]
            print('The predicted value of ' + str(coverPredicted) + ' will be entered into the dataset for quadrat ' + quadrat + ', PFT = ' + PFT)
            print('\n')

        except KeyError: # If the PFT from the dataset is not found in the dictionary, add a zero
            print('PFT type not found in quadrat according to predictions, quadrat ' + quadrat + ', PFT = ' + PFT + ' will be entered as 0')
            print('\n')
            coverPredicted = 0

        # Add the predicted PFT percent to the dataset
        PFTtotals['cover_predicted'].iat[x] = coverPredicted

    print('Final PFT totals from field data and classified raster:')
    print(PFTtotals)
    print('\n')

    csvPath = str(site + '_PFTtotal.csv')
    PFTtotals.to_csv(os.path.abspath(os.path.join(resultsDir, csvPath)))

    # 3.5 ---------- Clean up ----------

    os.path.join(resultsDir, quadPath)
    quadPath = quadPath.replace('\\', '/')
    SHPdriver.DeleteDataSource(quadPath)