# ---------------------------------------------------- #
# ------ FUNCTIONS FOR WORKING WITH UAV IMAGERY ------ #
# ---------------------------------------------------- #

# Functions used to ingest UAV imagery products and training/testing data and produce classified output
# GDAL based implementation

import os
import sys
import numpy
from osgeo import gdal, ogr, osr, gdal_array
import itertools
import datetime

# FILE WRANGLING  ----------------------------------------------------------------------

# Purpose: get all file paths matching a set of criteria within Pix4D output nested directory
# Parameters: directory = directory to search, type = label that will determine the criteria to use, choose from: 'rgb', 'multispectral', 'chm', 'texture'
# Return: list of file path strings
def getPathsPix4D(directory, type):

    # Set workspace
    os.chdir(directory)

    all = []

    # Get list of files
    for subdir, dirs, files in os.walk('.'):
        for file in files:
            if(type == 'rgb'):
                if ('2_mosaic' in subdir) and ('tiles' not in subdir) and ('30m' in file) and (file.endswith('.tif')):  # Grab only 30m full rgb orthomosaics
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'multispectral'):
                if ('4_index' in subdir) and ('tiles' not in subdir) and ('30m' in file) and (file.endswith('.tif')):  # Grab only 30m reflectance tifs for the 5 bands + NDVI
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'chm'):
                if ('5_chm' in subdir) and ('30m' in file) and (file.endswith('.tif')):  # Grab only 30m chm tifs
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'texture'):
                if ('30m' in file) and (file.endswith('_texture.tif')):
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

    print('--- List creation complete ---')
    print('\n')
    return all

# Purpose: get all file paths matching a set of criteria within a directory
# Parameters: directory = directory to search, type = label that will determine the criteria to use, choose from: 'rgb', 'multispectral', 'chm', 'texture'
# Return: list of file path strings
def getPaths(directory, type):

    # Set workspace
    os.chdir(directory)

    all = []

    # Get list of files
    for subdir, dirs, files in os.walk('.'):
        for file in files:
            if(type == 'rgb'):
                if ('30m' in file) and ('mosaic' in file) and (file.endswith('.tif')):  # Grab only 30m full rgb orthomosaics
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'multispectral'):
                if ('30m' in file) and ('reflectance' in file or 'ndvi' in file) and (file.endswith('.tif')):  # Grab only 30m reflectance tifs for the 5 bands + NDVI
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'chm'):
                if ('30m' in file) and (file.endswith('chm_focalMean.tif')):  # Grab only 30m chm tifs
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list
            if(type == 'texture'):
                if ('30m' in file) and (file.endswith('_texture.tif')):
                    all.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

    print('--- List creation complete ---')
    print('\n')
    return all

# Purpose: get all file paths belonging to a certain site within a list
# Parameters: pathList = list to search, site = site to match, type = label that will determine the criteria to use, choose from: 'rgb', 'multispectral', 'chm', 'texture'
# Return: list of file path strings
def selectPath(pathList, site, type):

    # Select the tif of interest
    path = [x for x in pathList if x.split('\\')[4].split('_')[0] == site]

    # Make sure we have the correct number of tifs
    if(type == 'rgb'):
        if len(path) != 1:
            raise Exception('Incorrect number of RGB tifs found')

    if(type == 'multispectral'):
        if len(path) != 6:
            raise Exception('Incorrect number of multispectral tifs found')
        path.sort()  # Sort the list

    if(type == 'chm'):
        if len(path) != 1:
            raise Exception('Incorrect number of CHM tifs found')

    if(type == 'texture'):
        if len(path) != 1:
            raise Exception('Incorrect number of texture tifs found')

    print('--- Selection complete ---')
    print('\n')
    return path

# DATA IN/OUT ----------------------------------------------------------------------

# Purpose: convert path to raster to array (raster can contain multiple bands)
# Parameters: rasterPath = string representation of a file path to a .tif file
# Return: array
def RasterToArray(rasterPath):

    # Open raster
    imgRaster = gdal.Open(rasterPath)

    # Initialize 3D array using properties from raster
    # Dimension 1: Latitude
    # Dimension 2: Longitude
    # Dimenson 3: Bands
    imgArray = numpy.zeros((imgRaster.RasterYSize, imgRaster.RasterXSize, imgRaster.RasterCount), gdal_array.GDALTypeCodeToNumericTypeCode(imgRaster.GetRasterBand(1).DataType))

    # Loop over all bands in the raster stack
    for b in range(imgRaster.RasterCount):

        # Grab the band
        band = imgRaster.GetRasterBand(b + 1) # Remember, GDAL index is on 1, but Python is on 0 -- so we add 1 for our GDAL calls

        # Read in the bands data into the third dimension of our array
        imgArray[:, :, b] = band.ReadAsArray()

        # Report progress
        print('Processed band ' + str(b + 1) + ' of ' + str(imgRaster.RasterCount))

    print('--- Conversion to array complete ---')
    print('\n')

    return imgArray

# Purpose: convert array to .tif raster and save (array can be three dimensional)
# Parameters: rasterRef = raster to pull reference information from (can be GDAL dataset or path string), array = array to convert, outPath = path string for output .tif, dataType = indicate whether output data should be 'integer' or 'float', NoData = specify NoData value if none in reference raster or need to change it
# Return: raster saved to disk, no output from function
def ArrayToRaster(rasterRef, array, outPath, dataType, NoData = None):

    # Open raster
    if isinstance(rasterRef, str):
        rasterRef = gdal.Open(rasterRef)
        print('Reference raster opened')

    # Get raster driver
    rasterDriver = gdal.GetDriverByName('GTiff')

    # Get size properties from raster
    ncol = rasterRef.RasterXSize # Columns
    nrow = rasterRef.RasterYSize # Rows

    # Determine number of bands
    if (len(array.shape) == 3):  # Array is 3D
        numBands = array.shape[-1]
        print('3D array detected with ' + str(numBands) + ' bands')
    else: # Array is 2D
        numBands = 1
        array = array[:, :, None] # Add third dimension for indexing purposes
        print('2D array detected')

    # Determine NoData value
    if NoData == None:
        refBand = rasterRef.GetRasterBand(1)
        NoData = refBand.GetNoDataValue()
    print('The NoData value will be: ' + str(NoData))

    # Create output raster
    if(dataType == 'integer'):
        outRaster = rasterDriver.Create(outPath, ncol, nrow, numBands, gdal.GDT_Byte) # Initalize output raster
        print('Integer output raster initialized')
    if (dataType == 'float'):
        outRaster = rasterDriver.Create(outPath, ncol, nrow, numBands, gdal.GDT_Float32)  # Initalize output raster
        print('Float output raster initialized')

    # Write each subarray to band
    for b in range(numBands):
        subarray = array[:, :, b]
        band = outRaster.GetRasterBand(b + 1) # GDAL functions are 1 indexed
        band.WriteArray(subarray)
        print('Subarray written to band ' + str(b + 1))
        band.SetNoDataValue(NoData)
        print('NoData information added')
        band.FlushCache()
        band = None

    # Add projection and geoinformation
    geotrans = rasterRef.GetGeoTransform()
    proj = rasterRef.GetProjection()
    outRaster.SetGeoTransform(geotrans)
    outRaster.SetProjection(proj)
    print('Projection and geoinformation added')

    # Finalize changes
    outRaster.FlushCache()
    outRaster = None
    print('--- Array written to raster, changes finalized ---')
    print('\n')

# SHAPEFILE PROCESSING  ----------------------------------------------------------------------

# Purpose: get list of unique site codes present in a shapefile
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer
# Return: list of strings denoting unique site codes
def getSiteCodes(inData):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    site_codes = []

    for feature in inLayer:
        if feature.GetField("site_code") not in site_codes:
            site_codes.append(feature.GetField("site_code"))

    inLayer.ResetReading()
    print('--- Site code list creation complete ---')
    print('\n')
    return site_codes

# Purpose: extract a column from a shapefile and place values in list
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer; colName = name of column to grab
# Return: list of values from column
def getColumn(inData, colName):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    column = []

    for feature in inLayer:
        column.append(feature.GetField(colName))

    inLayer.ResetReading()
    print('--- Column extraction complete ---')
    print('\n')
    return column

# Purpose: subset a shapefile based on criteria
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer, field = field to select by, value = value/criteria to select by
# Return: shapefile layer
def selectByAttribute(inData, field, value):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Select criteria of interest
    inLayer.SetAttributeFilter(field + " = " + "'" + value + "'")

    # Ensure correct criteria is selected
    for feature in inLayer:
        if feature.GetField(field) != value:
            raise Exception('Incorrect field found, try filter by attributes again')
    inLayer.ResetReading()

    ptsNumFeatures = inLayer.GetFeatureCount()
    print('The number of features for ' + field + ' ' + value + ' is: ' + str(ptsNumFeatures))
    print('--- Select by attribute complete ---')
    print('\n')
    return inLayer

# Purpose: subset a shapefile based on multiple critieria
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer; field = field to select by; values = list of values/criteria to select by
# Return: shapefile layer
def selectByAttributeList(inData, field, values):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Select criteria of interest
    inLayer.SetAttributeFilter(field + " IN {}".format(tuple(values)))

    # Ensure correct criteria is selected
    for feature in inLayer:
        if feature.GetField(field) not in values:
            raise Exception('Incorrect field found, try filter by attributes again')
    inLayer.ResetReading()

    numFeatures = inLayer.GetFeatureCount()
    print('The number of selected features for ' + field + ' is: ' + str(numFeatures))
    print('--- Select by attribute list complete ---')
    print('\n')
    # return inLayer

# Purpose: project shapefile to a specific spatial reference
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer, outPath = string denoting output path, fromSpatialRef = current spatial reference, toSpatialRef = desired spatial reference
# Return: nothing, shapefile written to disk
def projectShapefile(inData, outPath, fromSpatialRef, toSpatialRef):

    # Create a coordinate transformation
    coordTrans = osr.CoordinateTransformation(fromSpatialRef, toSpatialRef)

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Get driver
    SHPdriver = ogr.GetDriverByName('ESRI Shapefile')

    # Create output file, specifying correct spatial reference and geometry type
    outDataSource = SHPdriver.CreateDataSource(outPath)
    outLayer = outDataSource.CreateLayer(outPath[:-4], toSpatialRef, geom_type=inLayer.GetGeomType()) # parameters: CreateLayer(layer name, spatial reference, geometry type)

    # Add fields -- pull fields from input and copy over to output
    inLayerDefn = inLayer.GetLayerDefn()
    for i in range(0, inLayerDefn.GetFieldCount()):
        fieldDefn = inLayerDefn.GetFieldDefn(i)
        outLayer.CreateField(fieldDefn)

    # Get the output layers feature definition
    outLayerDefn = outLayer.GetLayerDefn()

    # Loop through the input features
    numFeatures = inLayer.GetFeatureCount()
    cnt = 1
    inFeature = inLayer.GetNextFeature()
    while inFeature is not None:  # While there are still features ...

        # Get the input geometry
        geom = inFeature.GetGeometryRef()

        # Reproject the geometry
        geom.Transform(coordTrans)

        # Create a new feature
        outFeature = ogr.Feature(outLayerDefn)

        # Set the geometry and attribute
        outFeature.SetGeometry(geom)
        for i in range(0, outLayerDefn.GetFieldCount()):
            outFeature.SetField(outLayerDefn.GetFieldDefn(i).GetNameRef(), inFeature.GetField(i))

        # Add the feature to the shapefile
        outLayer.CreateFeature(outFeature)

        # Dereference the features and get the next input feature
        outFeature = None
        inFeature = inLayer.GetNextFeature()

        # Report progress
        print('Feature ' + str(cnt) + ' of ' + str(numFeatures) + ' projected ...')
        cnt += 1

    # Save and close DataSources
    inDataSource = None
    outDataSource = None

    print('--- Shapefile reprojection complete ---')
    print('\n')

# Purpose: buffer point data to a specific distance
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer, outPath = string denoting output path, spatialReference = desired spatial reference, bufferDist = distance for buffer in meters
# Return: nothing, shapefile written to disk
def createBuffer(inData, outPath, spatialReference, bufferDist):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Get driver
    SHPdriver = ogr.GetDriverByName('ESRI Shapefile')

    # Check to see if the output path is already occupied, and if so delete it to allow creation of new file
    if os.path.exists(outPath):
        SHPdriver.DeleteDataSource(outPath)

    # Create output layer, specifiying spatial reference and geometry type
    outDataSource = SHPdriver.CreateDataSource(outPath)
    outLayer = outDataSource.CreateLayer(outPath, spatialReference, geom_type=ogr.wkbPolygon)

    # Add fields -- pull fields from input and copy over to output
    inLayerDefn = inLayer.GetLayerDefn()
    for i in range(0, inLayerDefn.GetFieldCount()):
        fieldDefn = inLayerDefn.GetFieldDefn(i)
        outLayer.CreateField(fieldDefn)

    # Get the output layers feature definition
    outLayerDefn = outLayer.GetLayerDefn()

    # Loop through features in input layer
    numFeatures = inLayer.GetFeatureCount()
    cnt = 1
    inFeature = inLayer.GetNextFeature()
    while inFeature is not None:  # While there are still features ...

        # Get the input geometry
        inGeom = inFeature.GetGeometryRef()

        # Perform buffer
        geomBuffer = inGeom.Buffer(bufferDist)

        # Create output feature
        outFeature = ogr.Feature(outLayerDefn)

        # Set the geometry and attribute
        outFeature.SetGeometry(geomBuffer)
        for i in range(0, outLayerDefn.GetFieldCount()):
            outFeature.SetField(outLayerDefn.GetFieldDefn(i).GetNameRef(), inFeature.GetField(i))

        # Add output feature to output layer
        outLayer.CreateFeature(outFeature)

        # Dereference the feature and get the next input feature
        outFeature = None
        inFeature = inLayer.GetNextFeature()

        print('Feature ' + str(cnt) + ' of ' + str(numFeatures) + ' buffered')
        cnt += 1

    # Save and close DataSources
    inDataSource = None
    outDataSource = None

    print('--- Buffer points complete ---')
    print('\n')

# Purpose: given an input categorical field, create new integer field with unique values mapping to categorical field
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer, dictionary = python dictionary mapping categorical values to integer values, inField = field where categorical variable is stored, outField = name of output integer field
# Return: nothing, shapefile edited in place
def categoricalFieldToInteger(inData, dictionary, inField, outField):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Add outField if it does not already exist
    inLayerDef = inLayer.GetLayerDefn()
    field_names = [inLayerDef.GetFieldDefn(i).GetName() for i in range(inLayerDef.GetFieldCount())]
    if outField not in field_names:
        print('outField does NOT exist, adding...')
        new_field = ogr.FieldDefn(outField, ogr.OFTInteger)
        inLayer.CreateField(new_field)
    else:
        print('outField exists, proceeding to populate field...')

    # Loop through the input features
    numFeatures = inLayer.GetFeatureCount()
    cnt = 1
    inFeature = inLayer.GetNextFeature()
    while inFeature is not None:  # While there are still features ...

        key = str(inFeature.GetField(inField))# Get the class as a string
        value = dictionary[key] # Get the corresponding integer value from the dictionary
        print('The class is: ' + str(key))
        print('The class ID is: ' + str(value))
        inFeature.SetField(outField, value) # Set the field to the value
        inLayer.SetFeature(inFeature) # Trigger update

        # Get the next input feature
        inFeature = inLayer.GetNextFeature()

        # Report progress
        print('Feature ' + str(cnt) + ' of ' + str(numFeatures) + ' processed ...')
        cnt += 1

    inDataSource = None

    print('--- Categorical field conversion complete ---')
    print('\n')

# Purpose: convert a shapefile (vector data) to raster
# Parameters: inData = string of shapefile path, shapefile datasource or shapefile layer, field = integer field used to populate raster, rasterPath = path to reference raster, outRasterPath = output path for rasterized data
# Return: nothing, shapefile edited in place
def vectorToRaster(inData, field, rasterPath, outRasterPath):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(inData, str):
        inDataSource = ogr.Open(inData, 1)
        inLayer = inDataSource.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = inData.GetLayer()
    elif str(type(inData)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = inData
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    rasterRef = gdal.Open(rasterPath)

    # Get properties from raster
    ncol = rasterRef.RasterXSize # Columns
    nrow = rasterRef.RasterYSize # Rows
    proj = rasterRef.GetProjectionRef() # Projection
    ext = rasterRef.GetGeoTransform() # Extent

    # Close raster
    rasterRef = None

    # Create temporary raster
    rasterDriver = gdal.GetDriverByName('GTiff')
    outRaster = rasterDriver.Create(outRasterPath, ncol, nrow, 1, gdal.GDT_UInt16) # Parameters: .Create(output filename, number of columns, number of rows, number of bands, raster type) -- GDT_Byte creates raster of unsigned integers

    # Set projection and extent to match reference raster
    outRaster.SetProjection(proj)
    outRaster.SetGeoTransform(ext)

    # Fill the raster with zeros initially
    band = outRaster.GetRasterBand(1)
    band.Fill(0)

    # Rasterize the shapefile layer to our new dataset
    status = gdal.RasterizeLayer(outRaster,  # Output to our new dataset
                                 [1],  # Output to our new dataset's first band
                                 inLayer,  # Rasterize this layer
                                 None, None,  # Dont worry about transformations since we're in same projection
                                 [0],  # Burn value 0
                                 ['ATTRIBUTE=' + field])  # Populate raster values according to the field values
    # Optional parameter: change to ['ALL_TOUCHED=TRUE', 'ATTRIBUTE=' + field]
    # This will rasterize ALL pixels touched by the training point polygon, even if just a sliver of the circle touches the polygon
    # Without this parameter only pixels where a majority of the circle falls within the pixel are counted

    outRaster = None

    # Report statistics
    outRaster = gdal.Open(outRasterPath)
    outArray = outRaster.GetRasterBand(1).ReadAsArray() # Convert raster to array
    classes = numpy.unique(outArray) # Get unique values
    for c in classes: # For each unique value, report how many pixels have this value
        print('Class {c} contains {n} pixels'.format(c=c, n=(outArray == c).sum()))

    # Test success
    if status != 0:
        raise Exception('Conversion to raster failed, try again \n')
    else:
        print('--- Conversion to raster complete ---')
        print('\n')

# RASTER PROCESSING  ----------------------------------------------------------------------

# Purpose: reproject raster to specified spatial reference system
# Parameters: targetRaster = raster to project, rasterRef = raster from which destination srs information will be pulled, dataType = indicate whether output data should be 'integer' or 'float', NoData = specify NoData value if none in reference raster or need to change it
# Return: GDAL dataset object
def reprojectRaster(targetRaster, refRaster):

    # If the function input is a string, open it as dataset
    if isinstance(targetRaster, str):
        ds = gdal.Open(targetRaster) # GDAL syntax
    else:
        ds = targetRaster

    # If the function input is a string, open it as dataset
    if isinstance(refRaster, str):
        dsRef = gdal.Open(refRaster) # GDAL syntax
    else:
        dsRef = refRaster

    print(ds)
    print(dsRef)

    # Get coordinate system of target raster ('from' coordinate system)
    wkt = ds.GetProjection()
    srs = osr.SpatialReference()
    srs.ImportFromWkt(wkt)
    srsName = wkt.split('"')[1]

    # Get coordinate system of reference raster ('to' coordinate system)
    wktRef = dsRef.GetProjection()
    srsRef = osr.SpatialReference()
    srsRef.ImportFromWkt(wktRef)
    srsRefName = wktRef.split('"')[1]

    print('The raster will be reprojected from ' + srsName + ' to ' + srsRefName)

    ds = gdal.Warp('', targetRaster, format = 'MEM', dstSRS = srsRef)

    print('--- Raster reprojected ---')
    print('\n')

    return ds

# Purpose: calculate the minimum extent between two rasters
# Parameters: raster1, raster2
# Return: list of 4 coordinates representing corners of the minimum extent
def calculateRasterIntersection(raster1, raster2):

    # If the function inputs are strings, open them as datasets
    if isinstance(raster1, str):
        ds1 = gdal.Open(raster1) # GDAL syntax
    else:
        ds1 = raster1
    if isinstance(raster2, str):
        ds2 = gdal.Open(raster2) # GDAL syntax
    else:
        ds2 = raster2

    # Make sure rasters are in the same projection
    proj1 = osr.SpatialReference(wkt=ds1.GetProjection())
    epsg1 = proj1.GetAttrValue('AUTHORITY',1)
    proj2 = osr.SpatialReference(wkt=ds2.GetProjection())
    epsg2 = proj2.GetAttrValue('AUTHORITY',1)
    print('The raster 1 EPSG code is: ' + str(epsg1))
    print('The raster 2 EPSG code is: ' + str(epsg2))
    if (epsg1 == epsg2):
        print('Rasters do not require reprojection')
        print('\n')
    else:
        print('Reprojecting raster 2 to match ESPG code of raster 1 ...')
        ds2 = reprojectRaster(ds2, ds1)

    # GDAL affine georeferencing transform contents
    # The affine transform consists of six coefficients returned by GDALDataset::GetGeoTransform() which map pixel/line coordinates into georeferenced space using the following relationship:
    # GT = GDALDataset::GetGeoTransform()
    # Xgeo = GT(0) + Xpixel*GT(1) + Yline*GT(2)
    # Ygeo = GT(3) + Xpixel*GT(4) + Yline*GT(5)
    # e.g. (358485.0, 30.0, 0.0, 4265115.0, 0.0, -30.0)
    # 30 m pixel size, pixel top left corner (0,0), coordinates top left corner (358485.0, 4265115.0)
    # (coordinates start left, pixel width, pixel start left, coordinates start top, pixel start top, pixel height)

    # Get raster 1 geoinformation
    gt1 = ds1.GetGeoTransform() # GDAL syntax

    # [left, top, right, bottom]
    # [xOrigin, yOrigin, xEnd, yEnd]
    r1 = [gt1[0], gt1[3], gt1[0] + (gt1[1] * ds1.RasterXSize), gt1[3] + (gt1[5] * ds1.RasterYSize)] # GDAL syntax

    # Get raster 2 geoinformation
    gt2 = ds2.GetGeoTransform() # GDAL syntax

    # [left, top, right, bottom]
    # [xOrigin, yOrigin, xEnd, yEnd]
    r2 = [gt2[0], gt2[3], gt2[0] + (gt2[1] * ds2.RasterXSize), gt2[3] + (gt2[5] * ds2.RasterYSize)] # GDAL syntax

    # Calculate intersection between the two
    # [left, top, right, bottom]
    intersection = [max(r1[0], r2[0]), min(r1[1], r2[1]), min(r1[2], r2[2]), max(r1[3], r2[3])]

    print('--- Intersection calculated ---')
    print('\n')

    return intersection

# Purpose: clips raster according to a set of 4 coordinates representing corners
# Parameters: coords = list of 4 coordinates representing corners of a box, raster = raster to clip
# Return: GDAL dataset object
def clipRasterWithCoords(coords, raster):

    # If the function input is a string, open it as dataset
    if isinstance(raster, str):
        ds = gdal.Open(raster) # GDAL syntax
    else:
        ds = raster

    # Clip raster
    ds = gdal.Translate('', ds, format = 'MEM', projWin = coords)

    print('--- Raster resized ---')
    print('\n')

    return ds

# Purpose: resample a raster to match the resolution of a reference raster
# Parameters: referenceRaster = raster to use as reference, resampleRaster = raster to be resampled
# Return: GDAL dataset object with resolution of referenceRaster
def resampleRaster(resampleRaster, referenceRaster):

    # If the function inputs are strings, open them as datasets
    if isinstance(referenceRaster, str):
        dsRef = gdal.Open(referenceRaster) # GDAL syntax
    else:
        dsRef = referenceRaster
    if isinstance(resampleRaster, str):
        dsResample = gdal.Open(resampleRaster) # GDAL syntax
    else:
        dsResample = resampleRaster

    gt = dsRef.GetGeoTransform()
    xRes = gt[1]
    yRes = -gt[5]

    dsResample = gdal.Warp('', dsResample, format = 'MEM', xRes=xRes, yRes=yRes)

    print('--- Raster resampled ---')
    print('\n')

    return dsResample

# Purpose: normalize a raster band i.e. array
# Parameters: band = band i.e. array to normalize
# Return: array
def normalizeBand(band):
    band_min, band_max = band.min(), band.max()

    print('--- Band normalized ---')
    print('\n')

    return ((band - band_min)/(band_max - band_min))

# RASTER AGGREGATION  ----------------------------------------------------------------------

# Purpose: take a list of individual rasters and combine them into a raster stack
# Parameters: referenceRaster = raster with all the metadata/properties that you wish all of the raster in the stacks to have, rasterList = list of rasters to stack, dataTypeOut = desired data type for output stack -- choose from data types listed here: https://gdal.org/python/osgeo.gdalconst-module.html, outPath = OPTIONAL parameter denoting the file path location to save the rasters stack -- if this argument is not provided the stack will be saved in memory, Nbands = OPTIONAL parameter denoting how many bands the output stack should have - used for checking stack at end
# Return: GDAL dataset object
def createRasterStack(referenceRaster, rasterList, dataTypeOut, outPath = None, Nbands = None):

    # Open reference raster
    if isinstance(referenceRaster, str):
        dsRef = gdal.Open(referenceRaster)  # GDAL syntax
    else:
        dsRef = referenceRaster

    # Get geo information from reference raster
    gtRef = dsRef.GetGeoTransform()
    projRef = osr.SpatialReference(wkt=dsRef.GetProjection())
    epsgRef = projRef.GetAttrValue('AUTHORITY',1)
    xResRef = gtRef[1]
    yResRef = -gtRef[5]
    print('The reference geotransform is: ' + str(gtRef))
    print('The reference EPSG code is: ' + str(epsgRef))
    print('The reference X resolution is: ' + str(xResRef))
    print('The reference Y resolution is: ' + str(yResRef))
    print('\n')

    # Calculate minimum extent of all rasters in the list
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

    pathList = []

    # Loop through all rasters in the list, clip to minimum extent and align with reference raster (projection, resolution)
    for raster in rasterList:

        rasterName = raster.split('\\')[-1]

        print('CURRENT RASTER: ' + rasterName)
        print('\n')

        # Open raster
        if isinstance(raster, str):
            ds = gdal.Open(raster)  # GDAL syntax
        else:
            ds = raster
        
        # Reproject
        proj = osr.SpatialReference(wkt=ds.GetProjection())
        epsg = proj.GetAttrValue('AUTHORITY',1)
        print('The current raster EPSG code is: ' + str(epsg))
        if (epsg == epsgRef):
            print('Raster does not require reprojection')
            print('\n')
        else:
            ds = reprojectRaster(ds, dsRef)

        # Resample
        gt = ds.GetGeoTransform()
        xRes = gt[1]
        yRes = -gt[5]
        if (xRes == xResRef) and (yRes == yResRef):
            print('Raster does not require resampling')
            print('\n')
        else:
            ds = resampleRaster(ds, dsRef)

        # Clip to minimum extent
        ds = clipRasterWithCoords(intersect, ds)

        # Loop through raster bands
        numBands = ds.RasterCount # Get number of bands in raster
        for b in range(numBands):
            
            # Get single band
            band = ds.GetRasterBand(b+1)

            # Test to make sure band is not RGB alpha band
            # Bands have color interpretations to specify which type of band they are, 6 = RGB alpha band, see here for details: https://github.com/mapbox/rasterio/issues/100
            band.ComputeStatistics(0)
            print('The band minimum is: ' + str(band.GetMinimum()))
            print('The band maximum is: ' + str(band.GetMaximum()))
            print('The band color interpretation is: ' + str(band.GetColorInterpretation()))
            if (band.GetColorInterpretation() == 6):
                print('Alpha band detected, moving on to next band')
                continue

            # Get band data type
            dataType = gdal.GetDataTypeName(band.DataType)
            print('The band data type is: ' + str(dataType))
            print('\n')

            # Get single band and store as dataset
            dsSingle = gdal.Translate('', ds, format='MEM', bandList = [b + 1])

            # Convert to output data type and save
            dsPath = ''.join(raster.split('.')[0:-1]) + '_band' + str(b + 1) + '_temp.' + ''.join(raster.split('.')[-1])
            dsSingle =  gdal.Warp(dsPath, dsSingle, format = 'GTiff', workingType = eval('gdal.GDT_' + dataType), outputType = eval('gdal.GDT_' + dataTypeOut))
            dsSingle.FlushCache()
            dsSingle = None

            # Append band path to final dataset list
            pathList.append(dsPath)
            print('Raster ' + str(dsPath) + ' added to list')
            print('\n')

    print('The list of rasters is: ' + str(pathList))
    print('\n')

    # Stack rasters
    stack = gdal.BuildVRT('', pathList, separate=True)
    print('VRT stack created')
    print('Number of bands in stack: ' + str(stack.RasterCount))
    print('Stack preview:')
    print(stack.ReadAsArray())
    print('\n')

    # Clean up -- remove temporary files
    for path in pathList:
        os.remove(path)
    print('Clean up completed')
    print('\n')

    # Check band number if appropriate
    if Nbands != None:
        if stack.RasterCount != Nbands:
            raise Exception('Number of bands in stack does not match desired number of bands, halting execution ...')

    # Return and/or write stack to disk
    if outPath == None:
        print('--- Raster stack creation complete ---')
        print('\n')
        return stack
    else:
        print('The raster stack will be written to: ' + str(outPath))
        print('\n')
        stack = gdal.Translate(outPath, stack, format = 'GTiff') # Convert VRT to geotiff and write to disk
        stack.FlushCache()
        print('--- Raster stack creation complete ---')
        print('\n')
        return stack

# RASTER/SHAPEFILE ANALYSIS AND SUMMARY  ----------------------------------------------------------------------

# Purpose: take a feature from vector data and get statistics: mean, median, standard deviation, variance
# Parameters: feat = feature from shapefile layer - provided by loopZonalStatistics function, input_zone_polygon = string of shapefile path MUST BE PATH - provided by loopZonalStatistics function,  input_value_raster = raster from which to extract statistics - provided by loopZonalStatistics function
# Return: list of numbers
def zonalStatistics(feat, input_zone_polygon, input_value_raster):

    # Open data
    raster = gdal.Open(input_value_raster)
    inDataSource = ogr.Open(input_zone_polygon)
    inLayer = inDataSource.GetLayer()

    # Get raster georeference info
    transform = raster.GetGeoTransform()
    xLeft_raster = transform[0] # origin
    yTop_raster = transform[3] # origin
    pixelWidth = transform[1]
    pixelHeight = transform[5]
    raster_cols = raster.RasterXSize
    raster_rows = raster.RasterYSize
    xRight_raster= xLeft_raster + (raster_cols * pixelWidth)
    yBottom_raster = yTop_raster + (raster_rows * pixelHeight)

    # Reproject vector geometry to same projection as raster
    sourceSR = inLayer.GetSpatialRef()
    targetSR = osr.SpatialReference()
    targetSR.ImportFromWkt(raster.GetProjectionRef())
    coordTrans = osr.CoordinateTransformation(sourceSR,targetSR)
    feat = inLayer.GetNextFeature()
    geom = feat.GetGeometryRef()
    geom.Transform(coordTrans)

    # Get extent of feature
    geom = feat.GetGeometryRef()
    if (geom.GetGeometryName() == 'MULTIPOLYGON'):
        count = 0
        pointsX = []; pointsY = []
        for polygon in geom:
            geomInner = geom.GetGeometryRef(count)
            ring = geomInner.GetGeometryRef(0)
            numpoints = ring.GetPointCount()
            for p in range(numpoints):
                    lon, lat, z = ring.GetPoint(p)
                    pointsX.append(lon)
                    pointsY.append(lat)
            count += 1
    elif (geom.GetGeometryName() == 'POLYGON'):
        ring = geom.GetGeometryRef(0)
        numpoints = ring.GetPointCount()
        pointsX = []; pointsY = []
        for p in range(numpoints):
                lon, lat, z = ring.GetPoint(p)
                pointsX.append(lon)
                pointsY.append(lat)

    else:
        sys.exit("ERROR: Geometry needs to be either Polygon or Multipolygon")

    # Get extent/bounding box of feat
    xLeft_feat = min(pointsX)
    xRight_feat = max(pointsX)
    yBottom_feat = min(pointsY)
    yTop_feat = max(pointsY)

    # Specify offset and rows and columns to read
    xoff = int((xLeft_feat - xLeft_raster)/pixelWidth)
    yoff = int((yTop_raster - yTop_feat)/pixelWidth)
    xcount = int((xRight_feat - xLeft_feat)/pixelWidth)+1
    ycount = int((yTop_feat - yBottom_feat)/pixelWidth)+1

    # Ensure that current feature overlaps raster
    if xLeft_feat < xLeft_raster or xRight_feat > xRight_raster or yBottom_feat < yBottom_raster or yTop_feat > yTop_raster:
        return False

    # Create memory target raster (feature raster) with boundaries defined by the feature
    target_ds = gdal.GetDriverByName('MEM').Create('', xcount, ycount, 1, gdal.GDT_Byte)
    target_ds.SetGeoTransform((
        xLeft_feat, pixelWidth, 0,
        yTop_feat, 0, pixelHeight,
    ))

    # Create for target raster the same projection as for the value raster
    raster_srs = osr.SpatialReference()
    raster_srs.ImportFromWkt(raster.GetProjectionRef())
    target_ds.SetProjection(raster_srs.ExportToWkt())

    # Rasterize zone polygon to raster
    gdal.RasterizeLayer(target_ds, [1], inLayer, burn_values=[1])

    # Read raster as arrays
    banddataraster = raster.GetRasterBand(1)
    dataraster = banddataraster.ReadAsArray(xoff, yoff, xcount, ycount).astype(numpy.float)

    bandmask = target_ds.GetRasterBand(1)
    datamask = bandmask.ReadAsArray(0, 0, xcount, ycount).astype(numpy.float)

    # Mask zone of raster
    zoneraster = numpy.ma.masked_array(dataraster,  numpy.logical_not(datamask))

    # Calculate statistics of zonal raster
    return numpy.average(zoneraster),numpy.mean(zoneraster),numpy.median(zoneraster),numpy.std(zoneraster),numpy.var(zoneraster)

# Purpose: loop through all the features in a shapefile and provide zonal statistics
# Parameters: input_zone_polygon = string of shapefile path MUST BE PATH, input_value_raster = raster from which to extract statistics
# Return: python dictionary
def loopZonalStatistics(input_zone_polygon, input_value_raster):

    inDataSource = ogr.Open(input_zone_polygon)
    inLayer = inDataSource.GetLayer()
    featList = range(inLayer.GetFeatureCount())
    statDict = {}

    for FID in featList:
        feat = inLayer.GetFeature(FID)
        meanValue = zonalStatistics(feat, input_zone_polygon, input_value_raster)

        # Ensure valid statistics were returned -- if not the current feature is outside the raster area and we should move on
        if meanValue == False:
            # Report progress and advance
            print('The current feature does not overlap the raster, moving on to next feature...')
            continue

        statDict[FID] = meanValue
    return statDict

# Purpose: take a feature from vector data and get statistics: pixel count for each pixel value
# Parameters: feat = feature from shapefile layer - provided by loopZonalStatistics function, input_zone_polygon = string of shapefile path MUST BE PATH - provided by loopZonalStatistics function,  input_value_raster = raster from which to extract statistics - provided by loopZonalStatistics function
# Return: python dictionary
def zonalStatisticsCategorical(feat, input_zone_polygon, input_value_raster):

    # Open raster
    raster = gdal.Open(input_value_raster)

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(input_zone_polygon, str):
        inDataSource = ogr.Open(input_zone_polygon)
        inLayer = inDataSource.GetLayer()
    elif str(type(input_zone_polygon)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = input_zone_polygon.GetLayer()
    elif str(type(input_zone_polygon)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = input_zone_polygon
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    # Get raster georeference info
    transform = raster.GetGeoTransform()
    xLeft_raster = transform[0] # origin
    yTop_raster = transform[3] # origin
    pixelWidth = transform[1]
    pixelHeight = transform[5]
    raster_cols = raster.RasterXSize
    raster_rows = raster.RasterYSize
    xRight_raster= xLeft_raster + (raster_cols * pixelWidth)
    yBottom_raster = yTop_raster + (raster_rows * pixelHeight)

    # Reproject vector geometry to same projection as raster
    sourceSR = inLayer.GetSpatialRef()
    targetSR = osr.SpatialReference()
    targetSR.ImportFromWkt(raster.GetProjectionRef())
    coordTrans = osr.CoordinateTransformation(sourceSR,targetSR)
    geom = feat.GetGeometryRef()
    geom.Transform(coordTrans)

    # Get extent of feat
    geom = feat.GetGeometryRef()
    if (geom.GetGeometryName() == 'MULTIPOLYGON'):
        count = 0
        pointsX = []; pointsY = []
        for polygon in geom:
            geomInner = geom.GetGeometryRef(count)
            ring = geomInner.GetGeometryRef(0)
            numpoints = ring.GetPointCount()
            for p in range(numpoints):
                    lon, lat, z = ring.GetPoint(p)
                    pointsX.append(lon)
                    pointsY.append(lat)
            count += 1
    elif (geom.GetGeometryName() == 'POLYGON'):
        ring = geom.GetGeometryRef(0)
        numpoints = ring.GetPointCount()
        pointsX = []; pointsY = []
        for p in range(numpoints):
                lon, lat, z = ring.GetPoint(p)
                pointsX.append(lon)
                pointsY.append(lat)

    else:
        sys.exit("ERROR: Geometry needs to be either Polygon or Multipolygon")

    # Get extent/bounding box of feat
    xLeft_feat = min(pointsX)
    xRight_feat = max(pointsX)
    yBottom_feat = min(pointsY)
    yTop_feat = max(pointsY)

    # Specify offset and rows and columns to read -- basically defining pixels from the raster that overlap the feature
    xoff = int((xLeft_feat - xLeft_raster)/pixelWidth)
    yoff = int((yTop_raster - yTop_feat)/pixelWidth)
    xcount = int((xRight_feat - xLeft_feat)/pixelWidth)+1
    ycount = int((yTop_feat - yBottom_feat)/pixelWidth)+1

    # Ensure that current feature overlaps raster
    if xLeft_feat < xLeft_raster or xRight_feat > xRight_raster or yBottom_feat < yBottom_raster or yTop_feat > yTop_raster:
        return False

    # Create memory target raster (feature raster) with boundaries defined by the feature
    target_ds = gdal.GetDriverByName('MEM').Create('', xcount, ycount, 1, gdal.GDT_Byte)
    target_ds.SetGeoTransform((
        xLeft_feat, pixelWidth, 0,
        yTop_feat, 0, pixelHeight,
    ))

    # Create for target raster the same projection as for the value raster
    raster_srs = osr.SpatialReference()
    raster_srs.ImportFromWkt(raster.GetProjectionRef())
    target_ds.SetProjection(raster_srs.ExportToWkt())

    # Rasterize zone polygon to raster
    gdal.RasterizeLayer(target_ds, [1], inLayer, burn_values=[1])
    # gdal.RasterizeLayer(target_ds, [1], inLayer, None, None , [1], ['ALL_TOUCHED=TRUE'])
    # Optional parameter: add ['ALL_TOUCHED=TRUE']
    # This will rasterize ALL pixels touched by the training point polygon, even if just a sliver of the circle touches the polygon
    # Without this parameter only pixels where a majority of the circle falls within the pixel are counted

    # Read raster as arrays
    banddataraster = raster.GetRasterBand(1)
    dataraster = banddataraster.ReadAsArray(xoff, yoff, xcount, ycount).astype(numpy.float)

    bandmask = target_ds.GetRasterBand(1)
    datamask = bandmask.ReadAsArray(0, 0, xcount, ycount).astype(numpy.float)

    # Mask zone of raster
    zoneraster = numpy.ma.masked_array(dataraster,  numpy.logical_not(datamask))

    # Calculate statistics of zonal raster
    unique, counts = numpy.unique(zoneraster, return_counts=True)

    target_ds = None

    return dict(zip(list(map(int, unique[:-1])), counts[:-1])) # Exclude the last unique value which denotes masked pixels, convert pixel values to integers

# Purpose: loop through all the features in a shapefile and provide zonal statistics
# Parameters: input_zone_polygon = string of shapefile path MUST BE PATH, input_value_raster = raster from which to extract statistics
# Return: python dictionary
def loopZonalStatisticsCategorical(input_zone_polygon, input_value_raster, field):

    # Open input shapefile -- amount of preprocessing necessary depends on input object type
    if isinstance(input_zone_polygon, str):
        inDataSource = ogr.Open(input_zone_polygon)
        inLayer = inDataSource.GetLayer()
    elif str(type(input_zone_polygon)) == "<class 'osgeo.ogr.DataSource'>":
        inLayer = input_zone_polygon.GetLayer()
    elif str(type(input_zone_polygon)) == "<class 'osgeo.ogr.Layer'>":
        inLayer = input_zone_polygon
    else:
        raise Exception('Unknown input data type -- please specify either a file path, DataSource or Layer type')

    statList = {}

    # Track progress
    numFeatures = inLayer.GetFeatureCount()
    cnt = 0

    print(str(numFeatures) + ' features found')

    # Loop through the input features
    inLayer.ResetReading()
    feat = inLayer.GetNextFeature()
    while feat is not None:  # While there are still features ...

        print('The quadrat is: ' + str(feat.GetField('quadrat')))

        print('Processing feature: ' + str(cnt + 1) + ' out of ' + str(numFeatures))

        meanValue = zonalStatisticsCategorical(feat, input_zone_polygon, input_value_raster) # Perform zonal statistics

        # Ensure valid statistics were returned -- if not the current feature is outside the raster area and we should move on
        if meanValue == False:
            # Report progress and advance
            cnt += 1
            feat = inLayer.GetNextFeature()
            print('The current feature does not overlap the raster, moving on to next feature...')
            continue

        fieldValue = feat.GetField(field) # Grab the value from the field of interest, this is included in the outer dictionary to identify each dictionary of class: pixels
        statList[fieldValue] = meanValue # Add key/value pair to dictionary -- dictionary structure {fieldValue1: {class1: pixels, class2: pixels ...}, fieldValue2: {class1: pixels, class2: pixels ...}, ...}

        # Report progress and advance
        cnt += 1
        feat = inLayer.GetNextFeature()

    return statList

# BASH SCRIPTS  ----------------------------------------------------------------------

# Purpose: write list to file with each list item as one line
# Parameters: input_list = input text list, file_name = output file name, with file extension, rownames = list of row names strings, colnames = list of column name strings, delim = delimiter (default: ", "), append = if the lines should be appended to the file
# Return: write to file
def write_list_to_file(input_list,
                       file_name,
                       rownames=None,
                       colnames=None,
                       delim=", ",
                       append=False):
    """
    Function to write list to file
    with each list item as one line
    :param input_list: input text list
    :param file_name: output file name, with file extension
    :param rownames: list of row names strings
    :param colnames: list of column name strings
    :param delim: delimiter (default: ", ")
    :param append: if the lines should be appended to the file
    :return: write to file
    """
    if len(input_list) == 0:
        raise ValueError('Empty input list supplied')

    elif type(input_list[0]).__name__ in ('list', 'tuple'):
        if rownames is not None:
            if len(rownames) != len(input_list):
                raise ValueError('Row name list does not have sufficient elements')
            else:
                input_list = list([rownames[i]] + elem for i, elem in enumerate(input_list))

        if colnames is not None:
            if len(rownames) != len(input_list[0]):
                raise ValueError('Column name list does not have sufficient elements')
            else:
                input_list = [colnames] + input_list

        input_list = '\n'.join(list(delim.join(list(str(elem) for elem in line)) for line in input_list))

    elif type(input_list[0]).__name__ == 'str':
        input_list = '\n'.join(input_list)
    else:
        raise ValueError('Input list not in types: list of list, list of tuples, list of strings')

    # write to file
    if append:
        open_type = 'a'  # append
    else:
        open_type = 'w'  # write

    with open(file_name, open_type, newline = '\n') as fileptr:
        fileptr.write('{}\n'.format(input_list))

# Purpose: creates a bash command dictionary for common applications
# Parameters: script_name = name of script, language = programming language - choose from 'R' or 'python', *args = OPTIONAL, list any additional variables input to scripts e.g. site name, paths to data
# Return: write to file
def build_slurm_command_dictionary(script_name, language, *args):

    # Use arguments to create string of variables
    sep = ' '
    variables = sep.join(args)

    if language == 'R':
        command_dict = {
        'purge': 'module purge',
        'load': 'module load anaconda3/2020.11',
        'activate': 'conda activate R_geospatial',
        'run': 'Rscript ' + str(script_name) + ' ' + variables
        }
    elif language == 'python':
        command_dict = {
        'purge': 'module purge',
        'load': 'module load anaconda3/2020.11',
        'activate': 'conda activate py37',
        'which': 'which python',
        'run': 'srun python -u ' + str(script_name) + ' ' + variables
        }
    else:
        print('Programming language not recognized, choose either R or python')
        print('\n')

    return command_dict

# Purpose: write slurm script for the given parameters
# Parameters: commands = dictionary of SLURM commands, each item in the dictionary will be a separate line in the BASH script, data_location = path to data and/or scripts, log_location = path where log output should go, file_name = output file name for the BASH script, job_name = Name of the SLURM job, time_in_mins = expected run time in minutes, cpus = Number of CPUs requested, ntasks = Number of tasks, mem = Memory requested (in MB), array = (bool) If using job arrays, iterations = Job array upper limit (e.g. 132 for 1-132 array), kwargs = key word arguments
# Return: write to .sh file
def write_slurm_script(commands,
                       data_location,
                       log_location,
                       file_name = 'pyscript.sh',
                       job_name='pyscript',
                       time_in_mins=60,
                       cpus=1,
                       ntasks=1,
                       mem=2000,
                       array=False,
                       iterations=1):
    """
    Write slurm script for the given parameters
    :param commands: dictionary of SLURM commands, each item in the dictionary will be a separate line in the BASH script
    :data_location: path to data and/or scripts
    :log_location: path where log output should go
    :param file_name: output file name for the BASH script
    :param job_name: Name of the SLURM job
    :param time_in_mins: expected run time in minutes
    :param cpus: Number of CPUs requested
    :param ntasks: Number of tasks
    :param mem: Memory requested (in MB)
    :param array: (bool) If using job arrays
    :param iterations: Job array upper limit (e.g. 132 for 1-132 array)
    :param kwargs: key word arguments
    :return:
    """
    script_dict = {
        'bash': '#!/bin/bash',
        'job-name': '#SBATCH --job-name=' + job_name,
        'time': '#SBATCH --time=' + str(datetime.timedelta(minutes=time_in_mins)),
        'cpus': '#SBATCH --cpus-per-task=' + str(cpus),
        'ntasks': '#SBATCH --ntasks=' + str(ntasks),
        'mem': '#SBATCH --mem=' + str(mem),
        'array_def': '#SBATCH --array=1-' + str(iterations),
        'array_out': '#SBATCH --output=' + log_location + job_name + '_%A_%a.out',
        'out': '#SBATCH --output=' + log_location + job_name + '_%j.out',
        'dir': '#SBATCH --chdir=' + data_location,
        'date': 'date',
        'array_echo': 'echo "Job ID is"$SLURM_ARRAY_TASK_ID'
    }
    if array:
        script_list = [
            script_dict['bash'],
            script_dict['job-name'],
            script_dict['time'],
            script_dict['cpus'],
            script_dict['mem'],
            script_dict['array_def'],
            script_dict['array_out'],
            script_dict['dir'],
            script_dict['date'],
        ]
        for key, value in commands.items():
            script_list.append(value)
        script_list.append(script_dict['date'])
    else:
        script_list = [
            script_dict['bash'],
            script_dict['job-name'],
            script_dict['time'],
            script_dict['cpus'],
            script_dict['mem'],
            script_dict['out'],
            script_dict['dir'],
            script_dict['date'],
        ]
        for key, value in commands.items():
            script_list.append(value)
        script_list.append(script_dict['date'])

    print(script_list)
    write_list_to_file(script_list, file_name)