# -------------------------------------- #
# ------ COMPRESSES RASTER STACKS ------ #
# -------------------------------------- #

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
from osgeo import gdal

# 1. ---------- SET PARAMTERS ----------

# https://kokoalberti.com/articles/geotiff-compression-optimization-guide/

indir = '*/UAV_classification/stacks/'
outdir = '*/UAV_classification/stacks_compressed/'
translateoptions = gdal.TranslateOptions(gdal.ParseCommandLine("-of Gtiff -co COMPRESS=ZSTD -co BIGTIFF=YES -co PREDICTOR=3")) # gdal.ParseCommandLIne(-of {output file type} -co COMPRESS {compression algorithm} -co BIGTIFF {flag as large tif, prevent tif file size error} -co PREDICTOR {predictor setting})

tifList= []

# Get all tifs
for subdir, dirs, files in os.walk(indir):
    for file in files:
        if (file.endswith('.tif')):  # Grab only tifs
            tifList.append(os.path.join(subdir, file))

print('The list of files to compress is: ' + str(tifList))

# Compress each tif
for tif in tifList:
    raster = gdal.Open(tif)
    file = tif.split('/')[-1]
    out_path = os.path.join(outdir, file)
    print('The compressed raster stack will be written to ' + str(out_path))
    gdal.Translate(out_path, raster, options=translateoptions)
