import sys
import os
import numpy
import joblib
from UAV_fxns import *
parsers = __import__("0_createParsers")

'''
Script to apply saved Random Forest model to raster stack to create classified raster
usage: python 3_0_classify.py raster_infile model_infile
positional arguments:
  raster_infile         input raster stack file name

  model_infile          pickle file with saved model
'''

def main(raster_path,
         model_path):

    """
    Main function to run raster classification
    :param raster_path: Raster stack filename with full path
    :param model_path: Model filename with full path
    """
    # 1. ========== PARAMETERS ==========

    # Set output directory
    outDir = '*/UAV_classification/results/'

    # 2. ========== DATA PREPARATION ==========

    # 2.1 ---------- Load data ----------

    # Confirm that stack and model match
    stackSite = raster_path.split('/')[-1].split('_')[0]
    modelSite = model_path.split('/')[-1].split('_')[0]
    if(stackSite != modelSite):
        raise Exception('Mismatch between raster stack and model. Check stack and model lists and try again')

    print('The current site is: ' + stackSite)
    print('The raster stack is: ' + str(raster_path))
    print('The model is: ' + str(model_path))
    print('\n')

    # Load model
    model = joblib.load(model_path)

    # 2.s ---------- Tidy data ----------

    # Open predictor stack and convert multiband raster to numpy array
    stackArray = RasterToArray(raster_path)
    print('--- Raster stack ingested and converted to array ---')
    print(stackArray)
    print('Raster stack shape: ' + str(stackArray.shape)) # Should be (bands, X, Y)
    print('\n')

    # Convert any lingering NaN values to NoData value of -10000
    stackArray[numpy.isnan(stackArray)] = -10000 # Replace NaN, NaN values not allowed in sklearn functions
    print('Raster stack shape: ' + str(stackArray.shape)) # Should be (bands, X, Y)
    print('\n')

    # Take our raster and reshape into long 2d array (nrow * ncol, nband) for classification
    longDataFormat = (stackArray.shape[0] * stackArray.shape[1], stackArray.shape[2])
    print('Dimensions to reshape to: ', str(longDataFormat))
    print('\n')
    stackArrayToClassify = stackArray[:, :, :].reshape(longDataFormat)
    print('--- Array reshaped ---')
    print('Final raster stack shape: ' + str(stackArrayToClassify.shape)) # Should be (bands, X, Y)
    print('\n')

    # 3. ========== PREDICT ==========

    # Predict for each pixel
    stackArrayClassified = model.predict(stackArrayToClassify)
    print('--- Predictions made ---')
    print('\n')

    # Reshape our classification map
    stackArrayClassified = stackArrayClassified.reshape(stackArray[:, :, 0].shape)
    print('--- Array returned to original shape ---')
    print('\n')

    # Convert predicted array back to raster
    classifiedName = str(stackSite + '_classified.tif')
    classifiedPath = str(os.path.join(outDir, classifiedName))
    ArrayToRaster(raster_path, stackArrayClassified, classifiedPath, 'integer')

if __name__ == '__main__':

    args = parsers.classifyParser().parser.parse_args()

    main(args.raster_infile,
         args.model_infile)