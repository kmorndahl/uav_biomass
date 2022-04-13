# ------------------------------------------------------------- #
# ------ CREATES RASTER STACKS OF ALL UAV PREDICTOR DATA ------ #
# ------------------------------------------------------------- #

import os
import sys
from UAV_fxns import *
parsers = __import__("0_createParsers")

'''
usage: python 1_2_createRasterStack.py site
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

    # 1. ---------- SET PARAMTERS ----------

    inDir = '*/UAV_classification/imagery/' 
    chmDir = '*/UAV_chm/results_base/'
    outDir = '*/UAV_classification/stacks/'

    # Specify desired order for bands -- numbers reference position when sorted alphabetically
    listOrder = [8, 1, 3, 4, 5, 6, 7, 0, 2] # [rgb, ndvi, blue, green, nir, red edge, red, chm, texture]
    tifList = []

    # Band order: rgb, ndvi, blue, green, nir, red edge, red, chm, texture

    # 2. ---------- PREPARE LISTS ----------

    # Get all tifs
    for subdir, dirs, files in os.walk(inDir):
        for file in files:
            if (file.endswith('.tif') and not file.endswith('_chm.tif') and not file.endswith('_chm_focalMean.tif') and site in file):  # Grab only tifs, exclude CHMs -- these are the raw (i.e. bad) CHMs from Pix4D
                tifList.append(os.path.join(subdir, file))

    # Get all CHMs
    for subdir, dirs, files in os.walk(chmDir):
        for file in files:
            if (file.endswith('.tif') and site in file):  # Grab all base CHMs
                tifList.append(os.path.join(subdir, file))

    # Sort tifs alphabetically
    tifList.sort()
    print('--- list grouped ---')
    print(tifList)
    print('\n')

    # Order the list of tifs
    tifList = [tifList[i] for i in listOrder]
    print('The final list of tifs is: ' + str(tifList))
    print('\n')

    # 3. ---------- EXECUTE STACK ----------

    site = tifList[0].split('/')[-1].split('_')[0]
    print('The site is: ' + site)
    print('\n')

    print('The list of rasters to stack is: ' + str(tifList))
    print('\n')
    # Band order: rgb, ndvi, blue, green, nir, red edge, red, chm, texture
    stackName = str(site + '_RGBmsStack.tif')
    stackPath = str(os.path.join(outDir, stackName))
    createRasterStack(tifList[0], tifList, 'Float32', outPath = stackPath, Nbands = 11)

if __name__ == '__main__':

    args = parsers.createRasterStackParser().parser.parse_args()

    main(args.site)