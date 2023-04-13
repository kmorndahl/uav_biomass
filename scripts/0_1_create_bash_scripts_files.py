# ---------------------------------------------------------------------------- #
# ------ CREATE BASH SCRIPTS THAT REFERENCE UAV FILES ------------------------ #
# ---------------------------------------------------------------------------- #

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import sys
import os
from UAV_fxns import *

# 1. ---------- SET PARAMETERS ----------

stackDir = '*/UAV_classification/stacks' # Directory where UAV raster stacks are located
modelDir = '*/UAV_classification/results' # Directory where UAV classification models are located
classifiedDir = '*/UAV_classification/results' # Directory where final UAV classifications are located
lasDir = '*/UAV_chm/point_clouds' # Directory where UAV point clouds are located

script_name = '5_create_CHMs.R' # Specify script name (including extension), to include in bash script
language = 'R' # Specify programing language for bash script

script_location = '*/0_UAV_final/scripts' # Path where script to run resides, to include in bash script
log_location= '*/UAV_chm/log/' # Path of location to store output logs, to include in bash script

bash_suffix = '_chm.sh' # Suffix for bash script including file extension, will be added to site name i.e. site + bash_suffix
job_suffix = '_chm' # Suffix for slurm job name, will be added to site name i.e. site + job_suffix

run_time = 480 # Time, in minutes, to allocate for job
cpus = 1 # Number of cpus to assign for job
n_tasks = 1 # Number of tasks to assign for job
mem = 100000 # Amount of memory, in MB, to allocate for job

sites = []

# 2. ---------- GET LIST OF RASTER STACKS ----------

stackList = []

for subdir, dirs, files in os.walk(stackDir):
    for file in files:
        if ((file.endswith('.tif')) and ('Stack' in file)):  # Grab only tifs
            stackList.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

stackList.sort()  # Sort the list
print('The list of raster stacks is: ' + str(stackList))

# 3. ---------- GET LIST OF MODELS ----------

modelList = []

for subdir, dirs, files in os.walk(modelDir):
    for file in files:
        if (file.endswith('.pkl')):  # Grab only pickle files
            modelList.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

modelList.sort()  # Sort the list
print('The list of models is: ' + str(modelList))

# 4. ---------- GET LIST OF CLASSIFIED RASTERS ----------

classifiedList = []

for subdir, dirs, files in os.walk(classifiedDir):
    for file in files:
        if (file.endswith('_classified_final.tif')):  # Grab only tifs
            classifiedList.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

classifiedList.sort()  # Sort the list
print('The list of classified rasters is: ' + str(classifiedList))

# 5. ---------- GET LIST OF POINT CLOUDS ----------

lasList = []

for subdir, dirs, files in os.walk(lasDir):
    for file in files:
        if (file.endswith('.las')):  # Grab only .las files
            lasList.append(os.path.abspath(os.path.join(subdir, file)))  # Add them to the list

lasList.sort()  # Sort the list
print('The list of point clouds is: ' + str(lasList))

# 6. ---------- CONFIRM AGREEMENT BETWEEN STACKS AND MODELS ----------

if(len(stackList)!=len(modelList)):
    raise Exception('Different number of multispectral raster stacks and random forest models. Confirm each site has both raster stack and model and try again')

# 7. ---------- CONFIRM AGREEMENT BETWEEN CLASSIFIED RASTERS AND POINT CLOUDS ----------

if(len(classifiedList)!=len(lasList)):
    raise Exception('Different number of multispectral raster stacks and random forest models. Confirm each site has both raster stack and model and try again')

# 8. ---------- FORMAT PATHS FOR USE ON MONSOON ----------

stackList = [path.replace(os.sep, '/').replace('//minim.hpc.nau.edu', '') for path in stackList]
modelList = [path.replace(os.sep, '/').replace('//minim.hpc.nau.edu', '') for path in modelList]
classifiedList = [path.replace(os.sep, '/').replace('//minim.hpc.nau.edu', '') for path in classifiedList]
lasList = [path.replace(os.sep, '/').replace('//minim.hpc.nau.edu', '') for path in lasList]

print('--- Paths formatted ---')

# 9. ---------- CREATE BASH SCRIPTS ----------

index = 0

list1 = lasList
list2 = classifiedList

for path1 in list1:

    # Grab corresponding path from list 2
    path2 = list2[index]

    # Confirm that paths match
    path1Site = path1.split('/')[-1].split('_')[0]
    path2Site = path2.split('/')[-1].split('_')[0]
    if(path1Site != path2Site):
        raise Exception('Mismatch between paths. Check path lists and try again')

    # Write currrent stack and model arguments to a bash script
    command_dict = build_slurm_command_dictionary(script_name, language, path1, path2)

    # write_slurm_script(commands, script location, log location, bash file name, job name, time in minutes, cpus, number of tasks, memory in MB, array, iterations)
    write_slurm_script(command_dict, script_location, log_location, path1Site + bash_suffix, path1Site + job_suffix, run_time, cpus, n_tasks, mem)

    # Increment
    index+=1
