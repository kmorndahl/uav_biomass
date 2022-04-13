# ---------------------------------------------------------------------------- #
# ------ CREATE BASH SCRIPTS THAT REFERENCE UAV SITES ------------------------ #
# ---------------------------------------------------------------------------- #

import sys
import os
from UAV_fxns import *

# 1. ---------- SET PARAMETERS ----------

siteDir = '*/UAV_chm/results' # Path to UAV raster products that can be used to grab site names

file_suffix = '_vol_cm.tif' # Suffix used to identify valid rasters within siteDir, with file extension

script_name = '8_1_predictBiomass.R' # Specify script name (including extension), to include in bash script
language = 'R' # Specify programing language for bash script

script_location = '*/0_UAV_final/scripts' # Path where script to run resides, to include in bash script
log_location= '*/UAV_biomass/log/' # Path of location to store output logs, to include in bash script

bash_suffix = '_biomass.sh' # Suffix for bash script including file extension, will be added to site name i.e. site + bash_suffix
job_suffix = '_biomass' # Suffix for slurm job name, will be added to site name i.e. site + job_suffix

run_time = 60 # Time, in minutes, to allocate for job
cpus = 1 # Number of cpus to assign for job
n_tasks = 1 # Number of tasks to assign for job
mem = 40000 # Amount of memory, in MB, to allocate for job

siteList = []

# 2. ---------- GET LIST OF SITES ----------

for subdir, dirs, files in os.walk(siteDir):
    for file in files:
        if (file.endswith(file_suffix)):  # Grab only files that end with file_suffix
            siteList.append(file.split('_')[0])

siteList.sort()  # Sort the list
print('The list of sites is: ' + str(siteList))

# 3. ---------- CREATE BASH SCRIPTS ----------

for site in siteList:

    # Write currrent stack and model arguments to a bash script
    command_dict = build_slurm_command_dictionary(script_name, language, site)

    # write_slurm_script(commands, script location, log location, bash file name, job name, time in minutes, cpus, number of tasks, memory in MB, array, iterations)
    write_slurm_script(command_dict, script_location, log_location, site + bash_suffix, site + job_suffix, run_time, cpus, n_tasks, mem)
