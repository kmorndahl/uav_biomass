# --------------------------------------------------------------------- #
# ------ AGGREGATE RANDOM FOREST CLASSIFICATION ACCURACY REPORTS ------ #
# --------------------------------------------------------------------- #

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

import os
import sys
import glob
import pandas

# 1. ========== SET UP  ==========

# 1.1 ---------- Directories ----------

###########################################################
# SET OUTPUT DIRECTORY
###########################################################

workingDir = '*/UAV_classification/results'

os.chdir(workingDir) # Sets the working directory

# 2. ========== GATHER FILES  ==========

files = glob.glob('*PFTaccuracyResults_SMOTE.csv')

print('The list of files is:')
print(files)
print('The number of files is: ' + str(len(files)))
print('\n')

# 3. ========== COMBINE DATA  ==========

accuracy_all = pandas.DataFrame()

for file in files:
    accuracy_all = accuracy_all.append(pandas.read_csv(file))

print(accuracy_all)
print('\n') 

accuracy_all.to_csv('ALL_SITES_accuracyResults.csv')

# 4. ========== CALCULATE AVERAGES ==========

accuracy_all = accuracy_all.rename(columns = {'f1-score':'f1score'})
accuracy_summary = accuracy_all.groupby('pft')[['precision', 'recall', 'f1score', 'support']].agg(['mean', 'min', 'max'])
accuracy_summary.columns = [ '_'.join((str(i) for i in reversed(col))) for col in accuracy_summary.columns]
accuracy_summary.reset_index(inplace=True)
accuracy_summary = pandas.wide_to_long(accuracy_summary, stubnames = ['mean', 'min', 'max'], i = 'pft', j = 'metric', sep = '_', suffix='\\w+').reset_index()

print(accuracy_summary)
print('\n') 

accuracy_summary.to_csv('ALL_SITES_SUMMARY_accuracyResults.csv')
