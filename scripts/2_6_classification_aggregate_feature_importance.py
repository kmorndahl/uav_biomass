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

files = glob.glob('*predictorImportance_SMOTE.csv')

print('The list of files is:')
print(files)
print('The number of files is: ' + str(len(files)))
print('\n')

# 3. ========== COMBINE DATA  ==========

predictorImportance_all = pandas.DataFrame()

for file in files:
    predictorImportance_all = predictorImportance_all.append(pandas.read_csv(file))

print(predictorImportance_all)
print('\n') 

predictorImportance_all.to_csv('ALL_SITES_predictorImportance.csv')

# 4. ========== CALCULATE AVERAGES ==========

predictorImportance_summary = predictorImportance_all.groupby('predictor_name')['predictor_importance_value'].agg(['mean', 'min', 'max']).reset_index().sort_values(by=['mean'], ascending = False)

print(predictorImportance_summary)
print('\n') 

predictorImportance_summary.to_csv('ALL_SITES_SUMMARY_predictorImportance.csv')
