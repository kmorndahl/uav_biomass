######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script gathers the PFT classification accuracy results from each site and aggregates them

######################################################################################################
######################################################################################################

library(dplyr)
library(tidyr)
library(qdapTools)
library(yardstick)
library(ggplot2)
library(ggpmisc)

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = '*/0_UAV_final/data'
outName = 'classification_field_UAV_data.csv'
outNameTidy = 'classification_field_UAV_data_tidy.csv'
outNameFinal = '0_cover_FINAL.csv'
setwd(dir)

fileDir = '*/UAV_classification/results/'

######################################################################################################
######################################################################################################

# 1. AGGREGATE DATA ------------------------------------------------------

# 1.1 GET FILES ------------------------------------------------------

paths = list.files(fileDir, '*_PFTtotal_SMOTE.csv')

# 1.2 AGGREGATE ------------------------------------------------------

coverTotal = data.frame()
for(path in paths){

  PFTdata = read.csv(path, header = T)
  print(head(PFTdata))

  coverTotal = rbind(coverTotal, PFTdata)

}

# 1.3 SAVE TO DISK ------------------------------------------------------

write.csv(coverTotal, outName, row.names = FALSE)

######################################################################################################
######################################################################################################

# 2. TIDY DATA ------------------------------------------------------

# Remove unneeded PFTs
coverTotal = coverTotal[coverTotal$PFT_fine != 'BRYOPHYTES' & coverTotal$PFT_fine != 'TREES' & coverTotal$PFT_fine != 'NON VEGETATED',]
droplevels(coverTotal)

# Reorder levels
coverTotal$PFT_fine = factor(coverTotal$PFT_fine, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS"))

# Rename columns
names(coverTotal) = c('site_code', 'quadrat_num', 'PFT', 'cover_observed', 'cover_predicted')

# Save tidy data
write.csv(coverTotal, outNameTidy, row.names = FALSE)

######################################################################################################
######################################################################################################

# 3. FINALIZE DATA ------------------------------------------------------

# Remove observations with poor data quality

# WICKERSHAM 10m, 30m, 50m, 70m, 90m -- quadrats harvested before UAV flight
# WICKERSHAM 89.5m -- no cover or height estimation done at this quadrat
# PEDRO 70m -- overtopping tall deciduous shrubs not captured in field data, field data does not adequately represent actual conditions

# Set up label
coverTotal$quad_label = paste(coverTotal$site_code, coverTotal$quadrat_num)

coverTotal = coverTotal[(coverTotal$quad_label != 'WICKERSHAM 10m') & 
                        (coverTotal$quad_label != 'WICKERSHAM 30m') & 
                        (coverTotal$quad_label != 'WICKERSHAM 50m') & 
                        (coverTotal$quad_label != 'WICKERSHAM 70m') &
                        (coverTotal$quad_label != 'WICKERSHAM 90m') & 
                        (coverTotal$quad_label != 'WICKERSHAM 89.5m') &
                        (coverTotal$quad_label != 'PEDRO 70m'),]

# Remove quadrat label
coverTotal = subset(coverTotal, select = -c(quad_label))

# Sort
coverTotal = dplyr::arrange(coverTotal, site_code, quadrat_num, PFT)

# Add id column
id = 1:nrow(coverTotal)
coverTotal = cbind(id, coverTotal)

# Save final data
write.csv(coverTotal, outNameFinal, row.names = FALSE)

