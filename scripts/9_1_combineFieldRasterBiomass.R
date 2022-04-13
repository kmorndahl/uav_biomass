######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = '*/0_UAV_final/data'
setwd(dir)

outName = 'biomass_UAV_data.csv'
outNameTidy = 'biomass_field_UAV_data_tidy.csv'
outNameWide ='biomass_field_UAV_data_wide.csv'
outNameFinal = '0_biomass_FINAL.csv'

biomassDir = '*/UAV_biomass/results_NAs_FINAL'

######################################################################################################
######################################################################################################

library(tidyverse)

######################################################################################################
######################################################################################################

# 1. AGGREGATE DATA------------------------------------------------------

# 1.1 GET FILES ------------------------------------------------------

paths = list.files(biomassDir, '*_UAV_biomass.csv')

# 1.2 AGGREGATE ------------------------------------------------------

biomassTotal = data.frame()
for(path in paths){
  
  PFTdata = read.csv(path, header = T)
  print(head(PFTdata))
  
  biomassTotal = rbind(biomassTotal, PFTdata)
  
}

biomassTotal = subset(biomassTotal, select = -c(X))

# 1.3 SAVE ------------------------------------------------------

write.csv(biomassTotal, outName)

######################################################################################################
######################################################################################################

# 1. READ IN DATA ------------------------------

# Read in field data
biomassField = read.csv('*/0_UAV_final/data/biomass_field_data.csv')
coverData = read.csv('*/0_UAV_final/data/classification_field_UAV_data_tidy.csv')

# Read in raster data
biomassUAV = read.csv('*/0_UAV_final/data/biomass_UAV_data.csv')

######################################################################################################
######################################################################################################

# 2. TIDY DATA ------------------------------------------------------

# 2.1 SUMMARISE FIELD DATA ------------------------------------------------------

# Summarize and gather biomass data
biomassField = biomassField %>% dplyr::group_by(site_code, quadrat_num, PFT_fine) %>% dplyr::summarise(weight = sum(weight))

######################################################################################################
######################################################################################################

# 3. ADD TOTAL CLASS ------------------------------------------------------

# 3.1 FIELD DATA ------------------------------------------------------

# Remove trees
biomassField = biomassField %>% dplyr::filter(PFT_fine != 'TREES')

# Create dataset for aggregating shrubs, checking PFTs
biomassCheckPFTs = biomassField

# Aggregate deciduous and evergreen shrubs to just 'shrubs'
biomassCheckPFTs$PFT_fine = gsub('DECIDUOUS SHRUBS', 'SHRUBS', biomassCheckPFTs$PFT_fine)
biomassCheckPFTs$PFT_fine = gsub('EVERGREEN SHRUBS', 'SHRUBS', biomassCheckPFTs$PFT_fine)

# Group by PFT and add weight
biomassCheckPFTs = biomassCheckPFTs %>% dplyr::group_by(site_code, quadrat_num, PFT_fine) %>% dplyr::summarise(weight = sum(weight)) 

# Count the number of PFTs present in each site/quadrat
biomassCheckPFTs_count = biomassCheckPFTs %>% dplyr::count(site_code, quadrat_num)

# Join counts to original database
biomassCheckPFTs = dplyr::left_join(biomassField, biomassCheckPFTs_count, by = c('site_code', 'quadrat_num'))

# Only select data where all PFTs are present
# When not all PFTs present, missing biomass bag presumed and we do not want a total for this site/quadrat
biomassCheckPFTs = biomassCheckPFTs[biomassCheckPFTs$n >=4,]

# Duplicate data, denote PFT = TOTAL for all
totalBiomass = biomassCheckPFTs
totalBiomass$PFT_fine = 'TOTAL'

# Sum total biomass
totalBiomass = totalBiomass %>% 
  dplyr::group_by(site_code, quadrat_num, PFT_fine) %>%
  dplyr::summarise(weight = sum(weight))

# Add TOTAL data to original data
biomassField = rbind(dplyr::select(biomassField, c(names(totalBiomass))), totalBiomass)

# 3.2 UAV DATA ------------------------------------------------------

# Remove unwanted PFTs, columns
biomassUAV = biomassUAV %>% dplyr::filter(PFT != 'BRYOPHYTES' & PFT != 'NON VEGETATED' & PFT != 'SHADOW'  & PFT != 'TREES')

# Duplicate data, denote PFT = TOTAL for all
totalBiomass = biomassUAV
totalBiomass$PFT = 'TOTAL'

# Sum total biomass
totalBiomass = totalBiomass %>% 
  dplyr::group_by(site_code, quadrat_num, PFT, metric) %>%
  dplyr::summarise(weight = sum(weight))

# Add TOTAL data to original data
biomassUAV = rbind(biomassUAV, totalBiomass)

######################################################################################################
######################################################################################################

# 4. JOIN DATA ------------------------------------------------------

# Pare down field biomass data to include only relevant sites
sites = unique(biomassUAV$site_code)
biomassField = biomassField[biomassField$site_code %in% sites,]

# Left join raster biomass data to cleaned field biomass data
compareBiomass = data.frame(dplyr::left_join(biomassField, biomassUAV, by = c("site_code", "quadrat_num", "PFT_fine" = "PFT")))
compareBiomass = compareBiomass %>% dplyr::rename(weight_observed = weight.x, weight_predicted = weight.y, PFT = PFT_fine)

# Left join cover data to cleaned field biomass data
compareBiomass = dplyr::left_join(compareBiomass, coverData, by= c("PFT", "site_code", "quadrat_num"))

# Cover fields will have NA for TOTAL biomass, fill with 'N/A' so they are not altered when working with NA values later
compareBiomass[is.na(compareBiomass$cover_observed),]$cover_observed = 'N/A'
compareBiomass[is.na(compareBiomass$cover_predicted),]$cover_predicted = 'N/A'

######################################################################################################
######################################################################################################

# 5. TIDY JOINED DATA ------------------------------------------------------

# Convert to factors
compareBiomass$site_code = as.factor(compareBiomass$site_code)
compareBiomass$quadrat_num = as.factor(compareBiomass$quadrat_num)
compareBiomass$PFT = as.factor(compareBiomass$PFT)

# Where there are NAs, convert to zero
compareBiomass$weight_observed[!is.finite(compareBiomass$weight_observed)] = 0
compareBiomass$weight_predicted[!is.finite(compareBiomass$weight_predicted)] = 0

# If biomass is less than zero, convert to zero
compareBiomass[compareBiomass < 0] = 0

######################################################################################################
######################################################################################################

# 6. HANDLE COARSE SHRUBS CLASS ------------------------------------------------------

# If there is a general SHRUBS class for a particular quadrat, we want to discard any other shrub type entries from that quadrat i.e. remove any DECIDUOUS SHRUBS or EVERGREEN SHRUBS entries from the same quadrat
# We do not know what the DECIDUOUS/EVERGREEN breakdown is for the unallocated SHRUBS class so the individual DECIDUOUS SHRUBS and EVERGREEN SHRUBS records are incomplete and should not be included

# Set up site/quadrat labels
compareBiomass$quad_label = paste(compareBiomass$site_code, compareBiomass$quadrat_num)

# Get all quadrats that have a SHRUBS class
shrub_data = compareBiomass[compareBiomass$PFT == 'SHRUBS',]

# From main dataframe, exclude any DECIDUOUS SHRU or EVERGREEN SHRUBS records where the quadrat also has a SHRUBS record
compareBiomass = compareBiomass[ ! (compareBiomass$quad_label %in% shrub_data$quad_label &  compareBiomass$PFT == 'EVERGREEN SHRUBS'),]
compareBiomass = compareBiomass[ ! (compareBiomass$quad_label %in% shrub_data$quad_label &  compareBiomass$PFT == 'DECIDUOUS SHRUBS'),]

# Remove SHRUBS class itself
compareBiomass = compareBiomass[compareBiomass$PFT != 'SHRUBS',]

######################################################################################################
######################################################################################################

# 7. HANDLE MISSING METRICS ------------------------------------------------------

# 2.4 HANDLE ZERO PREDICTIONS -- METRIC
# Deal with occurrences where some weight was observed but zero was predicted, have NAs for metric type

# Get all instances where metric is NA and make a data frame for each metric type
NAs_fit = compareBiomass[is.na(compareBiomass$metric),]
NAs_lwr = compareBiomass[is.na(compareBiomass$metric),]
NAs_upr = compareBiomass[is.na(compareBiomass$metric),]
NAs_ci = compareBiomass[is.na(compareBiomass$metric),]

# Fill in metric type
NAs_fit$metric[is.na(NAs_fit$metric)] = 'fit'
NAs_lwr$metric[is.na(NAs_lwr$metric)] = 'lwr'
NAs_upr$metric[is.na(NAs_upr$metric)] = 'upr'
NAs_ci$metric[is.na(NAs_ci$metric)] = 'ci'

# Remove instances where metric is NA
compareBiomass = compareBiomass[!is.na(compareBiomass$metric),]

# Add all of the metric type data frames
compareBiomass = rbind(compareBiomass, NAs_fit, NAs_lwr, NAs_upr, NAs_ci)

# Remove any lingering observations where metric is NA (should not be any)
compareBiomass = compareBiomass[!is.na(compareBiomass$metric),]

######################################################################################################
######################################################################################################

# 8. SAVE TIDY DATA ------------------------------------------------------

write.csv(compareBiomass, outNameTidy, row.names = FALSE)

######################################################################################################
######################################################################################################

# 9. SPREAD DATA ------------------------------------------------------

# 9.1 SPREAD ------------------------------------------------------

compareBiomass_wide = compareBiomass %>% tidyr::spread(metric, weight_predicted)
compareBiomass_wide = compareBiomass_wide %>% dplyr::rename(weight_predicted = fit)
compareBiomass_wide = compareBiomass_wide %>% dplyr::rename(weight_predicted_lwr = lwr)
compareBiomass_wide = compareBiomass_wide %>% dplyr::rename(weight_predicted_upr = upr)
compareBiomass_wide = compareBiomass_wide %>% dplyr::rename(weight_predicted_ci = ci)

# 9.2 FILL NAs WITH ZEROS
# If there is a very small amount of cover for a particular PFT within a particular quadrat, small differences amongst the metric rasters (fit, ci, lwr, upr) might mean that when classification raster is resampled to appropriate resolution, some of the metric rasters retain the rare PFT while others do not

compareBiomass_wide$weight_predicted_ci[!is.finite(compareBiomass_wide$weight_predicted_ci)] = 0
compareBiomass_wide$weight_predicted[!is.finite(compareBiomass_wide$weight_predicted)] = 0
compareBiomass_wide$weight_predicted_lwr[!is.finite(compareBiomass_wide$weight_predicted_lwr)] = 0
compareBiomass_wide$weight_predicted_upr[!is.finite(compareBiomass_wide$weight_predicted_upr)] = 0

######################################################################################################
######################################################################################################

# 10. SAVE WIDE DATA ------------------------------------------------------

write.csv(compareBiomass_wide, outNameWide, row.names = FALSE)

######################################################################################################
######################################################################################################

# 11. FINALIZE DATA ------------------------------------------------------

# 11.1 REMOVE OBSERVATIONS WITH POOR DATA QUALITY ------------------------------------------------------

# WICKERSHAM 10m, 30m, 50m, 70m, 90m -- biomass harvest done BEFORE drone flights
# GULCH 10m FORBS -- cover estimate very poor
# TWELVEMILE 50m LICHENS -- multi-bag sample and suspect missing bag based on comparison to cover/biomass from other quadrats

compareBiomass_wide = compareBiomass_wide[(compareBiomass_wide$quad_label != 'WICKERSHAM 10m') & 
                                          (compareBiomass_wide$quad_label != 'WICKERSHAM 30m') & 
                                          (compareBiomass_wide$quad_label != 'WICKERSHAM 50m') & 
                                          (compareBiomass_wide$quad_label != 'WICKERSHAM 70m') & 
                                          (compareBiomass_wide$quad_label != 'WICKERSHAM 90m') &
                                          !(compareBiomass_wide$quad_label == 'GULCH 10m' & compareBiomass_wide$PFT == 'FORBS') &
                                          !(compareBiomass_wide$quad_label == 'TWELVEMILE 50m' & compareBiomass_wide$PFT == 'LICHENS'),]

# 4.3 REMOVE ERRONEOUS OBSERVATIONS ------------------------------

# These are instances where there is no biomass harvest even though there is cover reported -- likely lost bag for biomass
compareBiomass_wide = compareBiomass_wide[!(compareBiomass_wide$weight_observed == 0 & compareBiomass_wide$cover_observed >= 1),]

# 4.4 GATHER ------------------------------

compareBiomass_wide = dplyr::rename(compareBiomass_wide, weight_predicted_fit = weight_predicted)
compareBiomass_wide = tidyr::gather(compareBiomass_wide, "metric", "weight_predicted", weight_predicted_ci, weight_predicted_fit, weight_predicted_lwr, weight_predicted_upr)
compareBiomass_wide$metric = gsub('weight_predicted_', '', compareBiomass_wide$metric)

# 4.5 SAVE ------------------------------

write.csv(compareBiomass_wide, outNameFinal, row.names = FALSE)
