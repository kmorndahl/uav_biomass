######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = "*/0_UAV_final/data"
setwd(dir)

outNameTidy = 'height_field_UAV_data_tidy.csv'
outNameFinal = '0_height_FINAL.csv'

######################################################################################################
######################################################################################################

library(dplyr)
library(tidyr)

######################################################################################################
######################################################################################################

# 1. READ IN DATA------------------------------

# Read in field data
heightField = read.csv('height_field_data.csv', header=T)
coverData = read.csv('classification_field_UAV_data_tidy.csv')

# Read in UAV height (CHM) data
heightUAV = read.csv('height_UAV_data.csv')

######################################################################################################
######################################################################################################

# 2. TIDY DATA ------------------------------

# 2.1 TIDY AND SUMMARISE FIELD DATA ------------------------------

# Group height field data PFTs to proper categories
heightField$PFT_fine[heightField$PFT_fine == 'CLUBMOSSES'] = 'FORBS'
heightField$PFT_fine[heightField$PFT_fine == 'HORSETAILS'] = 'FORBS'
heightField$PFT_fine[heightField$PFT_fine == 'GRASSES'] = 'GRAMINOIDS'
heightField$PFT_fine[heightField$PFT_fine == 'RUSHES'] = 'GRAMINOIDS'
heightField$PFT_fine[heightField$PFT_fine == 'SEDGES'] = 'GRAMINOIDS'

# Remove unneeded PFTs
heightField = heightField[heightField$PFT_fine != 'TOTAL' & heightField$PFT_fine != 'TUSSOCK' & heightField$PFT_fine != 'LITTER' & heightField$PFT_fine != 'TREES' & heightField$PFT_fine != 'SHRUBS' & heightField$PFT_fine != 'MUSHROOM' & heightField$PFT_fine != 'BRYOPHYTES',]
droplevels(heightField)

# Summarize and gather height data
heightField = heightField %>% dplyr::group_by(site_code, quadrat_num, PFT_fine) %>% dplyr::summarise(mean = mean(height), max = max(height))
heightField = heightField %>% tidyr::gather(key = "summaryType", value = "height", mean:max)

# 2.2 TIDY UAV FIELD DATA ------------------------------

# Remove unneeded PFTs
heightUAV = heightUAV[heightUAV$PFT != 'BRYOPHYTES' & heightUAV$PFT != 'TREES' & heightUAV$PFT != 'NON VEGETATED' & heightUAV$PFT != 'SHADOW',]
droplevels(heightUAV)

# 2.3 JOIN FIELD AND UAV DATA ------------------------------

# Join height field and UAV data
compareHeight = dplyr::full_join(heightUAV, heightField, by = c("site_code", "quadrat_num", "PFT" = "PFT_fine", "summaryType"), suffix = c("_predicted", "_observed"))

# Join cover field and UAV data
compareHeight = dplyr::left_join(compareHeight, coverData, by= c("PFT", "site_code", "quadrat_num"))

# 2.4 TIDY JOINED DATA ------------------------------

# Remove observations where cover is NA
# These are observations from sites with poor data quality that are excluded from modeling
compareHeight = compareHeight[!is.na(compareHeight$cover_observed),]

# Assign categorical columns as factors
compareHeight$site_code = as.factor(compareHeight$site_code)
compareHeight$quadrat_num = as.factor(compareHeight$quadrat_num)
compareHeight$PFT = as.factor(compareHeight$PFT)
compareHeight$summaryType = as.factor(compareHeight$summaryType)

# Where there are NAs for height values, assign zero height
compareHeight$height_observed[is.na(compareHeight$height_observed)] = 0
compareHeight$height_predicted[is.na(compareHeight$height_predicted)] = 0

# Convert predicted heights to cm
compareHeight$height_predicted = compareHeight$height_predicted * 100

######################################################################################################
######################################################################################################

# 3. ADD TOTAL HEIGHT CLASS

# 3.1 CREATE TOTAL HEIGHT CLASS

# Duplicate data, denote PFT = TOTAL for all
totalHeight = compareHeight
totalHeight$PFT = 'TOTAL'

# Split into mean/max
totalHeight_mean = totalHeight[totalHeight$summaryType == 'mean',]
totalHeight_max = totalHeight[totalHeight$summaryType == 'max',]

# Summarise mean
totalHeight_mean = data.frame(totalHeight_mean %>% 
                        group_by(site_code, quadrat_num, PFT, summaryType) %>% 
                        summarise(height_predicted = mean(height_predicted), 
                                  height_observed = mean(height_observed),
                                  cover_observed = sum(cover_observed),
                                  cover_predicted = sum(cover_predicted)))

# Summarise max
totalHeight_max = data.frame(totalHeight_max %>% 
                                group_by(site_code, quadrat_num, PFT, summaryType) %>% 
                                summarise(height_predicted = max(height_predicted), 
                                          height_observed = max(height_observed),
                                          cover_observed = sum(cover_observed),
                                          cover_predicted = sum(cover_predicted)))

# Combine TOTAL mean and max
totalHeight = rbind(totalHeight_mean, totalHeight_max)

# 3.2 ADD TOTALS TO ORIGINAL DATA

allHeight = rbind(compareHeight, totalHeight)

######################################################################################################
######################################################################################################

# 4. SAVE TIDY HEIGHT DATA

write.csv(allHeight, outNameTidy, row.names = FALSE)

######################################################################################################
######################################################################################################

# 5. FINALIZE DATA ------------------------------------------------------

# 5.1 REMOVE OBSERVATIONS WITH POOR DATA QUALITY ------------------------------------------------------

# WICKERSHAM 10m, 30m, 50m, 70m, 90m -- quadrats harvested before UAV flight
# WICKERSHAM 89.5m -- no cover or height estimation done at this quadrat
# PEDRO 70m -- overtopping tall deciduous shrubs not captured in field data, field data does not adequately represent actual conditions

# Set up label
allHeight$quad_label = paste(allHeight$site_code, allHeight$quadrat_num)

allHeight = allHeight[(allHeight$quad_label != 'WICKERSHAM 10m') & 
                      (allHeight$quad_label != 'WICKERSHAM 30m') & 
                      (allHeight$quad_label != 'WICKERSHAM 50m') & 
                      (allHeight$quad_label != 'WICKERSHAM 70m') &
                      (allHeight$quad_label != 'WICKERSHAM 90m') & 
                      (allHeight$quad_label != 'WICKERSHAM 89.5m') &
                      (allHeight$quad_label != 'PEDRO 70m'),]

# 5.2 REMOVE ERRONEOUS OBSERVATIONS ------------------------------
# These are instances where the height was accidentally not recorded in the field

allHeight = allHeight[!(allHeight$height_observed == 0 & allHeight$cover_observed >= 1),]

# 5.3 GET "PRESENCE ONLY" DATA BASED ON CLASSIFICATION ------------------------------
# This removes the 'spurious pixel' problem where a small number of mis-classified pixels have height values associated with them that bias results
# i.e. when there is very little cover of something in a quadrat, the height estimates are unreliable
# This also removes most instances where predicted height is zero, as these are almost always in areas where no cover was predicted in the quadrat (i.e. NA was converted to zero earlier in tidying)

allHeight = allHeight[allHeight$cover_predicted >= 2,]

# 5.4 TIDY ------------------------------

# Count number of PFTs for each site/quadrat/summary_type combination
n_PFT = allHeight %>% count(site_code, quadrat_num, summaryType)

# Add count to dataframe
allHeight = dplyr::left_join(allHeight, n_PFT, by = c('site_code', 'quadrat_num', 'summaryType'))

# Remove observations where only TOTAL remains after filtering
allHeight = allHeight[allHeight$n > 1,]

# Remove quadrat label and PFT count
allHeight = subset(allHeight, select = -c(quad_label, n))

# Sort
allHeight = dplyr::arrange(allHeight, site_code, quadrat_num, PFT)

# Add id column
id = 1:nrow(allHeight)
allHeight = cbind(id, allHeight)

# 5.5 SAVE ------------------------------

write.csv(allHeight, outNameFinal, row.names = FALSE)
