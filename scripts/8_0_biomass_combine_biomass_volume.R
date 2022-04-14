######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = "*/0_UAV_final/data"
setwd(dir)

outNameTidy = "biomass_volume_data_tidy.csv"
outNameFinal = "biomass_volume_model_input.csv"

######################################################################################################
######################################################################################################

# 1. IMPORT DATA --------------------------------------------------------------------------------------------------------------------------------

# Read in field data
biomassData = read.csv('*/0_UAV_final/data/biomass_field_data.csv')
volumeData = read.csv('*/0_UAV_final/data/volume_data.csv')
coverData = read.csv('*/0_UAV_final/data/classification_field_UAV_data_tidy.csv')

######################################################################################################
######################################################################################################

# 2. TIDY DATA --------------------------------------------------------------------------------------------------------------------------------

# 2.1 GROUP BIOMASS DATA --------------------------------------------------------------------------------------------------------------------------------

biomassData = data.frame(biomassData %>% dplyr::group_by(site_code, quadrat_num, PFT_fine) %>% dplyr::summarise(weight = sum(weight)))

# 2.2 MERGE DATA BASED ON PFT --------------------------------------------------------------------------------------------------------------------------------

volBiomass = dplyr::left_join(biomassData, volumeData, by= c("PFT_fine" = "PFT", "site_code", "quadrat_num"))
volBiomass = dplyr::left_join(volBiomass, coverData, by= c("PFT_fine" = "PFT", "site_code", "quadrat_num"))
volBiomass = volBiomass %>% rename(PFT = PFT_fine)

print('Data frames merged, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 2.3 SET UP SITE/QUADRAT LABELS --------------------------------------------------------------------------------------------------------------------------------

volBiomass$quad_label = paste(volBiomass$site_code, volBiomass$quadrat_num)

# 2.4 HANDLE COARSE SHRUB CLASS --------------------------------------------------------------------------------------------------------------------------------

# If there is a general SHRUBS class for a particular quadrat, we want to discard any other shrub type entries from that quadrat i.e. remove any DECIDUOUS SHRUBS or EVERGREEN SHRUBS entries from the same quadrat
# We do not know what the DECIDUOUS/EVERGREEN breakdown is for the unallocated SHRUBS class so the individual DECIDUOUS SHRUBS and EVERGREEN SHRUBS records are incomplete and should not be included

# Get all quadrats that have a SHRUBS class
shrub_data = volBiomass[volBiomass$PFT == 'SHRUBS',] 

# From main dataframe, exclude any DECIDUOUS SHRU or EVERGREEN SHRUBS records where the quadrat also has a SHRUBS record
volBiomass = volBiomass[!(volBiomass$quad_label %in% shrub_data$quad_label &  volBiomass$PFT == 'DECIDUOUS SHRUBS'),]
volBiomass = volBiomass[!(volBiomass$quad_label %in% shrub_data$quad_label &  volBiomass$PFT == 'EVERGREEN SHRUBS'),]

# Remove SHRUBS class itself
volBiomass = volBiomass[volBiomass$PFT != 'SHRUBS',]

# Remove TREES class too
volBiomass = volBiomass[volBiomass$PFT != 'TREES',]

print('Coarse shrub class handled, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 2.5 TIDY JOINED DATA --------------------------------------------------------------------------------------------------------------------------------

# Remove observations where cover is NA
# These are observations from sites with poor data quality that are excluded from modeling
volBiomass = volBiomass[!is.na(volBiomass$cover_observed),]

# Convert negative weights to zero
volBiomass$weight[volBiomass$weight < 0] = 0

# Anywhere there are NAs in the volume data, convert to zero
volBiomass$volume[!is.finite(volBiomass$volume)] = 0 

######################################################################################################
######################################################################################################

# 3. SAVE TIDY BIOMASS/VOLUME DATA --------------------------------------------------------------------------------------------------------------------------------

write.csv(volBiomass, outNameTidy, row.names = FALSE)

######################################################################################################
######################################################################################################

# 4. FINALIZE DATA ------------------------------------------------------

# 4.1 REMOVE OBSERVATIONS WITH POOR DATA QUALITY ------------------------------------------------------

# WICKERSHAM 10m, 30m, 50m, 70m, 90m -- quadrats harvested before UAV flight
# TWELVEMILE 50m LICHENS -- multi-bag sample and suspect missing bag based on comparison to cover/biomass from other quadrats

volBiomass = volBiomass[(volBiomass$quad_label != 'WICKERSHAM 10m') & 
                        (volBiomass$quad_label != 'WICKERSHAM 30m') & 
                        (volBiomass$quad_label != 'WICKERSHAM 50m') & 
                        (volBiomass$quad_label != 'WICKERSHAM 70m') & 
                        (volBiomass$quad_label != 'WICKERSHAM 90m') &
                        !(volBiomass$quad_label == 'TWELVEMILE 50m' & volBiomass$PFT == 'LICHENS'),]

print('Outliers removed, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 4.2 SAVE ------------------------------

write.csv(volBiomass, outNameFinal, row.names = FALSE)
