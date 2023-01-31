######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script runs models to predict PFT biomass across UAV footprints
# Models are mixed effects models with biomass as the response, volume and PFT as predictors, and site as a random effect
# Models are applied to UAV data to produce per-pixel biomass estimates for each PFT across UAV footprints
# See manuscript reference in the README.md file for more details on methodology

######################################################################################################
######################################################################################################

# NOTE: OVERWRITE SET TO TRUE! CHANGE IF NECESSARY

# SET OUTPUT DIRECTORY

outPath = "*/UAV_biomass/results_NAs_FINAL/"

######################################################################################################
######################################################################################################

# 1. IMPORT AND TIDY DATA --------------------------------------------------------------------------------------------------------------------------------

# 1.1 READ IN DATA AND SET PARAMETERS --------------------------------------------------------------------------------------------------------------------------------

library(raster)
library(tidyverse)
library(lme4)
library(data.table)

# Read in field data
volBiomass = read.csv('*/0_UAV_final/data/biomass_volume_model_input.csv')
coverData = read.csv('*/0_UAV_final/data/classification_field_UAV_data_tidy.csv')

# Gather volume rasters
volPaths = list.files(path = '*/UAV_chm/results', pattern = "_vol_cm.tif$", full.names = TRUE)
print("Volume rasters are:")
print(volPaths)
cat("\n")

# Gather classified rasters
classifiedPaths = list.files(path = "*/UAV_classification/results", pattern = "classified_final.tif$", full.names = TRUE)
print("Classified rasters are:")
print(classifiedPaths)
cat("\n")

# Increase raster memory limit
raster::rasterOptions(maxmemory = 1e+11) # 100 GB
print(rasterOptions())

# Get bash parameters
params = commandArgs(trailingOnly = TRUE)

# Include random-effects in confidence intervals?
add_random = FALSE

# Initialize functions

# Function to calculate confidence intervals for linear mixed effect model
# NOTE: these confidence intervals are based on uncertainty from the fixed effects ONLY
# See section 'Predictions and/or confidence (or prediction) intervals on predictions' in the following link : 
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
# https://stats.stackexchange.com/questions/499248/confidence-intervals-in-probabilities-for-mixed-effects-logistic-regression
# https://stackoverflow.com/questions/42569698/how-to-just-calculate-the-diagonal-of-a-matrix-product-in-r
# Parameters: 
#   model = linear mixed effect model
#   data = data frame with predictors and predicted response
#   responseVar = name of response variable
#   reVar = name of random effects variable
#   z = z-statistic
#   add_random = TRUE or FALSE -- whether or not to include the variance from the random effects

LMM_CIs = function(model, data, responseVar, reVar, z, add_random) {
  
  # Set NA options
  current.na.action = options('na.action') # Save current option
  options(na.action='na.pass') # Change to na.pass
  print('Changed NA action:')
  print(options('na.action')$na.action)
  
  print('Data set dimensions:')
  print(dim(data))
  
  mm = model.matrix(terms(model), data) # Terms formulation includes fixed effects only
  print('Model matrix dimensions:')
  print(dim(mm))
  
  pvar1 = colSums(t(mm) * tcrossprod(vcov(model),mm)) # Identical to diag(mm %*% tcrossprod(vcov(model),mm)) but does not require performing full matrix multiplication
  print('Variance (fixed effects only) dimensions:') # vcov() function includes fixed effects only
  print(length(pvar1))
  
  tvar1 = pvar1+VarCorr(model)[[reVar]][1] # VarCorr() function brings in random effects variance
  print('Variance (fixed effects + variance of random effects) dimensions:')
  print(length(tvar1))
  
  cmult = z
  
  if(add_random == FALSE){
    CIs = data.frame(
      lwr = data[[responseVar]]-cmult*sqrt(pvar1)
      , upr = data[[responseVar]]+cmult*sqrt(pvar1)
    )
  }
  else if(add_random == TRUE){
    CIs = data.frame(
      lwr = data[[responseVar]]-cmult*sqrt(tvar1)
      , upr = data[[responseVar]]+cmult*sqrt(tvar1)
    )  
  }
  else{
    print('Please specify TRUE or FALSE to indicate whether you would like to add random effects variance')
    return()
  }
  
  # Return to standard NA option
  options('na.action' = current.na.action$na.action)
  
  # Confirm return was successful
  print('NA options returned to standard:')
  print(options('na.action')$na.action)
  cat("\n")
  
  return(CIs)
  
}

# 1.2 GET SITE --------------------------------------------------------------------------------------------------------------------------------

site = params[1]

print(paste0("==================== CURRENT SITE: ", site, " ===================="))
cat("\n")

# 1.3 GET RASTERS --------------------------------------------------------------------------------------------------------------------------------

vol = raster::raster(volPaths[grep(site, volPaths)])

classified = raster::raster(classifiedPaths[grep(site, classifiedPaths)])

print("Volume raster is:")
print(vol)
cat("\n")

print("Classified raster is: ")
print(classified)
cat("\n")

# 1.4 TIDY FIELD DATA --------------------------------------------------------------------------------------------------------------------------------

# 1.4.1 ADD COVER DATA

volBiomass = dplyr::left_join(volBiomass, coverData, by= c("PFT", "site_code", "quadrat_num"))

print('Data frames merged, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.4.2 CALCULATE COVER ERROR

volBiomass$cover_error = abs(volBiomass$cover_observed - volBiomass$cover_predicted)

# 1.4.3 SET UP LOOKUP TABLES

PFTlookup = data.frame(ID = seq(1,9), PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"))
sites = unique(volBiomass$site_code)
SITElookup = data.frame(site_code = sites, ID = seq(1, length(sites)))

# 1.4.4 REMOVE HIGH COVER ERROR

# For each PFT, remove the top X% of observations with the highest error in classification
# We only want the highest quality data used for modeling biomass, so exclude quadrats with high cover error
volBiomass = volBiomass %>% 
  group_by(PFT) %>% 
  arrange(PFT, desc(cover_error)) %>% 
  filter(cover_error < quantile(cover_error, .80))

print('High cover error removed, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.4.5 REMOVE ERRONEOUS PREDICTIONS
# Instances where we predict 0 volume but there is weight
# Instances where there is no weight but we predict volume
# These are instances where there was a verifiable error in modeling so we do not want to include them

volBiomass = volBiomass[!(volBiomass$volume == 0 & volBiomass$weight > 0),]
volBiomass = volBiomass[!(volBiomass$weight == 0 & volBiomass$volume > 0),]

print('Erroneous predictions removed, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.4.6 CONVERT FACTORS TO INTEGER BASED FACTORS

volBiomass$PFT = PFTlookup$ID[match(unlist(volBiomass$PFT), PFTlookup$PFT)]
volBiomass$PFT = as.factor(volBiomass$PFT)
volBiomass$site_code = SITElookup$ID[match(unlist(volBiomass$site_code), SITElookup$site_code)]
volBiomass$site_code = as.factor(volBiomass$site_code)

# 1.4.7 SAVE FOR MODELING

predict_data = volBiomass

######################################################################################################
######################################################################################################

# 2. PREPARE RASTER DATA --------------------------------------------------------------------------------------------------------------------------------

# 2.1 ALIGN DATA --------------------------------------------------------------------------------------------------------------------------------

# Check projections
georef = raster::crs(vol)
if(raster::compareCRS(georef, crs(classified)) == FALSE){
  print('Classified CRS different from volume CRS, reprojecting...')
  classified = raster::projectRaster(classified, crs = georef)
  print('Classified reprojected')} else{
    print('CRS match, no reprojection needed')
  }
cat("\n")

# Get minimum extent
e = raster::intersect(raster::extent(classified), raster::extent(vol))
print('Minimum extent calculated, the minimum extent is:')
print(e)
cat("\n")

# Crop
volCrop = raster::crop(vol, e)
classifiedCrop = raster::crop(classified, e)
print('Rasters cropped to minimum extent')
print('Cropped volume raster:')
print(volCrop)
print('Cropped classified raster:')
print(classifiedCrop)
cat("\n")

# Resample to same extent and resolution -- NEW WAY
classifiedResample = raster::aggregate(x=classifiedCrop, fact=xres(volCrop)/xres(classifiedCrop), fun = modal)
classifiedResample = raster::resample(x=classifiedResample, y=volCrop, method="ngb")

print('Classified raster resampled, the resampled classified raster is:')
print(classifiedResample)
cat("\n")

# 2.2 TIDY AND SCALE VOLUME DATA --------------------------------------------------------------------------------------------------------------------------------

# Change any negative values to zero
volCrop[volCrop < 0] = 0

# Scale for modeling
volCrop = sqrt(volCrop/1000)

# Report results
print('Volume raster negative values converted to zero and raster scaled for modeling, the tidy volume raster is:')
print(volCrop)
cat("\n")

# 2.3 MASK OUT PFTs NOT PRESENT IN BIOMASS DATA --------------------------------------------------------------------------------------------------------------------------------

# Mask out areas where PFT is non-vegetated, bryophyte, shadow or tree
classifiedMask = classifiedResample
m = c(1, NA, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, NA, 8, NA, 9, NA)
rclmat = matrix(m, ncol=2, byrow=TRUE)
classifiedMask = raster::reclassify(classifiedMask, rclmat, include.lowest = TRUE)
classifiedMask = round(classifiedMask) # Make sure values are integer

# Report results
print('PFT raster reclassified, the reclassified PFT raster is:')
print(classifiedMask)
cat("\n")

# 2.6 STACK RASTERS --------------------------------------------------------------------------------------------------------------------------------

# Finalize rasters
volFinal = volCrop
classifiedFinal = classifiedMask

print('The final volume raster is:')
print(volFinal)
cat("\n")

print('The final PFT raster is:')
print(classifiedFinal)
cat("\n")

# Stack raster : volume + classified
stackFinal = raster::stack(volFinal, classifiedFinal)

# Change names of rasters in stack to match model
names(stackFinal) = c('volume', 'PFT')

print('Rasters stacked, the raster stack is:')
print(stackFinal)
cat("\n")

# 2.7 CONVERT RASTER DATA TO DATA FRAME --------------------------------------------------------------------------------------------------------------------------------

# Get data from raster stack
stackDF = data.frame(as.matrix(stackFinal))

# Get site number
siteNum = SITElookup$ID[SITElookup$site_code == site]

# Add site to data frame
stackDF$site_code = rep(siteNum, nrow(stackDF))

# Finalize data frame names
names(stackDF) = c('volume', 'PFT', 'site_code')

# Convert PFT to factor and add all factor levels present in model
stackDF$PFT = as.factor(stackDF$PFT)
levels(stackDF$PFT) = c(2, 3, 4, 5, 6)

# Convert site_code to factor
stackDF$site_code = as.factor(stackDF$site_code)

# Report results
print('Raster stack converted to data frame, preview:')
print(stackDF[(nrow(stackDF)/2) : ((nrow(stackDF)/2) + 50),])
print(paste0('Number of rows: ', nrow(stackDF)))
cat("\n")

######################################################################################################
######################################################################################################

# 3. MODEL --------------------------------------------------------------------------------------------------------------------------------

# 3.1 FIT MODELS --------------------------------------------------------------------------------------------------------------------------------

# Select only relevant columns
predict_data = predict_data %>% subset(select = c(site_code, PFT, quadrat_num, weight, volume))

# Scale variables for modeling
predict_data$weight = sqrt(predict_data$weight)
predict_data$volume = sqrt(predict_data$volume/1000)

# Fit model -- mixed effects -- variables transformed -- forced through origin
mSqrtOrigin = lmer(weight ~ 0 + volume:PFT + (0 + volume:PFT | site_code), data = predict_data, control = lmerControl(optimizer = 'bobyqa')) # Using bobyqa optimizer to avoid convergence issues

print('Model fit, the model is:')
print(mSqrtOrigin)
cat("\n")

# 3.2 PREDICT --------------------------------------------------------------------------------------------------------------------------------

# 3.2.1 MAKE PREDICTIONS --------------------------------------------------------------------------------------------------------------------------------

# Make predictions -- square root -- origin

# Bug in predict.merMod code with re.form = NULL and na.action = na.pass
# Workaround: use na.action = na.omit and merge fitted values with full dataframe later
fit = predict(mSqrtOrigin, stackDF, re.form = NULL, allow.new.levels = TRUE, na.action = na.omit) # Mixed effects model -- NOTE that since we are predicting on sites that are specified in the model, we can take advantage of the random effects when predicting -- in other cases re.form is set to NA because the newdata does not contain the random effects variable
fit = data.frame(fit = fit) # Convert to data frame

print('Biomass predicted across raster stack (square root through origin), preview:')
print(fit[(nrow(fit)/2) : ((nrow(fit)/2) + 50),])
print(paste0('Number of predicted rows: ', nrow(fit)))
cat("\n")

# 3.2.2 CALCULATE CONFIDENCE INTERVALS --------------------------------------------------------------------------------------------------------------------------------

# Identify observations with complete predictor data
stackDF$weight = complete.cases(stackDF) # Identify rows with complete predictor data
print(paste0('Number of observations with complete predictor data: ', sum(stackDF$weight[stackDF$weight], na.rm = TRUE)))
cat("\n")

# Add predictions to dataframe
fit = data.frame(fit) # Convert predictions to data frame
stackDF$weight[stackDF$weight] = fit[1:sum(stackDF$weight[stackDF$weight], na.rm = TRUE),] # Fill complete rows with predictions
stackDF$weight[!complete.cases(stackDF)] = NA # Where there are NOT complete cases, fill with NAs
names(stackDF) = c('volume', 'PFT', 'site_code', 'weight')
print('Predictions added:')
print(head(stackDF))
cat("\n")

# Report results
print('Final data frame for confidence interval calculation, preview:')
print(stackDF[(nrow(stackDF)/2) : ((nrow(stackDF)/2) + 50),])
print(paste0('Dimensions: ', nrow(stackDF), ' ', ncol(stackDF)))
print(paste0('The number of NAs is: ', sum(is.na(stackDF$weight))))
print(str(stackDF))
cat("\n")

# Calculate confidence intervals
CIs = LMM_CIs(mSqrtOrigin, stackDF, 'weight', 'site_code', 1.96, add_random)
CIs[CIs < 0] = 0 # Replace negative lower confidence intervals with zero

# Report results
print('Confidence intervals calculated, preview:')
print(CIs[(nrow(CIs)/2) : ((nrow(CIs)/2) + 50),])
print(paste0('Dimensions: ', nrow(CIs), ' ', ncol(CIs)))
cat("\n")

# Combine into one data frame
biomass_sqrt_origin = cbind(stackDF$weight, CIs)
names(biomass_sqrt_origin) = c('fit', 'lwr', 'upr')

# Report results
print('Predictions and confidence intervals finalized, preview:')
print(biomass_sqrt_origin[(nrow(biomass_sqrt_origin)/2) : ((nrow(biomass_sqrt_origin)/2) + 50),])
print(paste0('Final data frame dimensions: ', nrow(biomass_sqrt_origin), ' ', ncol(biomass_sqrt_origin)))
cat("\n")

# 3.2.2 TRANSFORM PREDICTION UNITS --------------------------------------------------------------------------------------------------------------------------------

# Transform predictions back into non-transformed units -- origin
biomass_sqrt_origin = biomass_sqrt_origin^2

print('Biomass converted back to sensible units (square root through origin), preview:')
print(biomass_sqrt_origin[(nrow(biomass_sqrt_origin)/2) : ((nrow(biomass_sqrt_origin)/2) + 50),])

cat("\n")

# 3.2.3 SEPARATE FIT, LWR and UPR --------------------------------------------------------------------------------------------------------------------------------

fit = biomass_sqrt_origin$fit
lwr = biomass_sqrt_origin$lwr
upr = biomass_sqrt_origin$upr
ci = biomass_sqrt_origin$upr - biomass_sqrt_origin$lwr

# 3.2.4 INITIALIZE OUTPUT RASTER NAMES --------------------------------------------------------------------------------------------------------------------------------

if(add_random == FALSE){
  suffix = '_fixed_biomass.tif'
} else if(add_random == TRUE){
  suffix = '_biomass.tif'
}

# 3.3 CONVERT BACK TO RASTER --------------------------------------------------------------------------------------------------------------------------------
# https://gis.stackexchange.com/questions/365949/linear-regresion-at-each-raster-pixels-value-to-predict-future-value-in-r-lang

setwd(outPath)

# 3.3.1 ALL PFTs --------------------------------------------------------------------------------------------------------------------------------

# FIT

# Convert to raster -- square root -- origin
fit_raster = raster::setValues(volFinal, fit)
print('Biomass predictions converted to raster (square root through origin):')
print(fit_raster)
cat("\n")

# Assign zeros
fit_raster = raster::reclassify(fit_raster, cbind(NA, 0))

# Mask to classified
fit_raster = try(raster::mask(fit_raster, classifiedFinal))

if(class(fit_raster) != "try-error"){
  print("All PFT biomass prediction raster masked to classified:")
  print(fit_raster)
  cat("\n")
}

# Write to disk -- square root -- origin
raster::writeRaster(fit_raster, paste0(site, "_fit_biomass.tif"), overwrite=TRUE)
print('All PFT biomass prediction raster written to disk (square root through origin)')
cat("\n")

# LWR

# Convert to raster -- square root -- origin
lwr_raster = raster::setValues(volFinal, lwr)
print('Biomass lower limits converted to raster (square root through origin):')
print(lwr_raster)
cat("\n")

# Assign zeros
lwr_raster = raster::reclassify(lwr_raster, cbind(NA, 0))

# Mask to classified
lwr_raster = try(raster::mask(lwr_raster, classifiedFinal))

if(class(lwr_raster) != "try-error"){
  print("All PFT biomass lower limit raster masked to classified:")
  print(lwr_raster)
  cat("\n")
}

# Write to disk -- square root -- origin
raster::writeRaster(lwr_raster, paste0(site, "_lwr", suffix), overwrite=TRUE)
print('All PFT biomass lower limit raster written to disk (square root through origin)')
cat("\n")

# UPR

# Convert to raster -- square root -- origin
upr_raster = raster::setValues(volFinal, upr)
print('Biomass upper limits converted to raster (square root through origin):')
print(upr_raster)
cat("\n")

# Assign zeros
upr_raster = raster::reclassify(upr_raster, cbind(NA, 0))

# Mask to classified
upr_raster = try(raster::mask(upr_raster, classifiedFinal))

if(class(upr_raster) != "try-error"){
  print("All PFT biomass upper limit raster masked to classified:")
  print(upr_raster)
  cat("\n")
}

# Write to disk -- square root -- origin
raster::writeRaster(upr_raster, paste0(site, "_upr", suffix), overwrite=TRUE)
print('All PFT biomass upper limit raster written to disk (square root through origin)')
cat("\n")

# CI

# Convert to raster -- square root -- origin
ci_raster = raster::setValues(volFinal, ci)
print('Biomass confidence intervals converted to raster (square root through origin):')
print(ci_raster)
cat("\n")

# Assign zeros
ci_raster = raster::reclassify(ci_raster, cbind(NA, 0))

# Mask to classified
ci_raster = try(raster::mask(ci_raster, classifiedFinal))

if(class(ci_raster) != "try-error"){
  print("All PFT biomass confidence interval raster masked to classified:")
  print(ci_raster)
  cat("\n")
}

# Write to disk -- square root -- origin
raster::writeRaster(ci_raster, paste0(site, "_ci", suffix), overwrite=TRUE)
print('All PFT biomass confidence interval raster written to disk (square root through origin)')
cat("\n")

# 3.3.2 INDIVIDUAL PFTs --------------------------------------------------------------------------------------------------------------------------------

for(val in unique(classifiedFinal)){

  PFTname = gsub(" ", "_", as.character(PFTlookup$PFT[PFTlookup$ID == val]))

  print(paste0('The PFT is: ', PFTname))
  cat("\n")

  # Get current PFT from classified raster
  PFTmask = classifiedFinal
  PFTmask[PFTmask != val] = NA

  # Get current PFT from biomass rasters
  PFT_fit_raster = raster::mask(fit_raster, PFTmask)
  PFT_lwr_raster = raster::mask(lwr_raster, PFTmask)
  PFT_upr_raster = raster::mask(upr_raster, PFTmask)
  PFT_ci_raster = raster::mask(ci_raster, PFTmask)

  # Write to disk
  raster::writeRaster(PFT_fit_raster, paste0(site, "_", PFTname, "_fit_biomass.tif"), overwrite=TRUE)
  print(paste0(PFTname, ' biomass prediction raster written to disk (square root through origin)'))
  cat("\n")
  raster::writeRaster(PFT_lwr_raster, paste0(site, "_", PFTname, "_lwr", suffix), overwrite=TRUE)
  print(paste0(PFTname, ' biomass lower limit raster written to disk (square root through origin)'))
  cat("\n")
  raster::writeRaster(PFT_upr_raster, paste0(site, "_", PFTname, "_upr", suffix), overwrite=TRUE)
  print(paste0(PFTname, ' biomass upper limit raster written to disk (square root through origin)'))
  cat("\n")
  raster::writeRaster(PFT_ci_raster, paste0(site, "_", PFTname, "_ci", suffix), overwrite=TRUE)
  print(paste0(PFTname, ' biomass confidence interval raster written to disk (square root through origin)'))
  cat("\n")

}

print('========== COMPLETE ==========')
cat("\n")




