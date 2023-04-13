######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots the volume/biomass mixed effects model
# See manuscript reference in the README.md file for more details

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

output_results = TRUE

outPath = 'results/'

outName = 'biomass_volume_mixed_effects_model.png'

######################################################################################################
######################################################################################################

library(tidyverse)
library(lme4)
library(yardstick)
library(qdapTools)

######################################################################################################
######################################################################################################

# 1. IMPORT AND TIDY DATA --------------------------------------------------------------------------------------------------------------------------------

# 1.1 INITIALIZE FUNCTIONS --------------------------------------------------------------------------------------------------------------------------------

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

# 1.2 READ IN DATA AND SET PARAMETERS --------------------------------------------------------------------------------------------------------------------------------

# Read in field data
volBiomass = read.csv('data/biomass_volume_model_input.csv')
coverData = read.csv('data/classification_field_UAV_data.csv')

# 1.3 TIDY FIELD DATA --------------------------------------------------------------------------------------------------------------------------------

# 1.3.1 ADD COVER DATA

volBiomass = dplyr::left_join(volBiomass, coverData, by= c("PFT" = "PFT_fine", "site_code", "quadrat_num"))

print('Data frames merged, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.3.2 CALCULATE COVER ERROR

volBiomass$cover_error = abs(volBiomass$cover_percent - volBiomass$cover_predicted)

# 1.3.3 REMOVE HIGH COVER ERROR

# For each PFT, remove the top X% of observations with the highest error in classification
# We only want the highest quality data used for modeling biomass, so exclude quadrats with high cover error
volBiomass = volBiomass %>% 
  dplyr::group_by(PFT) %>% 
  dplyr::arrange(PFT, desc(cover_error)) %>% 
  dplyr::filter(cover_error < quantile(cover_error, .80))

print('High cover error removed, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.3.4 REMOVE ERRONEOUS PREDICTIONS
# Instances where we predict 0 volume but there is weight
# Instances where there is no weight but we predict volume
# These are instances where there was a verifiable error in modeling so we do not want to include them

volBiomass = volBiomass[!(volBiomass$volume == 0 & volBiomass$weight > 0),]
volBiomass = volBiomass[!(volBiomass$weight == 0 & volBiomass$volume > 0),]

print('Erroneous predictions removed, the data frame dimensions are: ')
print(dim(volBiomass))
cat("\n")

# 1.3.5 REORDER PFTs

volBiomass$PFT = factor(volBiomass$PFT, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS"))

######################################################################################################
######################################################################################################

# 2. MODEL --------------------------------------------------------------------------------------------------------------------------------

predict_data = volBiomass

# 2.1 CREATE MODELS ------------------------------

# Scale variables
predict_data$weight = sqrt(predict_data$weight)
predict_data$volume = sqrt(predict_data$volume/1000)

# Fit model -- mixed effects
# Since we want the intercept to be the same for all iterations, we add ONLY the interaction term
mSqrt = lmer(weight ~ 0 + volume:PFT + (0 + volume:PFT | site_code), data = predict_data, control = lmerControl(optimizer = 'bobyqa')) # Using bobyqa optimizer to avoid "Model failed to converge with max|grad| = 0.00791425 (tol = 0.002, component 1)" issues

print('Model fit, the model is:')
print(mSqrt)
cat("\n")

# 2.2 MAKE PREDICTIONS ------------------------------
# https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html

# Fixed effects only
data.sqrt = cbind(predict_data, fit = predict(mSqrt, predict_data, re.form = NA)) # Note that since we are predicting on sites that are specified in the model, we can take advantage of the random effects when predicting -- in other cases re.form is set to NA because the newdata does not contain the random effects variable
CIs = LMM_CIs(mSqrt, data.sqrt, 'fit', 'site_code', 1.96, FALSE)
CIs[CIs < 0] = 0 # Replace negative lower confidence intervals with zero
data.sqrt$fit[data.sqrt$fit < 0] = 0 # Replace any negative fit values with zero
data.sqrt = cbind(data.sqrt, CIs)

# Mixed effects model
data.sqrtRE = cbind(predict_data, fit = predict(mSqrt, predict_data)) # Note that since we are predicting on sites that are specified in the model, we can take advantage of the random effects when predicting -- in other cases re.form is set to NA because the newdata does not contain the random effects variable
CIs = LMM_CIs(mSqrt, data.sqrtRE, 'fit', 'site_code', 1.96, FALSE)
CIs[CIs < 0] = 0 # Replace negative lower confidence intervals with zero
data.sqrtRE$fit[data.sqrtRE$fit < 0] = 0 # Replace any negative fit values with zero
data.sqrtRE = cbind(data.sqrtRE, CIs)

# Return variables to original scales
data.sqrt$weight = data.sqrt$weight^2
data.sqrt$volume = (data.sqrt$volume^2)*1000
data.sqrt$fit = data.sqrt$fit^2
data.sqrt$lwr = data.sqrt$lwr^2
data.sqrt$upr = data.sqrt$upr^2
data.sqrtRE$weight = data.sqrtRE$weight^2
data.sqrtRE$volume = (data.sqrtRE$volume^2)*1000
data.sqrtRE$fit = data.sqrtRE$fit^2
data.sqrtRE$lwr = data.sqrtRE$lwr^2
data.sqrtRE$upr = data.sqrtRE$upr^2

# Convert to g m-2 instead of g 0.25m-2
data.sqrt$weight = data.sqrt$weight*4
data.sqrt$volume = data.sqrt$volume*4
data.sqrt$fit = data.sqrt$fit*4
data.sqrt$lwr = data.sqrt$lwr*4
data.sqrt$upr = data.sqrt$upr*4
data.sqrtRE$weight = data.sqrtRE$weight*4
data.sqrtRE$volume = data.sqrtRE$volume*4
data.sqrtRE$fit = data.sqrtRE$fit*4
data.sqrtRE$lwr = data.sqrtRE$lwr*4
data.sqrtRE$upr = data.sqrtRE$upr*4

# 2.3 CALCULATE RMSE ------------------------------

RMSE.sqrt = data.sqrt %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(weight, fit)

RMSE.sqrt = tidyr::spread(RMSE.sqrt, .metric, .estimate)
RMSE.sqrt = subset(RMSE.sqrt, select = -c(.estimator))
RMSE.sqrt$rmse_label = paste('RMSE =', round(RMSE.sqrt$rmse, 2), ' g')

RMSE.sqrtRE = data.sqrtRE %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(weight, fit)

RMSE.sqrtRE = tidyr::spread(RMSE.sqrtRE, .metric, .estimate)
RMSE.sqrtRE = subset(RMSE.sqrtRE, select = -c(.estimator))
RMSE.sqrtRE$rmse_label = paste('RMSE =', round(RMSE.sqrtRE$rmse, 2), ' g')

# Join RMSE to data frame
data.sqrt = dplyr::left_join(data.sqrt, RMSE.sqrt, by = c('PFT'))
data.sqrtRE = dplyr::left_join(data.sqrtRE, RMSE.sqrtRE, by = c('PFT'))

# Calculate relative RMSE
# We standardized RSE by mean AGB from field measurements (i.e. RSE/mean(AGB)) 
# This statistic, which we refer to as RSE(%), controlled for differences in biomass magnitude between studies
RMSE.relative.sqrt = data.sqrt %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(weight))

RMSE.relative.sqrt$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE.relative.sqrt$rmse_relative, 2))

RMSE.relative.sqrtRE = data.sqrtRE %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(weight))

RMSE.relative.sqrtRE$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE.relative.sqrtRE$rmse_relative, 2))

# Join relative RMSE to data frame
data.sqrt = dplyr::left_join(data.sqrt, RMSE.relative.sqrt, by = c('PFT'))
data.sqrtRE = dplyr::left_join(data.sqrtRE, RMSE.relative.sqrtRE, by = c('PFT'))

# 2.4 CALCULATE R2 ------------------------------

R2.sqrt = data.sqrt %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(r2 = cor(fit, weight)^2)

R2.sqrt$r2_label = paste0('R^2 == ', round(R2.sqrt$r2, 2))

R2.sqrtRE = data.sqrtRE %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(r2 = cor(fit^2, weight^2)^2)

R2.sqrtRE$r2_label = paste0('R^2 == ', round(R2.sqrtRE$r2, 2))

# Join proportional RMSE to data frame
data.sqrt = dplyr::left_join(data.sqrt, R2.sqrt, by = c('PFT'))
data.sqrtRE = dplyr::left_join(data.sqrtRE, R2.sqrtRE, by = c('PFT'))

######################################################################################################
######################################################################################################

# 3. PLOT --------------------------------------------------------------------------------------------------------------------------------

# 3.2.5 PLOT COLOR ------------------------------

PFTcolors = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES", "TOTAL"), color = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine', 'grey60'))

PFTs = as.character(levels(data.sqrt$PFT))
colors = PFTs %l% PFTcolors

textformula = y ~ 0 + x
tite_text_size = 26
theme_text_size = tite_text_size - 2
geom_text_size = theme_text_size / 4

PFT.plot.sqrt = 
  ggplot(data.sqrt, aes(x = volume,  y = weight, col = PFT, fill = PFT))+
  geom_point(shape = 21, col = 'black', size = 3, alpha = 0.5)+
  geom_line(aes(y = fit), col = 'black', size = 0.7)+ # Fixed effects only
  geom_line(data = data.sqrtRE, aes(y = fit, group = site_code), col = 'black', size = 0.5, alpha = 0.3)+ # Fixed effects + random slopes
  facet_wrap(~ PFT, scales = 'free')+
  geom_label(data = RMSE.relative.sqrtRE, aes(label=rmse_relative_label), x = -Inf, y = Inf, hjust = -0.04, vjust = 1.8, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  geom_label(data = RMSE.sqrtRE, aes(label=rmse_label), x = -Inf, y = Inf, hjust = -0.06, vjust = 3.1, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+  
  geom_label(data = R2.sqrt, aes(label=r2_label), parse = TRUE, x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  labs(y = expression(paste('Aboveground Biomass (g  ', m^-2, ')')), x = expression(paste('Volume (', cm^3, ' ', m^-2, ')')))+
    theme_minimal()+
  theme(text = element_text(size=theme_text_size), title = element_text(size=tite_text_size))+
  guides(color = FALSE)+
  guides(fill = FALSE)+
  coord_cartesian(ylim = c(0, NA))+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)

PFT.plot.sqrt

if(output_results){
  
  ggsave(
    paste0(outPath, outName),
    PFT.plot.sqrt,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 600
  )
  
}
