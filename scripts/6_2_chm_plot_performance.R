######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots canopy height model accuracy (observed vs. predicted average and maximum height within quadrats)
# See manuscript reference in the README.md file for more details

######################################################################################################
######################################################################################################

library(tidyverse)
library(qdapTools)
library(ggpmisc)
library(yardstick)

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outName.mean = 'chm_performance_height_mean.png'
outName.max = 'chm_performance_height_max.png'

output_results = TRUE

outPath = 'results/' # Set output directory if desired

######################################################################################################
######################################################################################################

# 1. READ IN AND TIDY DATA------------------------------

# Read in height data
heightData = read.csv('data/0_height_FINAL.csv')

######################################################################################################
######################################################################################################

# 2. DEFINE FUNCTIONS ------------------------------

# https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
FacetEqualWrap = ggproto(
  "FacetEqualWrap", FacetWrap,
  
  train_scales = function(self, x_scales, y_scales, layout, data, params) {
    
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetWrap, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id = match(layer_data$PANEL, layout$PANEL)
      
      x_vars = intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars = intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X = layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y = layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)

facet_wrap_equal = function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super = facet_wrap(...)
  
  ggproto(NULL, FacetEqualWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}

######################################################################################################
######################################################################################################

# 3. SET GRAPHING PARAMTERS ------------------------------

# 3.1 TEXT SIZE ------------------------------------------------------

textformula = y ~ x
tite_text_size = 26
theme_text_size = tite_text_size - 2
geom_text_size = theme_text_size / 4

# 3.2 SET UP LOOK UP TABLES ------------------------------------------------------

# Lookup information
PFTcolors = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES", "TOTAL"), color = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine', 'grey60'))

######################################################################################################
######################################################################################################

# 4. TIDY DATA ------------------------------

# Define factors
heightData$site_code = as.factor(heightData$site_code)
heightData$quadrat_num = as.factor(heightData$quadrat_num)
heightData$PFT = as.factor(heightData$PFT)
heightData$summary_type = as.factor(heightData$summary_type)
  
# Reorder factor levels
heightData$PFT = factor(heightData$PFT, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS", "TOTAL"))

######################################################################################################
######################################################################################################

# 5. PLOT DATA ------------------------------

PFTs = as.character(levels(heightData$PFT))
colors = PFTs %l% PFTcolors

# 5.1 PLOT DATA: ALL SITES BY PFT INCLUDING TOTAL  ------------------------------

# 5.1.3 DIVIDE DATA

data.mean = heightData[heightData$summary_type == 'mean',]
data.max = heightData[heightData$summary_type == 'max',]

# 5.1.4 CREATE MODELS

m.pft.mean = lm(height_observed ~ height_predicted*PFT, data = data.mean)
m.pft.max = lm(height_observed ~ height_predicted*PFT, data = data.max)

# 5.1.5 MAKE PREDICTIONS

data.pft.mean = cbind(data.mean, predict(m.pft.mean, interval='conf', data = data.mean))
data.pft.max = cbind(data.max, predict(m.pft.max, interval='conf', data = data.max))

# 5.1.6 CALCULATE RMSE

# Mean
RMSE.pft.mean = data.pft.mean %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(height_observed, height_predicted)

RMSE.pft.mean = tidyr::spread(RMSE.pft.mean, .metric, .estimate)

RMSE.pft.mean = subset(RMSE.pft.mean, select = -c(.estimator))
RMSE.pft.mean$rmse_label = paste0('RMSE = ', round(RMSE.pft.mean$rmse, 2), 'cm')

data.pft.mean = dplyr::left_join(data.pft.mean, RMSE.pft.mean, by = c('PFT'))

# Max
RMSE.pft.max = data.pft.max %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(height_observed, height_predicted)

RMSE.pft.max = tidyr::spread(RMSE.pft.max, .metric, .estimate)
RMSE.pft.max = subset(RMSE.pft.max, select = -c(.estimator))
RMSE.pft.max$rmse_label = paste0('RMSE = ', round(RMSE.pft.max$rmse, 2),'cm')

data.pft.max = dplyr::left_join(data.pft.max, RMSE.pft.max, by = c('PFT'))

# 5.1.7 CALCULATE RELATIVE RMSE
# We standardized RSE by mean AGB from field measurements (i.e. RSE/mean(AGB)) 
# This statistic, which we refer to as RSE(%), controlled for differences in biomass magnitude between studies

# Mean
RMSE.relative.mean = data.pft.mean %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(height_observed))

RMSE.relative.mean$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE.relative.mean$rmse_relative, 2))

data.pft.mean = dplyr::left_join(data.pft.mean, RMSE.relative.mean, by = c('PFT'))

# Max
RMSE.relative.max = data.pft.max %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(height_observed))

RMSE.relative.max$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE.relative.max$rmse_relative, 2))

data.pft.max = dplyr::left_join(data.pft.max, RMSE.relative.max, by = c('PFT'))

# 5.1.8 PLOT

PFT.plot.mean = 
  ggplot(data.pft.mean, aes(y = height_observed,  x = height_predicted, col = PFT, fill = PFT))+
  geom_point(shape = 21, col = 'black', size = 3, alpha = 0.5)+
  geom_line(aes(y = fit), col = 'black', size = 0.5)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_label(data = RMSE.relative.mean, aes(label=rmse_relative_label), x = -Inf, y = Inf, hjust = -0.04, vjust = 1.8, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  geom_label(data = RMSE.pft.mean, aes(label=rmse_label), x = -Inf, y = Inf, hjust = -0.06, vjust = 3.1, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+  
  facet_wrap_equal(~ PFT, scales = 'free')+
  labs(x = 'Predicted Height (cm)', y = 'Observed Height (cm)', fill = 'Plant Functional Type')+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), title = element_text(size=tite_text_size), plot.margin = margin(10, 10, 10, 10))+
  guides(color = FALSE)+
  guides(fill = FALSE)+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..rr.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.1, vjust = -1.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.05, vjust = -0.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))

PFT.plot.mean

if(output_results){
  
  ggsave(
    paste0(outPath, outName.mean),
    PFT.plot.mean,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 600
  )

}
  
PFT.plot.max = 
  ggplot(data.pft.max, aes(y = height_observed,  x = height_predicted, col = PFT, fill = PFT))+
  geom_point(shape = 21, col = 'black', size = 3, alpha = 0.5)+
  geom_line(aes(y = fit), col = 'black', size = 0.5)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_label(data = RMSE.relative.max, aes(label=rmse_relative_label), x = -Inf, y = Inf, hjust = -0.04, vjust = 1.8, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  geom_label(data = RMSE.pft.max, aes(label=rmse_label), x = -Inf, y = Inf, hjust = -0.06, vjust = 3.1, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+  
  facet_wrap_equal(~ PFT, scales = 'free')+
  labs(y = 'Predicted Height (cm)', x = 'Observed Height (cm)', fill = 'Plant Functional Type')+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), title = element_text(size=tite_text_size), plot.margin = margin(10, 10, 10, 10))+
  guides(color = FALSE)+
  guides(fill = FALSE)+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..rr.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.1, vjust = -1.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.05, vjust = -0.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))

PFT.plot.max

if(output_results){

  ggsave(
    paste0(outPath, outName.max),
    PFT.plot.max,
    width = 40,
    height = 30,
    units = 'cm',    
    bg = 'white',
    dpi = 600
  )
    
}