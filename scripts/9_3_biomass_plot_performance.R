######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = '*/UAV_figures/'
setwd(dir)

outName = 'biomass_sqrt_color_minimal_all_data_alpha_reorder.png'

######################################################################################################
######################################################################################################

library(qdapTools)
library(dplyr)
library(tidyr)
library(yardstick)
library(ggplot2)
library(ggpmisc)

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

# 1.1 READ IN DATA ------------------------------------------------------

compareBiomass = read.csv('*/0_UAV_final/data/0_biomass_FINAL.csv')

# 1.2 DEFINE FUNCTIONS ------------------------------------------------------

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

# 2. TIDY DATA ------------------------------------------------------

# 2.1 REMOVE CIs ------------------------------------------------------

data = compareBiomass[compareBiomass$metric == 'fit',]

# 2.2 REORDER PFTs ------------------------------------------------------

data$PFT = factor(data$PFT, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS", "TOTAL"))

######################################################################################################
######################################################################################################

# 3. SET UP GRAPHING PARAMETERS ------------------------------------------------------

# 3.1 TEXT SIZE ------------------------------------------------------

textformula = y ~ x
title_text_size = 26
theme_text_size = title_text_size - 2
geom_text_size = theme_text_size / 4

# 3.4 SET UP LOOK UP TABLES ------------------------------------------------------

# PFT lookup
PFTcolors = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES", "TOTAL"), color = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine', 'grey60'))

# Set PFT colors
PFTs = as.character(levels(data$PFT))
colors = PFTs %l% PFTcolors

######################################################################################################
######################################################################################################

# 4. PLOT DATA ------------------------------

# 4.1 CREATE MODELS

m = lm(biomass_observed ~ biomass_predicted*PFT, data = data)

# 4.2 MAKE PREDICTIONS

data.m = cbind(data, predict(m, interval='conf'))

# 4.3 CONVERT TO g m-2 INSTEAD OF g 0.25m-2

data.m$biomass_observed = data.m$biomass_observed*4
data.m$biomass_predicted = data.m$biomass_predicted*4
data.m$fit = data.m$fit*4
data.m$lwr = data.m$lwr*4
data.m$upr = data.m$upr*4

# 4.4 CALCULATE RMSE

RMSE = data.m %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(biomass_observed, biomass_predicted)

RMSE = tidyr::spread(RMSE, .metric, .estimate)
RMSE = subset(RMSE, select = -c(.estimator))
RMSE$rmse_label = paste0('RMSE = ', round(RMSE$rmse, 2),'g')

data.m = dplyr::left_join(data.m, RMSE, by = c('PFT'))

# 4.5 CALCULATE RELATIVE RMSE
# We standardized RSE by mean AGB from field measurements (i.e. RSE/mean(AGB)) 
# This statistic, which we refer to as RSE(%), controlled for differences in biomass magnitude between studies

RMSE.relative = data.m %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(biomass_observed))

RMSE.relative$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE.relative$rmse_relative, 2))

data.m = dplyr::left_join(data.m, RMSE.relative, by = c('PFT'))

# 4.8 PLOT

PFT.plot = 
  ggplot(data.m, aes(x = biomass_predicted,  y = biomass_observed, col = PFT, fill = PFT))+
  geom_point(shape = 21, col = 'black', size = 3, alpha = 0.5)+
  geom_line(aes(y = fit), col = 'black', size = 0.5)+
  geom_abline(slope = 1, intercept = 0, lty = 2)+
  facet_wrap_equal(~ PFT, scales = 'free')+
  geom_label(data = RMSE.relative, aes(label=rmse_relative_label), x = -Inf, y = Inf, hjust = -0.04, vjust = 1.8, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  geom_label(data = RMSE, aes(label=rmse_label), x = -Inf, y = Inf, hjust = -0.06, vjust = 3.1, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+  
  labs(y = expression(paste('Observed Aboveground Biomass (g  ', m^-2, ')')), x = expression(paste('Predicted Aboveground Biomass (g  ', m^-2, ')')), fill = 'Plant Functional Type')+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), 
        title = element_text(size=title_text_size), 
        plot.margin = margin(10, 10, 10, 10))+
  guides(color = FALSE)+
  guides(fill = FALSE)+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..rr.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.1, vjust = -1.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  stat_poly_eq(formula = textformula, geom = 'label', aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.05, vjust = -0.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))

PFT.plot

ggsave(
  outName,
  PFT.plot,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

