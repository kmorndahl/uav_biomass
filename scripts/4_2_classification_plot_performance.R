library(dplyr)
library(tidyr)
library(qdapTools)
library(yardstick)
library(ggplot2)
library(ggpmisc)

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = '*/UAV_figures/'
outName = 'cover_color_minimal_alpha.png'
setwd(dir)

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------------------------------

coverTotal = read.csv('*/0_UAV_final/data/0_cover_FINAL.csv', header = TRUE)

# Lookup information
PFTcolors = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine')
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))

# Graphing parameters
textformula = y ~ x
tite_text_size = 26
theme_text_size = tite_text_size - 2
geom_text_size = theme_text_size / 4

######################################################################################################
######################################################################################################

# 2. DEFINE FUNCTIONS ------------------------------------------------------

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

# 3. TIDY DATA ------------------------------------------------------

# Reorder levels
coverTotal$PFT = factor(coverTotal$PFT, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS"))

# Set PFT colors
PFTs = as.character(levels(as.factor(coverTotal$PFT)))
PFTs = PFTs %l% PFTlookup
colors = PFTs %l% data.frame(ID = seq(1:length(PFTcolors)), PFT = PFTcolors)

######################################################################################################
######################################################################################################

# 4. PLOT ------------------------------------------------------

# 4.1 MODEL AND CALCULATE RMSE

# Create model
m = lm(cover_observed ~ cover_predicted * PFT, data = coverTotal)

# Calculate predictions
coverTotal = cbind(coverTotal, predict(m, interval='conf'))

# Calculate RMSE by group
RMSE = coverTotal %>%
  dplyr::group_by(PFT) %>%
  yardstick::rmse(cover_observed, cover_predicted)

RMSE = tidyr::spread(RMSE, .metric, .estimate)
RMSE = subset(RMSE, select = -c(.estimator))
RMSE$rmse_label = paste0('RMSE = ', round(RMSE$rmse, 2), '%')

# Join RMSE to data frame
coverTotal = dplyr::left_join(coverTotal, RMSE, by = c('PFT'))

# Calculate relative RMSE
# We standardized RSE by mean AGB from field measurements (i.e. RSE/mean(AGB)) 
# This statistic, which we refer to as RSE(%), controlled for differences in biomass magnitude between studies
RMSE_relative = coverTotal %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(rmse_relative = mean(rmse)/mean(cover_observed))

RMSE_relative$rmse_relative_label = paste0('Relative RMSE = ', round(RMSE_relative$rmse_relative, 2))

# Join proportional RMSE to data frame
coverTotal = dplyr::left_join(coverTotal, RMSE_relative, by = c('PFT'))

# 4.2 GRAPH -- COLOR MINIMAL

# Plot
PFTplot =
  ggplot(coverTotal, aes(y = cover_observed,  x = cover_predicted, col = PFT, fill = PFT, label = quad_label))+
  geom_point(shape = 21, col = 'black', size = 3,alpha = 0.5)+
  geom_line(aes(y = fit), col = 'black', size = 0.5)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_label(data = RMSE_relative, aes(label=rmse_relative_label), x = -Inf, y = Inf, hjust = -0.04, vjust = 1.8, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  geom_label(data = RMSE, aes(label=rmse_label), x = -Inf, y = Inf, hjust =  -0.06, vjust = 3.1, color = 'grey20',  fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+ 
  facet_wrap_equal(~ PFT, scales = 'free')+
  labs(x = 'Predicted % Cover', y = 'Observed % Cover', fill = 'Plant Functional Type')+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), 
        title = element_text(size=tite_text_size),
        plot.margin = margin(r = 15))+
  guides(color = FALSE)+
  guides(fill = FALSE)+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  stat_poly_eq(formula = y ~ x, geom = 'label', aes(label = paste(..rr.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.1, vjust = -1.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))+
  stat_poly_eq(formula = y ~ x, geom = 'label', aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, label.y = -Inf, label.x = Inf, hjust = 1.05, vjust = -0.3, color = 'grey20', fill = 'white', size = geom_text_size, label.size = NA, alpha = 0.6, label.padding = unit(0.01, "lines"))
PFTplot

ggsave(
  outName,
  PFTplot,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

