######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

output_results = TRUE

outPath = 'results/'

outNamePFT = 'biomass_error_modeling_PFT.png'
outNameVegComm = 'biomass_error_modeling_veg_community.png'
outNameVegCommPFT = 'biomass_error_modeling_veg_community_PFT.png'

######################################################################################################
######################################################################################################

library(tidyverse)
library(qdapTools)
library(ICC)
library(rstatix)
library(ggpubr)
library(RColorBrewer)

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

# 1.1 READ IN DATA ------------------------------------------------------

biomassData = read.csv('data/0_biomass_FINAL.csv', header=T)
quadData = read.csv('data/0_quad_attributes_FINAL.csv', header = T)
siteData = read.csv('data/0_site_attributes_FINAL.csv', header = T)
coverData = read.csv('data/classification_field_UAV_data.csv')

# 1.2 GET CORRECT MODEL ------------------------------------------------------

biomassData = biomassData[biomassData$metric == 'fit',]
biomassData = data.frame(biomassData %>% dplyr::select(-c(metric)))

# 1.3 SET GRAPHING PARAMETERS ------------------------------------------------------

# Set text sizes
textformula = y ~ x
title_text_size = 26
theme_text_size = title_text_size - 2
geom_text_size = theme_text_size / 4

######################################################################################################
######################################################################################################

# 2. AGGREGATE AND TIDY DATA ------------------------------------------------------

# 2.1 JOIN ------------------------------------------------------

data = dplyr::left_join(biomassData, quadData, by = c('site_code', 'quadrat_num'))

siteData = siteData %>% dplyr::select(c(site_code, photo_quality, lighting, rgb_pt_dens)) # Select only relevant variables
data = dplyr::left_join(data, siteData, by = c('site_code'))

data = dplyr::left_join(data, coverData, by = c('site_code', 'quadrat_num', 'PFT' = 'PFT_fine'))

# 2.2 TIDY ------------------------------------------------------

data = na.omit(data %>% dplyr::select(-c(id.x, id.y)))

# 2.3 REORDER PFTs ------------------------------------------------------

data$PFT = factor(data$PFT, levels = c("DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "LICHENS", "GRAMINOIDS", "FORBS"))

# 2.4 SET UP LOOK UP TABLES ------------------------------------------------------

# PFT lookup
PFTcolors = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine')
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))

# Vegetation community lookup
VegCommLookup = data.frame(viereck = c("Tussock tundra", "Mixed shrub-sedge tussock tundra", "Wet sedge meadow tundra", "Sedge-birch tundra", "Willow-sedge shrub tundra", "Shrub birch-willow", "Willow-tall", "Willow-low", "Mesic shrub birch-ericaceous shrub", "Dryas tundra", "Sparsely vegetated", "Shrub birch", "Shrub birch-thickets", "Bearberry tundra", "Vaccinium tundra", "Foliose and fruticose lichen", "Sedge-dryas tundra", "Sedge-willow tundra", "Crowberry tundra", "Midgrass-herb", "Cassiope tundra", "Dryas-sedge tundra", "Mixed herbs", "Dryas-lichen tundra", "Alpine herb-sedge", "White spruce", "Willow-graminoid shrub bog", "Low willow", "Midgrass-shrub", "Willow tundra", "Subarctic lowland sedge wet meadow", "Shrub birch-ericaceous shrub bog"),
                           coarse = c("Mesic graminoid", "Low shrub", "Wet graminoid", "Mesic graminoid", "Low shrub", "Low shrub", "Tall shrub", "Low shrub", "Low shrub", "Dwarf shrub", "Sparsely vegetated", "Tall shrub", "Low shrub", "Dwarf shrub", "Dwarf shrub", "Lichen", "Mesic graminoid", "Mesic graminoid", "Dwarf shrub", "Dry graminoid", "Dwarf shrub", "Dwarf shrub", "Herbaceous", "Dwarf shrub", "Herbaceous", "Woodland", "Low shrub", "Low shrub", "Dry graminoid", "Dwarf shrub", "Wet graminoid", "Low shrub"))

# Set PFT colors
PFTcolors = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine')
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))
PFTs = as.character(levels(data$PFT))
PFTs = PFTs %l% PFTlookup
colors = PFTs %l% data.frame(ID = seq(1:length(PFTcolors)), PFT = PFTcolors)

# 2.5 CALCULATE ERROR ------------------------------------------------------

# Calculate error
data$error = abs(data$biomass_observed - data$biomass_predicted)

# 2.3 ADD DERIVED VARIABLES ------------------------------------------------------

data$lc_manual = data$viereck_fine %l% VegCommLookup
data$quad_label = paste(data$site_code, data$quadrat_num)

# 2.4 CONVERT ALL CHARACTER COLUMNS TO FACTOR ------------------------------------------------------

data[sapply(data, is.character)] = lapply(data[sapply(data, is.character)], as.factor)

######################################################################################################
######################################################################################################

# 3. FILTER ------------------------------

# 3.1 REMOVE HIGH COVER ERROR ------------------------------

data$cover_percent = as.numeric(as.character(data$cover_percent))
data$cover_predicted = as.numeric(as.character(data$cover_predicted))

data$cover_error = abs(data$cover_percent - data$cover_predicted)

# For each PFT, remove the top X% of observations with the highest error in classification
data = data.frame(data %>%
  dplyr::group_by(PFT) %>%
  dplyr::arrange(PFT, desc(cover_error)) %>%
  dplyr::filter(cover_error < quantile(cover_error, .80)))

data = data.frame(data %>% dplyr::select(-c(cover_error)))

######################################################################################################
######################################################################################################

# 4. DIVIDE DATA ------------------------------------------------------

# 4.1 PRESENCE/ABSENCE ------------------------------------------------------

# 1 == error
# 0 == no error

data_binom = data

data_binom$any_error = ifelse(data_binom$error >= 0.1, 1, 0)
data_binom$presence_error = ifelse((data_binom$biomass_observed < 0.1 & data_binom$biomass_predicted < 0.1) | (data_binom$biomass_observed >= 0.1 & data_binom$biomass_predicted >= 0.1), 0, 1) # Any error in determine presence/absence
data_binom$false_positive = ifelse((data_binom$biomass_observed < 0.1 & data_binom$biomass_predicted >= 0.1), 1, 0) # False positive
data_binom$false_negative = ifelse((data_binom$biomass_observed >= 0.1 & data_binom$biomass_predicted < 0.1), 1, 0) # False negative

# 4.2 PRESENCE ONLY ------------------------------------------------------

# Only where PFT is present according to field data
# Remove false positives
# Remove false negatives
# This removes all zeros from the dataset, those are taken care of with the bionomial modeling
# This dataset represents only observations where we correctly designate the PFT as present in the quadrat, now we want to figure out how good we do estimating cover
# Before filtering, convert from g/m2 to g per quadrat (g/0.25m2) to match previous code
data_presence = data[data$biomass_observed/4 >= 0.1 & data$biomass_predicted/4 >= 0.1,]

# Calculate relative percent difference
# https://stats.stackexchange.com/questions/86708/how-to-calculate-relative-error-when-the-true-value-is-zero
data_presence$RPD = (abs(data_presence$biomass_observed - data_presence$biomass_predicted) / ((abs(data_presence$biomass_observed) + abs(data_presence$biomass_predicted))/2)) * 100
# Produces NaN if biomass_observed, biomass_predicted and error are all 0 -- in these cases there is no error and RPD should be 0 as well
data_presence$RPD[data_presence$error == 0] = 0

# Calculate "relative" error
# https://stats.stackexchange.com/questions/86708/how-to-calculate-relative-error-when-the-true-value-is-zero
# https://stats.stackexchange.com/questions/307375/how-to-calculate-the-relative-error-of-a-measurement-for-close-to-zero-values
# Not truly "relative" as it will be different depending on the units (because of the + 1), but okay if not comparing across units and using well scaled units (distribution of values close to 1, not orders of magnitude different)
# This "relative" error term did the best job eliminating any correlation between the error metric and raw cover
data_presence$relative_error = (abs(data_presence$biomass_observed - data_presence$biomass_predicted) / (data_presence$biomass_observed))

# Make sure relative error is zero wherever raw error is zero
data_presence$relative_error[data_presence$error == 0] = 0

######################################################################################################
######################################################################################################

# 5. GRAPH PRESENCE (RPD) ------------------------------------------------------

# 5.1 PFT ------------------------------------------------------

# Test for independence of observations using intraclass correlation coefficient (ICC) --
ICC::ICCbare(data_presence$site_code, data_presence$RPD)
ICC::ICCest(data_presence$site_code, data_presence$RPD)
# Closer to 0 = less correlation within groups
# Closer to 1 = more correlation within groups
# ICC is very small (close to 0) which indicates that we do not have an issue with pseudoreplication/independence of observations

# Initial test to see if significant differences exist between groups --
# Cannot use ANOVA because our data does not meet the assumptions for linear modeling
# Use Kruskal-Wallis
kruskal.test(RPD ~ PFT, data = data_presence)
# P-value < 0.05, there are significant differences between groups

# Determine which groups have differences
# Dunn's test (non-parametric)
stat.test = data_presence %>% rstatix::dunn_test(RPD ~ PFT, p.adjust.method = "bonferroni")
stat.test = stat.test %>% rstatix::add_xy_position(x = "PFT")
stat.test$p.adj =  signif(stat.test$p.adj, 3)

# Get sample sizes
sample_size = data_presence %>% 
  dplyr::group_by(PFT) %>% 
  dplyr::tally()

# Plot
PFT =
  ggplot(data_presence, aes(x = as.factor(PFT),  y = RPD))+
  geom_boxplot(aes(fill = PFT), notch = TRUE)+
  scale_fill_manual(values = colors)+
  geom_text(data = sample_size, aes(PFT, -Inf, label = n), vjust = -0.1, size = geom_text_size)+
  labs(y = 'Relative Percent Difference', x = '', fill = 'Plant Functional Type')+
  stat_pvalue_manual(stat.test, label = "p.adj", y.position = 210, color = 'black', hide.ns = TRUE, tip.length = 0.01, step.increase = 0.06, size = geom_text_size)+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), axis.text.x=element_blank(), title = element_text(size=title_text_size), legend.justification = c(1, 0.5))

PFT

# Save
if(output_results){
  
  ggsave(
    paste0(outPath, outNamePFT),
    PFT,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 600
  )
  
}

# 5.2.2 VEGETATION COMMUNITY -- COARSE------------------------------------------------------

# Filter data ---
veg_data = data.frame(data_presence %>% 
  dplyr::group_by(viereck_coarse) %>% 
  dplyr::filter(n() > 1))

veg_data = droplevels(veg_data)

# Determine which groups have differences
# Dunn's test (non-parametric)
stat.test = veg_data %>% rstatix::dunn_test(RPD ~ viereck_coarse, p.adjust.method = "bonferroni")
stat.test = stat.test %>% rstatix::add_xy_position(x = "viereck_coarse")
stat.test$p.adj =  signif(stat.test$p.adj, 3)

# Get sample sizes
sample_size = veg_data %>% 
  dplyr::group_by(viereck_coarse) %>% 
  dplyr::tally()

# Custom palette
nb.cols = length(unique(veg_data$viereck_coarse))
mycolors = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(nb.cols)

# Plot
veg_comm =
  ggplot(veg_data, aes(x = as.factor(viereck_coarse),  y = RPD))+
  geom_boxplot(aes(fill = viereck_coarse))+
  scale_fill_manual(values = mycolors)+
  geom_text(data = sample_size, aes(viereck_coarse, -Inf, label = n), vjust = -0.1, size = geom_text_size)+
  labs(y = 'Relative Percent Difference', x = '', fill = 'Vegetation Community Type')+
  stat_pvalue_manual(stat.test, label = "p.adj", y.position = 195 , color = 'black', hide.ns = TRUE, tip.length = 0.01, step.increase = 0.05, size = geom_text_size)+
  theme_minimal()+
  theme(text = element_text(size=theme_text_size), axis.text.x=element_blank(), title = element_text(size=title_text_size))

veg_comm

# Save
if(output_results){
  
  ggsave(
    paste0(outPath, outNameVegComm),
    veg_comm,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 600
  )
  
}

# 5.2.3 VEGETATION COMMUNITY -- COARSE -- FACET PFT ------------------------------------------------------

veg_data = data_presence

# Get PFT/veg comm cover averages
# Use all data, not just presence data
avg_cover = data.frame(data %>% 
  dplyr::group_by(PFT, viereck_coarse) %>% 
  dplyr::summarise(cover_mean=mean(cover_percent)))
veg_data = dplyr::left_join(veg_data, dplyr::select(avg_cover, PFT, viereck_coarse, cover_mean), by = c('PFT', 'viereck_coarse'))

# Abbreviate vegetation community types
levels(veg_data$viereck_coarse) = c("Cl. low scr.", "Cl. tall scr.", "Dry forb", "Dry gram.", "Dryas dw. scr.", "Eric. dw. scr.", "Lichen", "Mesic forb", "Mesic gram.", "Ndl. wood.", "Open low scr.", "Open tall scr.", "Sparse veg.", "Wet gram.", "Willow dw. scr.")
droplevels(veg_data)

# Filter data
veg_data = veg_data %>% 
  dplyr::group_by(viereck_coarse, PFT) %>% 
  dplyr::filter(n() > 2)
veg_data = droplevels(veg_data)

# Determine which groups have differences
# Dunn's test (non-parametric)
stat.test = veg_data %>%
  dplyr::group_by(PFT) %>%
  rstatix::dunn_test(RPD ~ viereck_coarse, p.adjust.method = "bonferroni")
stat.test = stat.test %>% rstatix::add_y_position()
stat.test$p.adj =  signif(stat.test$p.adj, 3)

yLookup = data.frame(PFT = c('DECIDUOUS SHRUBS', 'EVERGREEN SHRUBS', 'FORBS', 'GRAMINOIDS', 'LICHENS'),
                     y.position = c(190, 190, 1.25, 200, 190))

stat.test$y.position = stat.test$PFT %l% yLookup

# Get sample sizes
sample_size = veg_data %>% 
  dplyr::group_by(viereck_coarse, PFT) %>% 
  dplyr::tally()

# Plot - shade boxplots by average cover percent
veg_comm_pft =
  ggplot(veg_data, aes(x = as.factor(viereck_coarse),  y = RPD))+
  geom_boxplot(aes(fill = cover_mean * 4))+
  facet_wrap(~ PFT, scales = 'free')+
  geom_text(data = sample_size, aes(viereck_coarse, -Inf, label = n), vjust = -0.1, size = geom_text_size)+
  labs(y = 'Relative Percent Difference', fill = expression(paste('Biomass (g ', m^-2, ')')), x = '', caption = 'Key', tag = 'cl. = closed\ndw. = dwarf\neric. = Ericaceous\ngram. = graminoid\nndl. = needleleaf\nscr. = scrub\nveg. = vegetated\nwood. = woodland')+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position = 195, hide.ns = TRUE, tip.length = 0, step.group.by = 'PFT', step.increase = 0.08, size = 8)+
  theme_minimal()+
  scale_fill_gradient(low='lightgoldenrod1', high='forestgreen')+
  guides(fill = guide_colorbar(frame.colour = 'black', frame.linewidth = 2, ticks.colour = 'black', ticks.linewidth = 2))+
  theme(plot.tag.position = c(0.703, 0.19), plot.tag = element_text(size = 20, hjust = 0), plot.caption = element_text(hjust = 0.715, vjust = 48, size = 26), text = element_text(size=theme_text_size), axis.text.x = element_text(color = "grey20", size = 18, angle = 45, hjust = 1), title = element_text(size=title_text_size), legend.position = c(0.868, 0.22), legend.justification = c(1,0), legend.title = element_text(size = 26), legend.text = element_text(size = 22))

veg_comm_pft

# Save
if(output_results){
  
  ggsave(
    paste0(outPath, outNameVegCommPFT),
    veg_comm_pft,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 600
  )
  
}
