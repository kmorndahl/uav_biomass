# UAV plant functional type aboveground biomass mapping

## Project summary

Arctic vegetation communities are rapidly changing with climate warming, which impacts wildlife, carbon cycling and climate feedbacks. Accurately monitoring vegetation change is thus crucial, but scale mismatches between field and satellite-based monitoring cause challenges. Remote sensing from unmanned aerial vehicles (UAVs) has emerged as a bridge between field data and satellite-based mapping. We assessed the viability of using high resolution UAV imagery and UAV-derived Structure from Motion (SfM) to predict cover, height and aboveground biomass (henceforth biomass) of Arctic plant functional types (PFTs) across a range of vegetation community types. We classified imagery by PFT, estimated cover and height, and modeled biomass from UAV-derived volume estimates. Predicted values were compared to field estimates to assess results. Cover was estimated with root-mean-square error (RMSE) 6.29-14.2% and height was estimated with RMSE 3.29-10.5 cm, depending on the PFT. Total aboveground biomass was predicted with RMSE 220.5 g m<sup>-2</sup>, and per-PFT RMSE ranged from 17.14-164.3 g m<sup>-2</sup>. Deciduous and evergreen shrub biomass was predicted most accurately, followed by lichen, graminoid, and forb biomass. Our results demonstrate the effectiveness of using UAVs to map PFT biomass, which provides a link towards improved mapping of PFTs across large areas using earth observation satellite imagery.

### Manuscript citation:

Orndahl, K.M., Ehlers, L.P.W, Herriges, J.D, Pernick, R.E., Hebblewhite, M., Goetz, S.J. (in press).
Mapping tundra ecosystem plant functional type cover, height and aboveground biomass in Alaska
and northwest Canada using unmanned aerial vehicles. Arctic Science.

### Data citation:

Kathleen Orndahl. 2022. Mapping tundra ecosystem plant functional type cover,
height and aboveground biomass in Alaska and northwest Canada using unmanned
aerial vehicles, 2018-2019. doi:10.18739/A2R785Q5B.

## Repository summary

This repository includes scripts and data for mapping plant functional type aboveground biomass from UAV imagery and Structure from Motion point clouds. Intermediary data files produced are also provided. Note that UAV imagery is not provided here, but can be requested from the author.

## Repository details

Scripts are prepended with numbers for organization, see details below for script groupings:

0. Prepare internals (hpc script details)
1. Prepare training data/predictors
2. Classification - preparation
    - Cross validation and accuracy assessment
    - Hyperparameter tuning
    - Feature importance
    - Build final models
3. Classification - execution
    - Classify
    - Mask
4. Classification - assess performance
5. Canopy height model - create
6. Canopy height model - assess performance
7. Volume - create
8. Biomass - predict
    - Prepare biomass/volume training data
    - Predict
    - Mask
9. Biomass - assess performance
10. Analysis - compare performance amongst PFTs, vegetation community types

