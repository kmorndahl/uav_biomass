######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script creates the fills the final canopy height models
# Gaps in the primary canopy height model are filled with data from the random forest derived canopy height model
# See manuscript reference in the README.md file for more details on methodology

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outPath = "*/UAV_chm/results/"

classifiedPath = "*/UAV_classification/results"

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

library(raster)
library(lidR)

# Gather CHMs
chmPaths = list.files(path = outPath, pattern = "_CHM.tif$", full.names = TRUE)
print("Primary CHMs are:")
print(chmPaths)
cat("\n")

# Gather fill CHMs
chmRFpaths = list.files(path = outPath, pattern = "_CHM_RF.tif$", full.names = TRUE)
print("Random forest CHMs are:")
print(chmRFpaths)
cat("\n")

# Gather classifed rasters
classifiedPaths = list.files(path = classifiedPath, pattern = "_classified_final.tif$", full.names = TRUE)
print("Classified rasters are:")
print(classifiedPaths)
cat("\n")

# Increase raster memory limit
raster::rasterOptions(maxmemory = 1e+11) # 100 GB
print(rasterOptions())

# Get bash parameters
params = commandArgs(trailingOnly = TRUE)

######################################################################################################
######################################################################################################

# 2. GET SITE ------------------------------

site = params[1]

print(paste0("==================== CURRENT SITE: ", site, " ===================="))
cat("\n")

######################################################################################################
######################################################################################################

# 3. GET RASTERS ------------------------------

chm = raster::raster(chmPaths[grep(site, chmPaths)])
names(chm) = 'height'

print("Primary CHM is:")
print(chm)
cat("\n")

chmRF = raster::raster(chmRFpaths[grep(site, chmRFpaths)])
names(chmRF) = 'height'

print("Random forest CHM is:")
print(chmRF)
cat("\n")

classified = raster::raster(classifiedPaths[grep(site, classifiedPaths)])

print("Classified raster is:")
print(classified)
cat("\n")

######################################################################################################
######################################################################################################

# 4. ALIGN RASTERS ------------------------------

# 4.1 GET ALIGNMENT PARAMETERS

# Target georeference
georef = crs(chm)
print('The target georeference is: ')
print(georef)
cat("\n")

# Get minimum extent
e = raster::intersect(raster::extent(chm), raster::extent(chmRF))

print('Minimum extent calculated, the minimum extent is:')
print(e)
cat("\n")

# Target resolution
res.ref = raster::xres(chm)
print('The target resolution is: ')
print(res.ref)
cat("\n")

# 4.2 REPROJECT

if(raster::compareCRS(crs(chmRF), georef) == FALSE){
  print(paste0('Raster ', names(chmRF), ' CRS different from main CHM CRS, reprojecting...'))
  chmRF = raster::projectRaster(chmRF, chm)
  print(paste0('Raster ', names(chmRF), ' reprojected'))
  cat("\n")
}else{
  print('Reprojection not necessary')
  cat("\n")}

# 4.3 RESAMPLE

if(raster::xres(chmRF) != res.ref){
  print(paste0('Raster ', names(chmRF), ' different resolution than main CHM, resampling...'))
  chmRF = raster::resample(chmRF, chm, method = 'bilinear')
  print(paste0('Raster ', names(chmRF), ' resampled'))
  cat("\n")
}else{
  print('Resampling not necessary')
  cat("\n")}

# 4.4 CROP

chm = raster::crop(chm, e)
print('Primary CHM cropped to minimum extent')
cat("\n")

chmRF = raster::crop(chmRF, e)
print('Random forest CHM cropped to minimum extent')
cat("\n")

# 4.5 CHECK ALIGNMENT AND REPORT FINAL RASTERS
# Even if projection and resolution match and rasters have been clipped to same extent, truncated cells can cause compareRaster errors

check = try(raster::compareRaster(chm, chmRF))

if(class(check) == "try-error"){
  print(paste0('Slight mismatch between primary CHM and random forest CHM causing compareRaster() error, resampling random forest CHM...'))
  chmRF = raster::resample(chmRF, chm, method = 'bilinear')
  print(paste0('Random forest CHM resampled'))
  cat("\n")
}

######################################################################################################
######################################################################################################

# 6. COVER ------------------------------

chmRF_cover = raster::cover(chm, chmRF)
print('Primary CHM filled with random forest CHM')
cat("\n")

######################################################################################################
######################################################################################################

# 7. APPLY FILTERS ------------------------------
# Focal mean applied to smooth boundaries between original CHM and RF CHM

chmRF_mean = raster::focal(chmRF_cover, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)

print('Focal mean filter applied')
print(chmRF_mean)
cat("\n")

######################################################################################################
######################################################################################################

# 8. FILL PRIMARY RASTER USING FILL RASTERS ------------------------------

chmRF_final = raster::cover(chm, chmRF_mean)

print('Primary CHM filled, final CHM complete')
print(chmRF_final)
cat("\n")

######################################################################################################
######################################################################################################

# 9. SAVE INTERMEDIATE RESULTS ------------------------------

setwd(outPath)

outCHMrf = paste0(site, '_CHM_filled.tif')

raster::writeRaster(chmRF_final, outCHMrf)

print("Results written to disk")
cat("\n")

######################################################################################################
######################################################################################################

# 10. CROP AND MASK FINAL CHM TO FINAL CLASSIFIED ------------------------------

# 10.1 ALIGN RASTERS

# 10.1.1 GET ALIGNMENT PARAMETERS

# Target georeference
georef = crs(chmRF_final)
print('The target georeference is: ')
print(georef)
cat("\n")

# Target resolution
res.ref = raster::xres(chmRF_final)
print('The target resolution is: ')
print(res.ref)
cat("\n")

# 10.1.2 REPROJECT

if(raster::compareCRS(crs(classified), georef) == FALSE){
  print(paste0('Raster ', names(classified), ' CRS different from CHM CRS, reprojecting...'))
  classified = raster::projectRaster(classified, chm)
  print(paste0('Raster ', names(classified), ' reprojected'))
  cat("\n")
}else{
  print('Reprojection not necessary')
  cat("\n")}

# 10.1.3 RESAMPLE

if(raster::xres(classified) != res.ref){
  print(paste0('Raster ', names(classified), ' different resolution than CHM, resampling...'))
  classified = raster::resample(classified, chm, method = 'ngb')
  print(paste0('Raster ', names(classified), ' resampled'))
  cat("\n")
}else{
  print('Resampling not necessary')
  cat("\n")}

# 10.1.4 CROP

# Get minimum extent
e = raster::intersect(raster::extent(chmRF_final), raster::extent(classified))

print('Minimum extent calculated, the minimum extent is:')
print(e)
cat("\n")

chmRF_final = raster::crop(chmRF_final, e)
print('CHM cropped to minimum extent')
cat("\n")

classified = raster::crop(classified, e)
print('Classified raster cropped to minimum extent')
cat("\n")

# 10.1.5 CHECK ALIGNMENT AND REPORT FINAL RASTERS
# Even if projection and resolution match and rasters have been clipped to same extent, truncated cells can cause compareRaster errors

check = try(raster::compareRaster(chmRF_final, classified))

if(class(check) == "try-error"){
  print(paste0('Slight mismatch between raster ', names(classified), ' and CHM causing compareRaster() error, resampling...'))
  classified = raster::resample(classified, chmRF_final, method = 'ngb')
  print(paste0('Raster ', names(classified), ' resampled'))
  cat("\n")
}

print("Aligned CHM is:")
print(chmRF_final)
cat("\n")

print("Aligned classified raster is:")
print(classified)
cat("\n")

# 10.2 MASK ------------------------------

chmRF_final = try(raster::mask(chmRF_final, classified))
if(class(chmRF_final) != "try-error"){
  print("CHM masked to classified:")
  print(chmRF_final)
  cat("\n")
}

######################################################################################################
######################################################################################################

# 11. SAVE FINAL RESULTS ------------------------------

if(class(chmRF_final) != "try-error"){ # Only write output to disk if mask was successful
  
  setwd(outPath)
  
  outCHMfinal = paste0(site, '_CHM_final.tif')
  
  raster::writeRaster(chmRF_final, outCHMfinal)
  
  print("Results written to disk")
  cat("\n")
}
