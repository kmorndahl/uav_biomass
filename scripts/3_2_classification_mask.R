######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outPath = "*/UAV_classification/results"

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

# 2. LOOP THROUGH CLASSIFIED RASTERS ------------------------------

# 2.1 GET SITE
# 2.2 GET RASTERS
# 2.3 ALIGN RASTERS
# 2.3.1 GET ALIGNMENT PARAMETERS
  # 2.3.2 REPROJECT
  # 2.3.3 RESAMPLE
  # 2.3.4 CROP
  # 2.3.5 CHECK ALIGNMENT AND REPORT FINAL RASTERS
# 2.4 MASK
# 2.5 SAVE RESULTS

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

library(raster)

# Gather classified imagery
classifiedPaths = list.files(path = "*/UAV_classification/results", pattern = "_classified.tif$", full.names = TRUE)
print("Classified rasters are:")
print(classifiedPaths)
cat("\n")

# Gather RGB imagery
RGBpaths = list.files(path = "*/UAV_classification/imagery", pattern = "transparent_mosaic_group1.tif$", full.names = TRUE)
print("RGB rasters are:")
print(RGBpaths)
cat("\n")

# Gather multispectral imagery
msPaths = list.files(path = "*/UAV_classification/imagery", pattern = "reflectance_green.tif$", full.names = TRUE)
print("Multispectral rasters are:")
print(msPaths)
cat("\n")

print(length(classifiedPaths))
print(length(RGBpaths))
print(length(msPaths))

# Specify site list
sites = c("AMSUMMIT", "AUFEIS", "BABBAGE", "BLOWRIVER", "BORDER",  "BROWNSCREEK", "BUTLER", "CALF", "CARIBOUCROSSING", "COALCREEK", "DEW", "EXCELSIOR", "FIRTH", "FOSSILCREEK", "FRYINGPAN", "GRANITE", "GULCH", "JAMESCREEK", "KINGSOLOMON", "LOWERQUARRY", "MATSONCREEK", "MOLLYCREEK", "MOSQUITOCREEK", "OBRIAN", "OGILVIE", "PAGE", "PEDRO", "PINNELL", "PORCUPINECREEK", "PRINDLE", "QUARTZ02", "SCHAR", "SEELA", "SHEEPCREEK", "SIXTYMILE", "SNOWFENCE", "SOUTHSTONEBOY", "SPRINGRIVER", "STONE01", "STONE02", "SWEDE", "THISTLE", "TOPOFTHEWORLD", "TOWYK", "TWELVEMILE", "UPPERPALDO", "UPPERQUARRY", "WHEATEAR", "WICKERSHAM")

######################################################################################################
######################################################################################################

# 2. LOOP THROUGH CLASSIFIED RASTERS ------------------------------

for(path in classifiedPaths){

  # 2.1 GET SITE ------------------------------

  pathPieces = strsplit(path, '/')[[1]]
  fileName = pathPieces[length(pathPieces)]
  site = strsplit(fileName, '_')[[1]][1]

  # Skip to next iteration if site not in site list
  if(!site %in% sites){
    print('Current site is not in site list, skipping...')
    cat("\n")
    next
  }

  # 2.2 GET RASTERS ------------------------------

  classified = raster::raster(path)
  print("Classified raster is:")
  print(classified)
  cat("\n")

  rgb = raster::raster(RGBpaths[grep(site, RGBpaths)])
  print("RGB raster is:")
  print(rgb)
  cat("\n")

  ms = raster::raster(msPaths[grep(site, msPaths)])
  print("Multispectral raster is:")
  print(ms)
  cat("\n")

  # 2.3 ALIGN RASTERS ------------------------------

  # 2.3.1 GET ALIGNMENT PARAMETERS

  # Target georeference
  georef = crs(classified)
  print('The target georeference is: ')
  print(georef)
  cat("\n")

  # Target resolution
  res.ref = raster::xres(classified)
  print('The target resolution is: ')
  print(res.ref)
  cat("\n")

  # 2.3.2 REPROJECT

  if(raster::compareCRS(crs(rgb), georef) == FALSE){
    print(paste0('Raster ', names(rgb), ' CRS different from classified CRS, reprojecting...'))
    rgb = raster::projectRaster(rgb, classified)
    print(paste0('Raster ', names(rgb), ' reprojected:'))
    print(rgb)
    cat("\n")
  }else{
    print('Reprojection not necessary')
    cat("\n")}

  if(raster::compareCRS(crs(ms), georef) == FALSE){
    print(paste0('Raster ', names(ms), ' CRS different from classified CRS, reprojecting...'))
    ms = raster::projectRaster(ms, classified)
    print(paste0('Raster ', names(ms), ' reprojected:'))
    print(ms)
    cat("\n")
  }else{
    print('Reprojection not necessary')
    cat("\n")}

  # 2.3.3 RESAMPLE

  if(raster::xres(rgb) != res.ref){
    print(paste0('Raster ', names(rgb), ' different resolution than classified, resampling...'))
    rgb = raster::resample(rgb, classified, method = 'bilinear')
    print(paste0('Raster ', names(rgb), ' resampled:'))
    print(rgb)
    cat("\n")
  }else{
    print('Resampling not necessary')
    cat("\n")}

  if(raster::xres(ms) != res.ref){
    print(paste0('Raster ', names(ms), ' different resolution than classified, resampling...'))
    ms = raster::resample(ms, classified, method = 'bilinear')
    print(paste0('Raster ', names(ms), ' resampled:'))
    print(ms)
    cat("\n")
  }else{
    print('Resampling not necessary')
    cat("\n")}

  # 2.3.4 CROP

  # Get minimum extent
  e = raster::intersect(raster::intersect(raster::extent(classified), raster::extent(rgb)), raster::extent(ms))

  print('Minimum extent calculated, the minimum extent is:')
  print(e)
  cat("\n")

  rgb = raster::crop(rgb, e)
  print('RGB raster cropped to minimum extent:')
  print(rgb)
  cat("\n")

  ms = raster::crop(ms, e)
  print('Multispectral raster cropped to minimum extent:')
  print(ms)
  cat("\n")

  classified = raster::crop(classified, e)
  print('Classified raster cropped to minimum extent:')
  print(classified)
  cat("\n")

  # 2.3.5 CHECK ALIGNMENT AND REPORT FINAL RASTERS
  # Even if projection and resolution match and rasters have been clipped to same extent, truncated cells can cause compareRaster errors

  rgbCheck = try(raster::compareRaster(classified, rgb))
  msCheck = try(raster::compareRaster(classified, ms))

  if(class(rgbCheck) == "try-error"){
    print(paste0('Slight mismatch between raster ', names(rgb), ' and classified raster causing compareRaster() error, resampling...'))
    rgb = raster::resample(rgb, classified, method = 'bilinear')
    print(paste0('Raster ', names(rgb), ' resampled'))
    cat("\n")
  }
  if(class(msCheck) == "try-error"){
    print(paste0('Slight mismatch between raster ', names(ms), ' and classified raster causing compareRaster() error, resampling...'))
    ms = raster::resample(ms, classified, method = 'bilinear')
    print(paste0('Raster ', names(ms), ' resampled'))
    cat("\n")
  }

  print("Aligned classified raster is:")
  print(classified)
  cat("\n")

  print("Aligned RGB raster is:")
  print(rgb)
  cat("\n")

  print("Aligned multispectral raster is:")
  print(ms)
  cat("\n")

  # 2.4 MASK ------------------------------

  classified = try(raster::mask(classified, rgb))
  if(class(classified) != "try-error"){
    print("Classified raster masked to RGB:")
    print(classified)
    cat("\n")
  }

  classified = try(raster::mask(classified, ms))
  if(class(classified) != "try-error"){
    print("Classified raster masked to multispectral:")
    print(classified)
    cat("\n")
  }

  # 2.5 SAVE RESULTS ------------------------------

  if(class(classified) != "try-error"){ # Only write output to disk if mask was successful

  setwd(outPath)

  outClassified = paste0(site, '_classified_final.tif')

  raster::writeRaster(classified, outClassified)

  print("Results written to disk")
  cat("\n")

  }

}