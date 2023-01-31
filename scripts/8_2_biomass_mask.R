######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script masks the final UAV PFT biomass predictions to match the canopy height models

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outPathFit = "*/UAV_biomass/results_zeros_FINAL/"
outPathCI = "*/UAV_biomass/results_zeros_FINAL/"

######################################################################################################
######################################################################################################

library(raster)

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

# Get CHMs
chmPaths = list.files(path = "*/UAV_chm/results", pattern = "CHM_final.tif$", full.names = TRUE)

# Get all biomass rasters
biomassPaths = list.files(path = "*/UAV_biomass/results_NAs_FINAL", pattern = ".tif$", full.names = TRUE)

######################################################################################################
######################################################################################################

# 2. LOOP THROUGH biomass ------------------------------

for(biomassPath in biomassPaths){ # START biomass LOOP
  
  pathPieces = strsplit(biomassPath, '/')[[1]]
  
  file = pathPieces[length(pathPieces)]
  
  filePieces = strsplit(file, '_')[[1]]
  
  filePieces = filePieces[filePieces != 'fixed']
  
  site = filePieces[1]
  
  PFTend = length(filePieces)-2
  
  if(PFTend < 2){
    PFT = 'TOTAL'
  } else{
    PFT = paste(filePieces[2:PFTend], collapse = ' ')
  }
  
  metric = filePieces[length(filePieces)-1]
  
  print(paste0("==================== CURRENT SITE: ", site, " ===================="))
  cat("\n")
  
  print(paste0("==================== CURRENT PFT: ", PFT, " ===================="))
  cat("\n")
  
  print(paste0("==================== CURRENT METRIC: ", metric, " ===================="))
  cat("\n")
  
  # Read in raster
  biomass = raster::raster(biomassPath)
  print(paste0("Biomass raster read in, the resolution is: ", round(xres(biomass), 4), " m"))
  cat("\n")
  
  print('The biomass raster is:')
  print(biomass)
  cat("\n")
  
  # Get chm raster
  chm = raster::raster(chmPaths[grep(site, chmPaths)])
  print(paste0("chm raster read in, the resolution is: ", round(xres(chm), 4), " m"))
  cat("\n")
  
  print('The chm raster is:')
  print(chm)
  cat("\n")
  
  ######################################################################################################
  ######################################################################################################
  
  # 3. ALIGN DATA ------------------------------
  
  # 3.1 GET ALIGNMENT PARAMETERS
  
  # Target georeference
  georef = crs(biomass)
  print('The target georeference is: ')
  print(georef)
  cat("\n")
  
  # Target resolution
  res.ref = raster::xres(biomass)
  print('The target resolution is: ')
  print(res.ref)
  cat("\n")
  
  # 3.2 REPROJECT
  
  if(raster::compareCRS(georef, crs(chm)) == FALSE){
    print('chm CRS different from biomass CRS, reprojecting...')
    chm = raster::projectRaster(chm, crs = georef)
    print('chm reprojected')
  } else{
    print('CRS match, no reprojection needed')
  }
  
  # 3.3 RESAMPLE
  
  if(raster::xres(chm) != res.ref){
    print('chm raster different resolution than biomass raster, resampling...')
    chm = raster::resample(chm, biomass, method = 'bilinear')
    print('chm raster resampled:')
    print(chm)
    cat("\n")
  }else{
    print('Resampling not necessary')
    cat("\n")}

  # 3.4 CROP
  
  # Get minimum extent
  e = raster::intersect(raster::extent(chm), raster::extent(biomass))
  print('Minimum extent calculated, the minimum extent is:')
  print(e)
  cat("\n")

  biomass = raster::crop(biomass, e)
  chm = raster::crop(chm, e)
  print('Rasters cropped to minimum extent')
  print('Cropped biomass raster:')
  print(biomass)
  print('Cropped chm raster:')
  print(chm)
  cat("\n")
  
  # 3.5 CHECK ALIGNMENT AND REPORT FINAL RASTERS
  # Even if projection and resolution match and rasters have been clipped to same extent, truncated cells can cause compareRaster errors
  
  chmCheck = try(raster::compareRaster(chm, biomass))

  if(class(chmCheck) == "try-error"){
    print('Slight mismatch between biomass raster and chm raster causing compareRaster() error, resampling...')
    chm = raster::resample(chm, biomass, method = 'bilinear')
    print('chm raster resampled')
    cat("\n")
  }
  
  print("Aligned biomass raster is:")
  print(biomass)
  cat("\n")
  
  print("Aligned chm raster is:")
  print(chm)
  cat("\n")
  
  ######################################################################################################
  ######################################################################################################
  
  # 4. MASK DATA ------------------------------
  
  # 4.1 ASSIGN ZEROS TO NO DATA VALUES
  
  biomass = raster::reclassify(biomass, cbind(NA, 0))
  
  # 4.2 MASK TO EXTENT OF CHM
  
  biomass = try(raster::mask(biomass, chm))
  
  if(class(biomass) != "try-error"){
    print("Biomass raster masked to chm:")
    print(biomass)
    cat("\n")
  }
  
  ######################################################################################################
  ######################################################################################################
  
  # 5. SAVE RESULTS ------------------------------
  
  if(class(biomass) != "try-error"){ # Only write output to disk if mask was successful
    
    outBiomass = file
    
    if(grepl('fit', outBiomass)){
      
      setwd(outPathFit)

      raster::writeRaster(biomass, outBiomass)
      
      print("Results written to disk -- fit folder")
      cat("\n")
      
    } else{
      
      setwd(outPathCI)

      raster::writeRaster(biomass, outBiomass)
      
      print("Results written to disk -- ci folder")
      cat("\n")
      
    }
    
  }

} # END biomass LOOP
