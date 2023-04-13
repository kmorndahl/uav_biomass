######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script calculates total volume for each PFT within each quadrat
# See manuscript reference in the README.md file for more details on methodology

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outName = 'volume_data.csv'

output_results = FALSE

outPath = '' # Set output directory if desired

######################################################################################################
######################################################################################################

library(sf)
library(raster)

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

# Load quadrat shapefile
quadratPath = "data/UAV_quadrats_final.shp"
allQuadrats = sf::st_read(quadratPath)
allQuadrats = allQuadrats[allQuadrats$quadrat != '89.5m',] # Remove 89.5m quadrat (only relevant for Wickersham)
allQuadrats$quadrat = droplevels(as.factor(allQuadrats$quadrat)) # Drop levels

# Get all volume rasters
volPaths = list.files(path = outPath, pattern = "_vol_cm.tif$", full.names = TRUE)

# Get classified rasters
classifiedPaths = list.files(path = "*/UAV_classification/results", pattern = "classified_final.tif$", full.names = TRUE)

# Lookup information
PFTcolors = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine')
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))

######################################################################################################
######################################################################################################

# Set up output data frame
volumeTotals = data.frame()

# 2. LOOP THROUGH VOLUME RASTERS ------------------------------

for(volPath in volPaths){ # START vol LOOP
  
  pathPieces = strsplit(volPath, '/')[[1]]
  
  file = pathPieces[length(pathPieces)]
  
  filePieces = strsplit(file, '_')[[1]]
  
  site = filePieces[1]
  
  print(paste0("==================== CURRENT SITE: ", site, " ===================="))
  cat("\n")
  
  # Read in raster
  vol = raster::raster(volPath)
  print(paste0("Volume raster read in, the resolution is: ", round(raster::xres(vol), 4), " m"))
  cat("\n")
  
  print('The volume raster is:')
  print(vol)
  cat("\n")
  
  # Get classified raster
  classified = raster::raster(classifiedPaths[grep(site, classifiedPaths)])
  print('The classified raster is:')
  print(classified)
  cat("\n")
  
  # Get site data
  quadrats = allQuadrats[allQuadrats$site_code == site,] # Get site quadrats

  ######################################################################################################
  ######################################################################################################
  
  # 3. PROJECT IF NECESSARY ------------------------------
  
  # Check projections
  georef = raster::crs(classified) # Get georeference of representative biomass raster (first one in list, they should all have the same georeference information)
  if(raster::compareCRS(georef, crs(quadrats)) == FALSE){
    print('Quadrat CRS different from classified CRS, reprojecting...')
    quadrats = sf::st_transform(quadrats, crs = georef)
    print('Quadrat reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  if(raster::compareCRS(georef, crs(vol)) == FALSE){
    print('vol CRS different from classified CRS, reprojecting...')
    vol = raster::projectRaster(vol, crs = georef)
    print('vol reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  cat("/n")
  
  ######################################################################################################
  ######################################################################################################
  
  # 4. LOOP THROUGH QUADRATS ------------------------------
  
  for(i in 1:nrow(quadrats)){ # START QUADRAT LOOP
    
    # 4.1 SUBSET DATA ------------------------------
    
    # Extract single quadrat
    quad = quadrats[i ,]
    quadName = quad$quadrat
    print(paste0("==================== CURRENT QUADRAT: ", quadName, " ===================="))
    cat("\n")
    
    # 4.2 CROP AND PLOT DATA ------------------------------
    
    # Crop rasters
    quadClassified = try(raster::crop(classified, quad))
    
    if(class(quadClassified) == "try-error"){
      print('Current quadrat does not overlap valid classified data, skipping to next quadrat...')
      next
    }
      
    quadClassified = raster::mask(quadClassified, quad)

    quadVol = try(raster::crop(vol, quad))
    
    if(class(quadVol) == "try-error"){
      print('Current quadrat does not overlap valid volume data, skipping to next quadrat...')
      next
    }

    quadVol = raster::mask(quadVol, quad)
    
    print('Rasters cropped to quadrat')
    cat("\n")
    
    # Align rasters for further analysis -- NEW WAY
    quadClassified = raster::aggregate(x=quadClassified, fact=xres(quadVol)/xres(quadClassified), fun = modal)
    quadClassified = raster::resample(x=quadClassified, y=quadVol, method="ngb")
    
    print('Rasters aligned to each other')
    cat("\n")

    # 4.3 EXTRACT DATA FOR INDIVIDUAL PFTS ------------------------------
    
    for(val in unique(quadClassified)){
      
      PFTname = gsub(" ", "_", as.character(PFTlookup$PFT[PFTlookup$ID == val]))
      
      print(paste0("==================== CURRENT PFT: ", PFTname, " ===================="))
      cat("\n")
      
      # Mask to PFT
      PFTmask = quadClassified
      PFTmask[PFTmask != val] = NA
      PFTvol = raster::mask(quadVol, PFTmask)
      print('Volume raster masked')
      cat("/n")
      
      # Extract data 
      dataVol = raster::extract(PFTvol, quad)[[1]]
      print('Data extracted within quadrat: ')
      print(head(dataVol))
      cat("\n")
      
      # Sum data
      sumVol = sum(dataVol[is.finite(dataVol)])
      if(!is.finite(sumVol)){sumVol = 0}
      print(paste0('The total volume is: ', sumVol))
      cat("\n")
      
      # Arrange data in data frame
      PFTname = gsub("_", " ", PFTname)
      df = data.frame(site_code = site, quadrat_num = quadName, PFT = PFTname, volume = sumVol)
      print("Final data:")
      print(df)
      cat("\n")
      
      # Add data to master data frame
      volumeTotals = rbind(volumeTotals, df)
      
      print(paste0("==================== END DATA EXTRACTION FOR ", quadName, " ", PFTname, " ===================="))
      cat("\n")
      
    } # END PFT LOOP
  } # END QUADRAT LOOP
} # END vol LOOP

setwd(outPath)

if(output_results){write.csv(volumeTotals, paste0(outPath, outName))}

print("Results written to disk")
cat("\n")
