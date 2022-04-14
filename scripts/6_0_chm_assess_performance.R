######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

dir = "*/0_UAV_final/data/"
setwd(dir)

quadratName = "UAV_quadrats_final.shp"
outName = 'height_UAV_data.csv'

classifiedPath = "*/UAV_classification/results"
chmPath = "*/UAV_chm/results"

######################################################################################################
######################################################################################################

library(sf)
library(raster)
library(dplyr)

######################################################################################################
######################################################################################################

# 1. READ IN GEOSPATIAL DATA ------------------------------

# Load quadrat shapefile
allQuadrats = sf::st_read(quadratName)

# Get all CHMs
chmPaths = list.files(path = chmPath, pattern = "CHM_final.tif$", full.names = TRUE)

# Get classified rasters
classifiedPaths = list.files(path = classifiedPath, pattern = "classified_final.tif$", full.names = TRUE)

# Setup lookup table
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))

######################################################################################################
######################################################################################################

# 2. LOOP THROUGH CHMs ------------------------------

# 2.1 SET UP OUTPUT DATA FRAME ------------------------------

heightTotals = data.frame()

# 2.2 START LOOP ------------------------------

for(chmPath in chmPaths){ # START CHM LOOP
  
  # 2.3 GRAB REQUIRED FILES ------------------------------
  
  pathPieces = strsplit(chmPath, '/')[[1]]
  
  file = pathPieces[length(pathPieces)]
  
  filePieces = strsplit(file, '_')[[1]]
  
  site = filePieces[1]
  
  print(paste0("==================== CURRENT SITE: ", site, " ===================="))
  cat("\n")
  
  # Read in raster
  chm = raster::raster(chmPath)
  print(paste0("CHM read in, the resolution is: ", round(xres(chm), 4), " m"))
  cat("\n")
  
  print('The CHM is:')
  print(chm)
  cat("\n")
  
  # Get classified raster
  classified = raster::raster(classifiedPaths[grep(site, classifiedPaths)])
  print('The classified raster is:')
  print(classified)
  cat("\n")
  
  # Get site data
  quadrats = allQuadrats[allQuadrats$site_code == site,] # Get site quadrats

  # 2.4 REPROJECT IF NECESSARY ------------------------------
  
  # Check projections
  georef = crs(classified) # Get georeference of representative biomass raster (first one in list, they should all have the same georeference information)
  if(raster::compareCRS(georef, crs(quadrats)) == FALSE){
    print('Quadrat CRS different from classified CRS, reprojecting...')
    quadrats = sf::st_transform(quadrats, crs = georef)
    print('Quadrat reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  if(raster::compareCRS(georef, crs(chm)) == FALSE){
    print('CHM CRS different from classified CRS, reprojecting...')
    chm = raster::projectRaster(chm, crs = georef)
    print('CHM reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  cat("/n")
  
  ######################################################################################################
  ######################################################################################################
  
  # 3. LOOP THROUGH QUADRATS ------------------------------
  
  for(i in 1:nrow(quadrats)){ # START QUADRAT LOOP
    
    # 3.1 SUBSET DATA ------------------------------
    
    # Extract single quadrat
    quad = quadrats[i ,]
    quadName = quad$quadrat
    print(paste0("==================== CURRENT QUADRAT: ", quadName, " ===================="))
    cat("\n")
    
    # 3.2 CROP AND PLOT DATA ------------------------------
    
    # Crop rasters
    quadClassified = try(raster::crop(classified, quad))
    
    if(class(quadClassified) == "try-error"){
      print('Current quadrat does not overlap valid classified data, skipping to next quadrat...')
      next
    }
    
    quadClassified = raster::mask(quadClassified, quad)
    
    quadCHM = try(raster::crop(chm, quad))
    
    if(class(quadCHM) == "try-error"){
      print('Current quadrat does not overlap valid CHM data, skipping to next quadrat...')
      next
    }
    
    quadCHM = raster::mask(quadCHM, quad)
    
    print('Rasters cropped to quadrat')
    cat("\n")
    
    # Align rasters for further analysis
    quadClassified = raster::aggregate(x=quadClassified, fact=xres(quadCHM)/xres(quadClassified), fun = modal)
    quadClassified = raster::resample(x=quadClassified, y=quadCHM, method="ngb")
    
    print('Rasters aligned to each other')
    cat("\n")
    
    # 3.3 EXTRACT DATA FOR INDIVIDUAL PFTS ------------------------------
    
    for(val in unique(quadClassified)){
      
      PFTname = gsub(" ", "_", as.character(PFTlookup$PFT[PFTlookup$ID == val]))
      
      print(paste0("==================== CURRENT PFT: ", PFTname, " ===================="))
      cat("\n")
      
      # Mask to PFT
      PFTmask = quadClassified
      PFTmask[PFTmask != val] = NA
      PFTchm = raster::mask(quadCHM, PFTmask)
      print('CHM masked')
      cat("/n")
      
      # Extract data 
      dataCHM = raster::extract(PFTchm, quad)[[1]]
      print('Data extracted within quadrat: ')
      print(head(dataCHM))
      cat("\n")
      
      # Average data
      meanCHM = mean(dataCHM[is.finite(dataCHM)])
      if(!is.finite(meanCHM)){meanCHM = 0}
      print(paste0('The average height for final CHM is: ', meanCHM))
      cat("\n")
      
      # Max data
      maxCHM = max(dataCHM[is.finite(dataCHM)])
      if(!is.finite(maxCHM)){maxCHM = 0}
      print(paste0('The maximum height for final CHM is: ', maxCHM))
      cat("\n")
      
      # Arrange data in data frame
      PFTname = gsub("_", " ", PFTname)
      dataValue = c(meanCHM, maxCHM)
      dataSummaryType = c("mean", "max")
      df = data.frame(site_code = rep(site, length(dataValue)), quadrat_num = rep(quadName, length(dataValue)), PFT = rep(PFTname, length(dataValue)), summaryType = as.factor(dataSummaryType), height = as.numeric(dataValue))
      print("Final data:")
      print(df)
      cat("\n")
      
      # Add data to master data frame
      heightTotals = rbind(heightTotals, df)
      
      print(paste0("==================== END DATA EXTRACTION FOR ", quadName, " ", PFTname, " ===================="))
      cat("\n")
      
    } # END PFT LOOP
  } # END QUADRAT LOOP
} # END CHM LOOP

write.csv(heightTotals, outName, row.names = FALSE)