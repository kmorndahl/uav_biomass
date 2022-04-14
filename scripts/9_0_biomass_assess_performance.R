######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outPath = "*/UAV_biomass/results_NAs_FINAL/"

setwd(outPath)

######################################################################################################
######################################################################################################

# 1. LOAD DATA ------------------------------

# 1.1 LIBRARIES ------------------------------

library(sf)
library(raster)
library(dplyr)

params = commandArgs(trailingOnly = TRUE)

# 1.2 READ IN DATA AND SET PARAMETERS ------------------------------

# Load quadrat shapefile
quadratPath = "*/0_UAV_final/data/UAV_quadrats_final.shp"
allQuadrats = sf::st_read(quadratPath)

# Get classified rasters
classifiedPaths = list.files(path = "*/UAV_classification/results", pattern = "classified_final.tif$", full.names = TRUE)

# Get all biomass rasters
biomassPathsAll = list.files(path = outPath, pattern = ".tif$", full.names = TRUE)

# Get only total biomass rasters
# Select by excluding SHRUBS GRAMINOIDS LICHENS FORBS
substrings = c('SHRUBS', 'FORBS', 'GRAMINOIDS', 'LICHENS')

biomassPaths = c()
for(path in biomassPathsAll){
  if(!grepl(paste(substrings, collapse="|"), path)){
    biomassPaths = c(biomassPaths, path)
  }
}

print('The list of biomass rasters is:')
print(biomassPaths)
cat("\n")

# Lookup information
PFTcolors = c('darkorange', 'green4', 'cyan4', 'chartreuse3', 'darkseagreen3', 'yellow3', 'burlywood4', 'dimgray', 'mediumaquamarine')
PFTlookup = data.frame(PFT = c("BRYOPHYTES", "DECIDUOUS SHRUBS", "EVERGREEN SHRUBS", "FORBS", "GRAMINOIDS", "LICHENS", "NON VEGETATED", "SHADOW", "TREES"), ID = seq(1,9))

######################################################################################################
######################################################################################################

# 2. GET SITE ------------------------------

site = params[1]

print(paste0("==================== CURRENT SITE: ", site, " ===================="))
cat("\n")

######################################################################################################
######################################################################################################

# 3. GATHER DATA

# Get site biomass paths
siteBiomassPaths = biomassPaths[grep(site, biomassPaths)]
print('The site biomass paths are:')
print(siteBiomassPaths)
cat("\n")

# Set up output data frame
biomassRaster = data.frame()

for(biomassPath in siteBiomassPaths){ # START metric LOOP
  
  # Get biomass raster
  biomass = raster::raster(biomassPath)
  print(paste0("Biomass raster read in, the resolution is: ", round(raster::xres(biomass), 4), " m"))
  print('The biomass raster is:')
  print(biomass)
  cat("\n")
  
  # Get classified raster
  classified = raster::raster(classifiedPaths[grep(site, classifiedPaths)])
  print('The classified raster is:')
  print(classified)
  cat("\n")
  
  # Get metadata
  pathPieces = strsplit(biomassPath, '/')[[1]]
    
  file = pathPieces[length(pathPieces)]
    
  filePieces = strsplit(file, '_')[[1]]
    
  metric = filePieces[2]
    
  print(paste0("==================== CURRENT METRIC: ", metric, " ===================="))
  cat("\n")
  
  # Get site data
  quadrats = allQuadrats[allQuadrats$site_code == site,] # Get site quadrats

  ######################################################################################################
  ######################################################################################################
    
  # 3. PROJECT IF NECESSARY ------------------------------
    
  # Check projections
  georef = raster::crs(biomass) # Get georeference of representative biomass raster (first one in list, they should all have the same georeference information)
  if(raster::compareCRS(georef, crs(quadrats)) == FALSE){
    print('Quadrat CRS different from biomass CRS, reprojecting...')
    quadrats = sf::st_transform(quadrats, crs = georef)
    print('Quadrat reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  if(raster::compareCRS(georef, crs(classified)) == FALSE){
    print('Classified CRS different from biomass CRS, reprojecting...')
    classified = raster::projectRaster(classified, crs = georef)
    print('Classified reprojected')} else{
      print('CRS match, no reprojection needed')
    }
  cat("\n")
    
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
      
    quadBiomass = try(raster::crop(biomass, quad))
      
    if(class(quadBiomass) == "try-error"){
      print('Current quadrat does not overlap valid biomass data, skipping to next quadrat...')
      next
    }
      
    quadBiomass = raster::mask(quadBiomass, quad)
      
    print('Rasters cropped to quadrat')
    cat("\n")
      
    # Align rasters for further analysis -- NEW WAY
    quadClassified = raster::aggregate(x=quadClassified, fact=raster::xres(quadBiomass)/raster::xres(quadClassified), fun = modal)
    quadClassified = raster::resample(x=quadClassified, y=quadBiomass, method="ngb")
    
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
      PFTbiomass = raster::mask(quadBiomass, PFTmask)
      print('Biomass raster masked')
      cat("/n")
        
      # Extract data 
      dataBiomass = raster::extract(PFTbiomass, quad)[[1]]
      print('Data extracted within quadrat: ')
      print(head(dataBiomass))
      cat("\n")
        
      # Sum data
      sumBiomass = sum(dataBiomass[is.finite(dataBiomass)])
      if(!is.finite(sumBiomass)){sumBiomass = 0}
      print(paste0('The total biomass is: ', sumBiomass))
      cat("\n")
        
      # Arrange data in data frame
      PFTname = gsub("_", " ", PFTname)
      df = data.frame(site_code = site, quadrat_num = quadName, metric = metric, PFT = PFTname, weight = as.numeric(sumBiomass))
      print("Final data:")
      print(df)
      cat("\n")
        
      # Add data to master data frame
      biomassRaster = rbind(biomassRaster, df)
      
      print(paste0("==================== END DATA EXTRACTION FOR ", quadName, " ", PFTname, " ===================="))
      cat("\n")
        
    } # END PFT LOOP
  } # END QUADRAT LOOP
}# END METRIC LOOP

setwd(outPath)

write.csv(biomassRaster, paste0(site, '_UAV_biomass.csv'))