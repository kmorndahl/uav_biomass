######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

outPath = '*/UAV_chm/results_base/'

setwd(outPath)

######################################################################################################
######################################################################################################

# 1. LOAD DATA ------------------------------

# 1.1 Libraries
# 1.2 Load data

# 2. READ AND TIDY DATA ------------------------------

# 2.1 Get metadata
# 2.2 Load files
# 2.3 Tidy

# 3. SET PARAMETERS ------------------------------

# 4. CLASSIFY POINT CLOUD ------------------------------

# 4.1 Filter canopy
#   4.1.1 First pass -- highest Z
#   4.1.2 Second pass -- nearest neighbor
# 4.2 Filter ground
#   4.2.1 First pass -- lowest Z
#   4.2.2 Second pass -- nearest neighbor
# 4.3 Merge ground and canopy

# 5. CREATE DTM ------------------------------

# 6. NORMALIZE POINT CLOUD ------------------------------

# 7. CREATE CHM ------------------------------

# 8. SAVE RESULTS ------------------------------

######################################################################################################
######################################################################################################

# 1. LOAD DATA ------------------------------

# 1.1 LIBRARIES ------------------------------

library(raster)
library(lidR)
source('*/0_UAV_final/scripts/UAV_lidR_fxns.R')

params = commandArgs(trailingOnly = TRUE)

# 1.2 LOAD DATA ------------------------------

# Load point cloud
lasPath = params[1]

######################################################################################################
######################################################################################################

# 2. READ AND TIDY DATA ------------------------------

# 2.1 GET METADATA ------------------------------

pathPieces = strsplit(lasPath, '/')[[1]]

file = pathPieces[length(pathPieces)]

site = strsplit(file, '_')[[1]][1]

print(paste0("==================== CURRENT SITE: ", site, " ===================="))
cat("\n")

# 2.2 LOAD FILES ------------------------------

# Get site .las
print(paste0('The .las file is: ', lasPath))
las = lidR::readLAS(lasPath)
print(".las file read in as LAS")
cat("\n")

# 2.3 TIDY ------------------------------

# Report point cloud metadata
print('The point cloud is:')
print(las)
cat("\n")

# Report sample of point cloud data
print('The point cloud data is:')
print(las@data)
cat("\n")

# Report number of points
print(paste0('Number of points is: ', nrow(las@data)))
cat("\n")

# Need 'ReturnNumber' attribute to be all 1s instead of all 0s for analysis in lidR to work
ones = rep(1, length(las$ReturnNumber))
las$ReturnNumber = as.integer(ones)
print("las ReturnNumber fixed")
cat("\n")

######################################################################################################
######################################################################################################

# 3. SET PARAMETERS ------------------------------

# Increase raster memory limit
raster::rasterOptions(maxmemory = 1e+11) # 100 GB
print(rasterOptions())

# # Get relevant attributes from site .las file -- lidR version < 2.1.0
# median_pt_dens = median(grid_density(las, res = 1)@data@values, na.rm = TRUE) # Find the median point density (per 1 m ^ 2)
# pt_spacing = 1/(sqrt(median_pt_dens)) # Find the point spacing, given as: point spacing =1/sqrt(point density)
# print(paste0('Point cloud point spacing is: ', pt_spacing))
# cat("\n")

# Get relevant attributes from site .las file -- lidR version >= 2.1.0
median_pt_dens = density(las) # Find the median point density (per 1 m ^ 2)
pt_spacing = 1/(sqrt(median_pt_dens)) # Find the point spacing, given as: point spacing =1/sqrt(point density)
print(paste0('Point cloud point spacing is: ', pt_spacing))
cat("\n")

# Set CHM parameters
canopy.res = pt_spacing
ground.res = pt_spacing
nn = 4
subcircle.multiplier = 0.5
max.edge = 0.2
chm.res = pt_spacing
print(paste0('Output resolution will be: ', chm.res))
cat("\n")

######################################################################################################
######################################################################################################

# 4. CLASSIFY POINT CLOUD ------------------------------

# 4.1 FILTER CANOPY ------------------------------

print(paste0('Canopy filter resolution is: ', canopy.res))
print(paste0('Number of neighbors to search is: ', nn))
cat("\n")

# 4.1.1 canopy filter first pass -- highest Z

canopy.points = grid.filter.canopy(las) # Local maximum function, uses canopy.res

print(paste0('Canopy points filtered to highest Z (first pass), number of canopy points is: ', nrow(canopy.points)))
cat("\n")

# 4.1.2 canopy filter second pass -- nearest neighbor

canopy.points = try(filter.to.highest.nn(canopy.points, k=nn))  # Nearest neighbor filtering

if(class(canopy.points)[1] == 'try-error'){
  stop("Filtering to nearest neighbor failed")
  cat("\n")
} else{
  print(paste0('Canopy points filtered to nearest neighbor (second pass), number of canopy points is: ', nrow(canopy.points)))
  cat("\n")
}

canopy.points$Classification = as.integer(1)
canopy = las
canopy@data = canopy.points # Save data table of points to las

# 4.2 FILTER GROUND ------------------------------

print(paste0('Ground filter resolution is: ', ground.res))
print(paste0('Number of neighbors to search is: ', nn))
cat("\n")

# 4.2.1 ground filter first pass -- lowest Z

ground.points = grid.filter.ground(las) # Local minimum function, uses ground.res

print(paste0('Ground points filtered to lowest Z (first pass), number of ground points is: ', nrow(ground.points)))
cat("\n")

# 4.2.2 ground filter second pass -- nearest neighbor

ground.points = try(filter.to.lowest.nn(ground.points, k=nn))  # Nearest neighbor filtering

if(class(ground.points)[1] == 'try-error'){
  stop("Filtering to nearest neighbor failed")
  cat("\n")
} else{
  print(paste0('Ground points filtered to nearest neighbor (second pass), number of ground points is: ', nrow(ground.points)))
  cat("\n")
}

ground.points$Classification = as.integer(2)
ground = las 
ground@data = ground.points # Save data table of points to las

# 4.3 MERGE GROUND AND CANOPY INTO SINGLE LAS FILE ------------------------------

las.classified = rbind(canopy, ground)

# Make sure there are ground points
if(sum(las.classified$Classification == 2) == 0){
  stop('Site contains no ground points, halting execution')
  cat("\n")
}

print("Point cloud classified")
cat("\n")

######################################################################################################
######################################################################################################

# 5. CREATE DTM ------------------------------

dtm = lidR::grid_terrain(las.classified, res = pt_spacing, algorithm = knnidw())
print('DTM created with IDW, default parameters')
cat("\n")

######################################################################################################
######################################################################################################

# 6. NORMALIZE POINT CLOUD ------------------------------
# Proceeding with discarding negative values
# This method ultimately produces the best biomass prediction results
# Reasoning: these are areas where the DTM is not performing well, we should not assume they are zero but rather designate them as no data
# Evidence: spot checked GRANITE and AUFEIS sites -- areas where Z < 0 should NOT actually be 0 and often occur in areas with shrubs/graminoids etc.

las.norm = las.classified - dtm

print("Point cloud normalized")
cat("\n")

print(paste0("Point cloud contains ", sum(las.norm$Z < 0), " Z values less than zero"))
las.norm = lidR::filter_poi(las.norm, Z >= 0) # Clean
print("Values less than zero discarded")
cat("\n")

######################################################################################################
######################################################################################################

# 7. CREATE CHMs ------------------------------

# 7.1 REPORT FINAL POINT CLOUD ------------------------------

print('Ready for CHM generation, the final point cloud is:')
print(las.norm)
cat("\n")

print('The point cloud data is:')
print(las.norm@data)
cat("\n")

# 7.2 SET PARAMETERS ------------------------------

radius = pt_spacing * subcircle.multiplier

# 7.3 KNNIDW ------------------------------

chm.idw = lidR::grid_canopy(las.norm, chm.res, p2r(subcircle = radius, na.fill = knnidw()))

print("CHM created with knnidw()")
cat("\n")

# 7.4 TIDY ------------------------------

chm.idw[chm.idw < 0] = 0 # Convert negative values to zero

######################################################################################################
######################################################################################################

# 8. SAVE RESULTS ------------------------------

chm = chm.idw

outCHM = paste0(site, '_CHM_base.tif')

raster::writeRaster(chm, outCHM)

print("Final CHM raster written to disk:")
print(outCHM)
cat("\n")

