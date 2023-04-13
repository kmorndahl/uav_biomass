######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script calculates volume from the final canopy height models
# See manuscript reference in the README.md file for more details

# NOTE: UAV products necessary for running this code are not hosted at github, see author for access

######################################################################################################
######################################################################################################

# SET OUTPUT PATH

output_results = FALSE

outPath = '' # Set output directory if desired

######################################################################################################
######################################################################################################

# 1. READ IN DATA AND SET PARAMETERS ------------------------------

library(raster)

# Gather CHMs
chmPaths = list.files(path = outPath, pattern = "CHM_final.tif$", full.names = TRUE)
print("CHMs are:")
print(chmPaths)
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

print("CHM is:")
print(chm)
cat("\n")

######################################################################################################
######################################################################################################

# 4. CALCULATE VOLUME ------------------------------

x.res = raster::xres(chm)
y.res = raster::yres(chm)

# (height in m * x resolution in m * y resolution in m) * (100 ^ 3) = volume in cm
vol = (chm * x.res * y.res) * (100^3)

print("Volume calculated")
cat("\n")

######################################################################################################
######################################################################################################

# 5. SAVE RESULTS ------------------------------

if(output_results){

  outVol = paste0(outPath, site, '_vol_cm.tif')
  
  raster::writeRaster(vol, outVol)
  
  print("Results written to disk")
  cat("\n")

}