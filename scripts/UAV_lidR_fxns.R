###################################################################################
#
# H Greaves 2016
# 
# This script contains functions called by the accompanying script 
# "Lidar ground-canopy-volume generation.R". Please see that script for information
# about using these functions.
#
###################################################################################

require(data.table)

## To read and store parts of file names for naming of derivative products
parse.filename = function(file.name){
  llx = unlist(strsplit(file.name, "_"))[1]
  lly = unlist(strsplit(unlist(strsplit(file.name, "_"))[2], ".txt"))
  name.parts = list(llx = llx, lly = lly)
}

parse.filename2 = function(file.name){
  llx = unlist(strsplit(file.name, "_"))[2]
  lly = unlist(strsplit(unlist(strsplit(file.name, "_"))[3], ".txt"))
  name.parts = list(llx = llx, lly = lly)
}

## This is a complicated function, but when it works right it does a lot of thinking for you to make sure your tiles are correctly named.
generate.filenames = function(point.type, output.type, ...){
  
  if(point.type == "ground"){
    if(output.type == "points"){
      ground.points.filename = sprintf("%s_%s_%s_%smfilter_nn%s", name.parts$site.name, name.parts$llx, name.parts$lly, 
                                       grid.res.ground.filter,nn)
      ground.points.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/GroundPoints/", ground.points.filename, ".txt")
      
      name.list = c(ground.points.filename, ground.points.file)
    } 
    
    if(output.type == "raster"){
      ground.points.filename = sprintf("%s_%s_%s_%smfilter_nn%s", name.parts$site.name, name.parts$llx, name.parts$lly, 
                                       grid.res.ground.filter,nn)
      ground.points.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/GroundPoints/", ground.points.filename, ".txt")
      
      ground.raster.filename = sprintf("%s_%s_%s_%smfilter_res%s_nn%s", name.parts$site.name, name.parts$llx, name.parts$lly,
                                       grid.res.ground.filter, grid.res.output, nn)
      ground.raster.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/GroundRasters/", ground.raster.filename, ".asc")
      
      name.list = c(ground.points.filename, ground.points.file, ground.raster.filename, ground.raster.file)
    }
  }  ## End ground loop 
  
  if(point.type == "canopy"){
    
    if(output.type == "points"){
      
      canopy.points.filename = sprintf("%s_%s_%s_%smfilter", name.parts$site.name, name.parts$llx, name.parts$lly, 
                                       grid.res.canopy.filter)
      canopy.points.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/CanopyPoints/", canopy.points.filename, ".txt")
      
      name.list = c(canopy.points.filename, canopy.points.file)
      
    }
    
    if(output.type == "raster"){
      
      canopy.points.filename = sprintf("%s_%s_%s_%smfilter", name.parts$site.name, name.parts$llx, name.parts$lly, 
                                       grid.res.canopy.filter)
      canopy.points.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/CanopyPoints/", canopy.points.filename, ".txt")
      
      canopy.raster.filename = sprintf("%s_%s_%s_%smfilter_res%s", name.parts$site.name, name.parts$llx, name.parts$lly,
                                       grid.res.canopy.filter, grid.res.output)
      canopy.raster.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/CanopyRasters/", canopy.raster.filename, ".asc")
      
      name.list = c(canopy.points.filename, canopy.points.file, canopy.raster.filename, canopy.raster.file)
    }
  } ## End canopy loop
  
  if(point.type == "volume"){
    volume.raster.filename = sprintf("%s_%s_%s_res%s_volume", name.parts$site.name, name.parts$llx, name.parts$lly,
                                     grid.res.output)
    volume.raster.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/VolumeRasters/", volume.raster.filename, ".asc")
    name.list = c(volume.raster.filename, volume.raster.file)
  } ## End volume raster loop
  
  if(point.type == "height"){
    height.raster.filename = sprintf("%s_%s_%s_res%s_height", name.parts$site.name, name.parts$llx, name.parts$lly,
                                     grid.res.output)
    height.raster.file = paste0("D:/hgreaves/UAV_pilot_project/LidarProcessing/HeightRasters/", height.raster.filename, ".asc")
    name.list = c(height.raster.filename, height.raster.file)
  } ## End reight raster loop

    name.list
  
}

# Just a quick function to write your data the way you want it:
write.points = function(las.input, outname = name.list[2]){
  
  write.table(las.input, outname,
              quote = F,
              row.names = F,
              col.names = F)
}

# Filter outliers
# https://cran.r-project.org/web/packages/lidR/vignettes/lidR-catalog-apply-examples.html
# https://gis.stackexchange.com/questions/371774/way-to-filter-outliers-from-point-cloud-in-lidr
filter.noise = function(las, sensitivity = 1.2) # Default is 95% confidence interval plus 20% sensitivity
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}

# Identifying canopy points based on user-specified processing parameters - single pass: grid filter:
grid.filter.canopy = function(las.input, grid.resolution = canopy.res){
  data = as.data.table(las.input@data) # Deep copy of the data table - current version of the data table is NOT over-allocated for some reason ... it is stuck at 19 column slots (see truelength()) and no amount of setalloccol() is working
  
  xi = round(min(data$X) - grid.resolution/2, digits = 3)    ##  Buffer the minimum point value by half the pixel size to find the lower bound for the xy grid
  yi = round(min(data$Y) - grid.resolution/2, digits = 3)
  
  data[,x.position := ceiling((data[,X]-xi)/grid.resolution)]   ## Assign x and y grid coordinates (pixels) to each point
  data[,y.position := ceiling((data[,Y]-yi)/grid.resolution)]   ## The := operator is causing errors with original data.table, see below
  
  # 1: In `[.data.table`(las.input@data, , `:=`(x.position, ceiling((las.input@data[, : Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
  
  setkeyv(data, c("x.position","y.position"))    ##  Allows subsetting by x and y pixel position
  
  max.z = data[,highest:=Z==max(Z), by = key(data)][highest == TRUE,]    ## gets highest z value for each xy grid "pixel"
  unique(max.z[,.(X,Y,Z,R,G,B)])    ##  eliminate duplicate points
}

# Identifying ground points based on user-specified processing parameters - first pass: grid filter:
grid.filter.ground = function(las.input, grid.resolution = ground.res){
  data = as.data.table(las.input@data) # Deep copy of the data table - current version of the data table is NOT over-allocated for some reason ... it is stuck at 19 column slots (see truelength()) and no amount of setalloccol() is working
  
  xi = round(min(data$X) - grid.resolution/2, digits = 3)    ##  Buffer the minimum point value by half the pixel size to find the lower bound for the xy grid
  yi = round(min(data$Y) - grid.resolution/2, digits = 3)
  
  data[,x.position := ceiling((data[,X]-xi)/grid.resolution)]   ##  Assign x and y grid coordinates (pixels) to each point
  data[,y.position := ceiling((data[,Y]-yi)/grid.resolution)]   ## The := operator is causing errors with original data.table, see below
  
  # 1: In `[.data.table`(las.input@data, , `:=`(x.position, ceiling((las.input@data[, : Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
  
  setkeyv(data, c("x.position","y.position"))    ##  Allows subsetting by x and y
  
  min.z = data[,lowest:=Z==min(Z), by = key(data)][lowest == TRUE,]    ## gets lowest z value for each xy grid "pixel"
  unique(min.z[,.(X,Y,Z,R,G,B)])    ##  eliminate duplicate points
}

# Identifying ground points based on user-specified processing parameters - second pass: nearest neighbor filter:
filter.to.lowest.nn = function(las.input, ...){
  require(FNN)
  
  matxy = get.knn(las.input[,.(X,Y)], k = nn)
  
  ##  ...this makes list of nearest xy (not xyz) neighbor matrices. (List has 2 matrices containing indices [row numbers] and distances)
  ##  (NOTE that these matrices do not include the points themselves (i.e. points are not considered their own nearest neighbor))

  matxy$z = matrix(las.input[as.vector(matxy$nn.index),Z], ncol=nn)    ## for ease of scripting, add an additional matrix to the list, containing the corresponding z value for each point in the nearest neighbor matrices
  matxy$nn.index = cbind(matxy$nn.index,1:nrow(las.input))    ##  add the row numbers of the points themselves (the points whose neighbors are indicated on each line of the nn matrices)
  matxy$z = cbind(matxy$z,las.input[,Z])    ##  add the z values of the points themselves

  indexlist = cbind(1:nrow(matxy$z), apply(matxy$z, 1, function(y){which.min(y)}))    ##  make a list of only the lowest point among each set of neighbors
  keeplist = matxy$nn.index[indexlist]    ##  create a vector containing the row numbers of those points

  unique(las.input[keeplist,])    ##  Filter the original set of lowest z values points to only include these lowest neighbors
}

# Identifying canopy points based on user-specified processing parameters - second pass: nearest neighbor filter:
filter.to.highest.nn = function(las.input, ...){
  require(FNN)
  
  matxy = get.knn(las.input[,.(X,Y)], k = nn)
  
  ##  ...this makes list of nearest xy (not xyz) neighbor matrices. (List has 2 matrices containing indices [row numbers] and distances)
  ##  (NOTE that these matrices do not include the points themselves (i.e. points are not considered their own nearest neighbor))
  
  matxy$z = matrix(las.input[as.vector(matxy$nn.index),Z], ncol=nn)    ## for ease of scripting, add an additional matrix to the list, containing the corresponding z value for each point in the nearest neighbor matrices
  matxy$nn.index = cbind(matxy$nn.index,1:nrow(las.input))    ##  add the row numbers of the points themselves (the points whose neighbors are indicated on each line of the nn matrices)
  matxy$z = cbind(matxy$z,las.input[,Z])    ##  add the z values of the points themselves
  
  indexlist = cbind(1:nrow(matxy$z), apply(matxy$z, 1, function(y){which.max(y)}))    ##  make a list of only the lowest point among each set of neighbors
  keeplist = matxy$nn.index[indexlist]    ##  create a vector containing the row numbers of those points
  
  unique(las.input[keeplist,])    ##  Filter the original set of lowest z values points to only include these lowest neighbors
}

# The lastools call for ground points (generalized, no specification of lower left corner)
run.blast2dem.ground = function(points.file, points.filename, raster.file, ...){    ## Rasterize filtered points
  shell(paste0("set PATH=%PATH%;C:/lastools_new/bin; & las2dem -i ", 
               points.file, " -iparse xyz -rescale 0.001 0.001 0.001 -kill 10 -step ",
               grid.res.output, 
               " -o ", raster.file, " -oasc"))
  
  if(!file.exists(raster.file)){print("Error in raster creation")} # this checks to be sure the raster was created
}

# The lastools call for canopy points (specifies that the extent should match the previously-made ground raster extent)
run.blast2dem = function(points.file, points.filename, raster.file, ...){    ## Rasterize filtered points
  shell(paste0("set PATH=%PATH%;C:/lastools_new/bin; & las2dem -i ", 
               points.file, " -iparse xyz -rescale 0.001 0.001 0.001 -kill 10 -step ",
               grid.res.output, " -ll ", xll, " ", yll, " -nrows ", num.rows, " -ncols ", num.cols,
               " -o ", raster.file, " -oasc"))
  
  if(!file.exists(raster.file)){print("Error in raster creation")}
}

# The lastools call for canopy rasters that didn't match their ground rasters' extent: shifts the data by a specified offset.
run.blast2dem.trans = function(points.file, points.filename, raster.file, ...){    ## Rasterize filtered points
  
  shell(paste0("set PATH=%PATH%;C:/lastools_new/bin; & las2dem -i ", 
               points.file, " -iparse xyz -rescale 0.001 0.001 0.001 -kill 10 -step ",
               grid.res.output, " -nrows ", num.rows, " -ncols ", num.cols, " -ll ", xll, " ", yll, 
               " -translate_xyz ", dx, " ", dy, " 0 ", 
               " -o ", raster.file))
  
  if(!file.exists(raster.file)){print("Error in raster creation")}
}

# Following translation, the raster data should be correct but the header will reflect the old extent: rewrite it.
edit.raster.corner = function(raster.file, dx=dx, dy=dy){
  raster.info = parse.raster(raster.file)
  
  sink(raster.file)
  cat(sprintf("ncols %s\nnrows %s\nxllcorner %s\nyllcorner %s\ncellsize %s\nNODATA_value -9999.0\n",
              num.cols, num.rows, round(raster.info$xll - dx, 6), round(raster.info$yll - dy, 6), grid.res.output))
  write.table(raster.info$raster.body, row.names = F, col.names = F)
  sink()
}

# A little function to read in an ascii raster as a list
parse.raster = function(raster.file){  ## Used in other functions
  
  num.cols = as.integer(read.table(raster.file, nrows = 1)[2])
  num.rows = as.integer(read.table(raster.file, nrows = 1, skip = 1)[2])
  
  xll = as.numeric(read.table(raster.file, nrows = 1, skip = 2)[2])
  yll = as.numeric(read.table(raster.file, nrows = 1, skip = 3)[2])
  cellsize = as.numeric(read.table(raster.file, nrows = 1, skip = 4)[2])
  raster.body = as.matrix(fread(raster.file, header = F), byrow = T, nrow = num.rows)
  
  list(num.cols = num.cols, num.rows = num.rows, xll = xll, yll = yll, cellsize = cellsize, raster.body = raster.body)
}

# An even littler function to read in just the header of an ascii raster as a list - faster than parse.raster()
parse.raster.header = function(raster.file){  ## Used in other functions
  
  num.cols = as.integer(read.table(raster.file, nrows = 1)[2])
  num.rows = as.integer(read.table(raster.file, nrows = 1, skip = 1)[2])
  
  xll = as.numeric(read.table(raster.file, nrows = 1, skip = 2)[2])
  yll = as.numeric(read.table(raster.file, nrows = 1, skip = 3)[2])
  cellsize = as.numeric(read.table(raster.file, nrows = 1, skip = 4)[2])
  
  list(num.cols = num.cols, num.rows = num.rows, xll = xll, yll = yll, cellsize = cellsize)
}

# mba algorithm for use in lidR grid_terrain()
mba <- function(n = 1, m = 1, h = 8, extend = TRUE) {
  # f is created inside mba and receive the ground points (what)
  # and the location where to compute the interpolation (where) 
  f <- function(what, where, scales = c(0,0), offsets = c(0,0)) {
    # computation of the interpolation (see the documentation of MBA package)
    res <- MBA::mba.points(what, where, n, m , h, extend)
    return(res$xyz.est[,3])
  }
  
  # f is a function but we set compatible classes. Here it is an
  # algorithm for spatial interpolation (SPI) we use an internal global
  # variable to set the good classes
  class(f) <- lidR:::LIDRALGORITHMSPI
  return(f)
}
