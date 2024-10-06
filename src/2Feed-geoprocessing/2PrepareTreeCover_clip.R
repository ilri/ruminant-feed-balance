library(raster)
library(stars)
#library(terra)
library(sf)
library(exactextractr)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
proPath <- paste0(root, "/src/1Data-download/SpatialData/inputs/TreeCover")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/TreeCover"); dir.create(outdir, F, T)

# read AOI
aoi_path <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp")

filenamesTifInter <- list.files(path = proPath ,pattern="*.tif$",full.names = T)
#filenamesTifInter2 <- list.files(path = proPath ,pattern="*.tif$",full.names = F)

##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(outdir, "/", basename(filenamesTifInter[i])), overwrite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = aoi_path, crop_to_cutline = T) #0.00297619, 0.00297619
} 