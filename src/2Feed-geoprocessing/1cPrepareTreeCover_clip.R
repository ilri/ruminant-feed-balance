# Prepare treecover layer
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("exactextractr")
# install.packages("terra")
# install.packages("gdalUtils")

# Load required packages
library(raster)
library(stars)
#library(terra)
library(sf)
library(exactextractr)
library(terra)
library(gdalUtils)

# study area
country <- "Nigeria"

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
proPath <- paste0(root, "/src/1Data-download/SpatialData/inputs/TreeCover")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/TreeCover"); dir.create(outdir, F, T)

# read AOI
aoi_path <- paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp")

filenamesTifInter <- list.files(path = proPath ,pattern="*.tif$",full.names = T)
#filenamesTifInter2 <- list.files(path = proPath ,pattern="*.tif$",full.names = F)

##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(outdir, "/", basename(filenamesTifInter[i])), overwrite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = aoi_path, crop_to_cutline = T) #0.00297619, 0.00297619
} 

# Fix extent
dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

itreeCover <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/TreeCover/ps_africa_treecover_2019_100m_v1.0.tif"))
itreeCover <- crop(itreeCover, ext(dmpTemp))
itreeCover <- resample(itreeCover, dmpTemp, method="bilinear")
itreeCover <- mask(itreeCover, mask = dmpTemp)

writeRaster(itreeCover, paste0(outdir, "/treecover300m.tif"), overwrite=TRUE)

# # plotting
# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# treecover300m <- rast(paste0(outdir, "/treecover300m.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = treecover300m) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="Tree cover (%)")
