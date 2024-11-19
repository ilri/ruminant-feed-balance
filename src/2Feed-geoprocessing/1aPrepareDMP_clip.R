# Prepare DMP layers
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("dplyr")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("sf")

# Load required packages
library(dplyr)
library(raster)
library(rgdal)
library(sf)

# study area
country <- "Nigeria"

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# output folder
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP"); dir.create(outdir, F, T)

# read AOI
aoi <- read_sf(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

nc_files <- list.files("/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/Geodata/DMP", pattern = ".nc$", full.names = TRUE, recursive = TRUE)

for (nc_file in nc_files){
  
  nc_name <- gsub('.{3}$', '', basename(nc_file))
  
  if (!file.exists(paste0(outdir, "/", nc_name, ".tif", sep=""))){
    
    iDMP <- raster::raster(nc_file, varname="DMP", ncdf=TRUE)
    iDMP <- crop(iDMP, extent(aoi))
    iDMP <- mask(iDMP, aoi)
    
    # save as GeoTIFF
    raster::writeRaster(iDMP, filename = paste0(outdir, "/", nc_name, ".tif"), overwrite=TRUE)
    
    }else{cat("Already cropped: ", nc_name, "\n")}
}

# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# iDMP <- rast(paste0(outdir, "/c_gls_DMP300-RT6_202001100000_GLOBE_OLCI_V1.1.2.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) +
#   geom_spatraster(data = iDMP) +
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) +
#   scale_fill_gradient(limits = c(0, 160), breaks = c(0, 50, 100, 160), low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="DM kg/ha")
