library(dplyr)
library(raster)
library(rgdal)
library(sf)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# output folder
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP"); dir.create(outdir, F, T)

# read AOI
aoi <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

nc_files <- list.files("/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/Geodata/DMP", pattern = ".nc$", full.names = TRUE, recursive = TRUE)

for (nc_file in nc_files){
  
  nc_name <- gsub('.{3}$', '', basename(nc_file))
  
  if (!file.exists(paste0(outdir, "/", nc_name, ".tif", sep=""))){
    
    iDMP <- raster::raster(nc_file, varname="DMP", ncdf=TRUE)
    iDMP <- crop(iDMP, extent(aoi))
    iDMP <- mask(iDMP, aoi)
    
    # save as GeoTIFF
    raster::writeRaster(iDMP, filename = paste0(outdir, "/", nc_name, ".tif"), overwrite=TRUE)
    
    }else{
      cat("Already cropped: ", nc_name, "\n")
    }
}
