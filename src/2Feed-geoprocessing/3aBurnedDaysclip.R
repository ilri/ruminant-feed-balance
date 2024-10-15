library(dplyr)
library(raster)
library(rgdal)
library(sf)
library(terra)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# output folder
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Burned"); dir.create(outdir, F, T)

# read AOI
aoi <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

nc_files <- list.files(paste0(root, "/src/1Data-download/SpatialData/inputs/Burned"), pattern = ".nc$", full.names = TRUE, recursive = TRUE)

lapply(nc_files, function(nc_file){
  
  nc_name <- gsub('.{3}$', '', basename(nc_file))
  
  if (!file.exists(paste0(outdir, "/", nc_name, ".tif", sep=""))){
    
    iBurned <- raster::raster(nc_file, varname="day_of_burn", ncdf=TRUE)
    iBurned <- iBurned %>% rast()
    iBurned <- crop(iBurned, ext(dmpTemp))
    iBurned <- resample(iBurned, dmpTemp, method="near", threads=TRUE)
    iBurned <- mask(iBurned, mask = dmpTemp)
    
    # save as GeoTIFF
    terra::writeRaster(iBurned, filename = paste0(outdir, "/", nc_name, ".tif"), overwrite=TRUE)
    rm(iBurned)
    gc()
    
  }else{
    cat("Already cropped: ", nc_name, "\n")
  }
})