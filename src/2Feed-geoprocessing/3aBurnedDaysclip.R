# Prepare burned layers
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("dplyr")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("sf")
# install.packages("terra")

# Load libraries
library(dplyr)
library(raster)
library(rgdal)
library(sf)
library(terra)

# study area
country <- "Nigeria"

# root folder
root <- "."

# read AOI
aoi <- read_sf(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

yearList <- c("2020", "2021", "2022", "2023")

lapply(yearList[2:4], function(year){
  
  # output folder
  outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Burned/", year); dir.create(outdir, F, T)
  
  nc_files <- list.files(paste0(root, "/src/1Data-download/SpatialData/inputs/Burned/", year), pattern = ".nc$", full.names = TRUE, recursive = TRUE)
  
  lapply(nc_files, function(nc_file){
    
    nc_name <- gsub('.{3}$', '', basename(nc_file))
    
    if (!file.exists(paste0(outdir, "/", nc_name, ".tif", sep=""))){
      
      iBurned <- raster::raster(nc_file, varname="day_of_burn", ncdf=TRUE)
      iBurned <- iBurned %>% rast()
      #iBurned <- project(iBurned, dmpTemp)
      iBurned <- crop(iBurned, ext(dmpTemp))
      iBurned <- resample(iBurned, dmpTemp, method="near", threads=TRUE)
      iBurned <- mask(iBurned, mask = dmpTemp)
      
      # save as GeoTIFF
      terra::writeRaster(iBurned, filename = paste0(outdir, "/", nc_name, ".tif"), overwrite=TRUE)
      rm(iBurned)
      gc()
      
    }else{cat("Already cropped: ", nc_name, "\n")}
  })
})

# library(tidyterra)
# library(sf)
# library(ggplot2)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# burned_jan2020 <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Burned/2020/burnCropMonths.tif"))
# burned_jan2020 <- burned_jan2020 %>% crop(., extent(aoi1)) %>% mask(., aoi1)
# burned_jan2020[burned_jan2020==0]<-NA
# burned_jan2020[burned_jan2020>=1]<-1
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = burned_jan2020) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(na.value = NA, name = "Burn scars", low = "red", high = "red")
