gc()
rm(list=ls())
# Process time series - Regional DM totals
# Author: John Mutua
# Last modified 5/1/2025

# avoid scientific notation
options(scipen = 999)

# # Install required packages
# install.packages("raster")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("exactextractr")

# Load libraries
library(raster)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)
library(terra)

rasterOptions(tmpdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp") # Process needs > 40GB of temporary disk space
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams")
LivestockParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/LivestockParams")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
Outputs_dir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/outputs")

# Feed DM totals
# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
for(year in yearList){
  tFeedDM <- rast(list.files(path = paste0(spatialDir, "/inputs/Feed_quantity"), pattern=paste0("mean_", year, "\\.tif$"),full.names = T)) %>% app(., fun="sum", na.rm=TRUE)
  writeRaster(tFeedDM, paste0(spatialDir, "/inputs/Feed_quantity/DMPTotal_", year, ".tif"), overwrite = TRUE)
}

tFeedDM <- stack(list.files(path = paste0(spatialDir, "/inputs/Feed_quantity"), pattern="DMPTotal",full.names = T))

#extract by aggregation zones
aggregation_zones <- c("country", "region", "state")
for(aggregation_zone in aggregation_zones){
  
  if(aggregation_zone == "country"){
    zones <- st_read(paste0(spatialDir, "/inputs/aoi0.shp"))
    zones <- bind_cols(select(zones, COUNTRY), exact_extract(terra::rast(tFeedDM), zones, fun = "sum"))
    
  }else if(aggregation_zone == "region"){
    zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))
    zones <- bind_cols(select(zones, ECOZone), exact_extract(terra::rast(tFeedDM), zones, fun = "sum"))
    
  }else if(aggregation_zone == "state"){
    zones <- st_read(paste0(spatialDir, "/inputs/aoi1.shp"))
    zones <- bind_cols(select(zones, NAME_1), exact_extract(terra::rast(tFeedDM), zones, fun = "sum"))
  }
  
  st_geometry(zones) <- NULL
  tsSum <- data.frame(zones)
  colnames(tsSum) <- c("NAME_1", "feedDM_2020", "feedDM_2021", "feedDM_2022", "feedDM_2023")
  
  write.csv(tsSum, paste0(Results_dir, "/totals_timeseries_DM_", aggregation_zone, ".csv"), row.names=FALSE)
  
  cat("Completed extracting DM stats for: ", aggregation_zone, "\n")
  
}
