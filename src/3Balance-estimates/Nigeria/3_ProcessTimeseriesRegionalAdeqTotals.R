gc()
rm(list=ls())
# Process time series - Regional adequacy totals
# Author: John Mutua/Simon Fraval
# Last modified by John Mutua on 19/11/2024

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
library(tidyr)
library(dplyr)
library(exactextractr)

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

# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
lv_List <- list()
for(year in yearList){
  
  ###Livestock requirements
  cattleIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/cattleMER_MJ_", year, ".tif"))
  shoatsIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/shoatsMER_MJ_", year, ".tif"))
  horseDonkeyIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/horseDonkeyMER_MJ_", year, ".tif"))
  
  FAOlvstPop <- read.csv(paste0(LivestockParams_dir, "/FAOSTAT_livestock_data.csv"))
  
  lvReq <- sum(horseDonkeyIntake_model_MJ, (cattleIntake_model_MJ + (cattleIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ + (shoatsIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Shoats"])), na.rm = T)
  
  writeRaster(lvReq, paste0(Outputs_dir, "/lvReqTotal", year, ".tif"), overwrite=TRUE)
    
  lv_List[[year]] <- sum(horseDonkeyIntake_model_MJ, (cattleIntake_model_MJ + (cattleIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ + (shoatsIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Shoats"])), na.rm = T)
    
}

tLv <- stack(lv_List)

rm(cattleIntake_model_MJ, shoatsIntake_model_MJ, horseDonkeyIntake_model_MJ)

## Adequacy outputs
tFeed <- stack(list.files(path = paste0(spatialDir, "/outputs"), pattern="Feed_total_mean_MJ",full.names = T))

#extract by aggregation zones
aggregation_zones <- c("country", "region", "state")
for(aggregation_zone in aggregation_zones){
  
  if(aggregation_zone == "country"){
    zones <- st_read(paste0(spatialDir, "/inputs/aoi0.shp"))
    zones <- bind_cols(dplyr::select(zones, COUNTRY), exact_extract(terra::rast(tLv), zones, fun = "sum"))
    
  }else if(aggregation_zone == "region"){
    zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))
    zones <- bind_cols(dplyr::select(zones, ECOZone), exact_extract(terra::rast(tLv), zones, fun = "sum"))
    
  }else if(aggregation_zone == "state"){
    zones <- st_read(paste0(spatialDir, "/inputs/aoi1.shp"))
    zones <- bind_cols(dplyr::select(zones, NAME_1), exact_extract(terra::rast(tLv), zones, fun = "sum"))
    
  }
  
  zones <- bind_cols(zones, exact_extract(terra::rast(tFeed), zones, fun = "sum"))
  st_geometry(zones) <- NULL
  tsSum <- data.frame(zones)
  colnames(tsSum) <- c("NAME_1", "lvstReqME_2020", "lvstReqME_2021", "lvstReqME_2022", "lvstReqME_2023", "feedME_mean_2020", "feedME_mean_2021", "feedME_mean_2022", "feedME_mean_2023")
  
  tsSum$adeq_2020 <- tsSum$feedME_mean_2020 / tsSum$lvstReqME_2020
  tsSum$adeq_2021 <- tsSum$feedME_mean_2021 / tsSum$lvstReqME_2021
  tsSum$adeq_2022 <- tsSum$feedME_mean_2022 / tsSum$lvstReqME_2022
  tsSum$adeq_2023 <- tsSum$feedME_mean_2023 / tsSum$lvstReqME_2023
  
  write.csv(tsSum, paste0(Results_dir, "/totals_timeseries_", aggregation_zone, ".csv"), row.names=FALSE)
  
  cat("Completed extracting stats for: ", aggregation_zone, "\n")
  
}
