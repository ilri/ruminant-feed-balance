gc()
rm(list=ls())
# Process time series - Regional adequacy totals
# Author: Simon Fraval
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
library(dplyr)
library(tidyr)
library(exactextractr)

rasterOptions(tmpdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp") # Process needs > 40GB of temporary disk space
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams")
LivestockParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/LivestockParams")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
Outputs_dir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/outputs")

zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))

# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
lv_List <- list()
for(year in yearList){
  
  ###Livestock requirements
  cattleIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/cattleMER_MJ_", year, ".tif"))
  shoatsIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/shoatsMER_MJ_", year, ".tif"))
  horseDonkeyIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/horseDonkeyMER_MJ_", year, ".tif"))
  
  FAOlvstPop <- read.csv(paste0(LivestockParams_dir, "/FAOSTAT_livestock_data.csv"))
  
  lv_List[[year]] <- sum(horseDonkeyIntake_model_MJ, (cattleIntake_model_MJ + (cattleIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ + (shoatsIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Shoats"])), na.rm = T)
  
}

tLv <- stack(lv_List)

rm(cattleIntake_model_MJ, shoatsIntake_model_MJ, horseDonkeyIntake_model_MJ)

## Adequacy outputs
tFeed <- stack(list.files(path = paste0(spatialDir, "/outputs"), pattern="Feed_total_mean_MJ",full.names = T))

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
zones <- bind_cols(select(zones, ECOZone), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqME_2020", "lvstReqME_2021", "lvstReqME_2022", "lvstReqME_2023", "feedME_mean_2020", "feedME_mean_2021", "feedME_mean_2022", "feedME_mean_2023")

tsSum$adeq_2020 <- tsSum$feedME_mean_2020 / tsSum$lvstReqME_2020
tsSum$adeq_2021 <- tsSum$feedME_mean_2021 / tsSum$lvstReqME_2021
tsSum$adeq_2022 <- tsSum$feedME_mean_2022 / tsSum$lvstReqME_2022
tsSum$adeq_2023 <- tsSum$feedME_mean_2023 / tsSum$lvstReqME_2023

write.csv(tsSum, paste0(Results_dir, "/totals_timeseries_region.csv"))