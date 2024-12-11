gc()
rm(list=ls())
# Process time series - Regional adequacy totals - minimum and maximum
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
root <- "."

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

###################
# Process minimum
lv_ListMin <- list()
for(year in yearList){
  ###Livestock requirements
  cattleIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/cattleMER_max_MJ", year, ".tif")) #Max feed requirement will give min adequacy
  shoatsIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/shoatsMER_max_MJ", year, ".tif")) #Max feed requirement will give min adequacy
  horseDonkeyIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/horseDonkeyMER_MJ_2020.tif"))
  
  FAOlvstPop <- read.csv(paste0(LivestockParams_dir, "/FAOSTAT_livestock_data.csv"))
  
  lv_ListMin[[year]] <- sum(horseDonkeyIntake_model_MJ, (cattleIntake_model_MJ + (cattleIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ + (shoatsIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Shoats"])), na.rm = T)
}

tLv <- stack(lv_ListMin)

rm(cattleIntake_model_MJ, shoatsIntake_model_MJ, horseDonkeyIntake_model_MJ)

## Adequacy outputs
tFeed <- stack(list.files(path = paste0(spatialDir, "/outputs"), pattern="Feed_total_min_MJ",full.names = T))

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
zones <- bind_cols(select(zones, ECOZone), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqMEmax_2020", "lvstReqMEmax_2021", "lvstReqMEmax_2022", "lvstReqMEmax_2023", "feedME_min_2020", "feedME_min_2021", "feedME_min_2022", "feedME_min_2023")

tsSum$adeqMin_2020 <- tsSum$feedME_min_2020 / tsSum$lvstReqMEmax_2020
tsSum$adeqMin_2021 <- tsSum$feedME_min_2021 / tsSum$lvstReqMEmax_2021
tsSum$adeqMin_2022 <- tsSum$feedME_min_2022 / tsSum$lvstReqMEmax_2022
tsSum$adeqMin_2023 <- tsSum$feedME_min_2023 / tsSum$lvstReqMEmax_2023

tsSum_adeq <- read.csv(paste0(Results_dir, "/totals_timeseries_region.csv"))
tsSum_adeq$adeqMin_2020 <- tsSum$adeqMin_2020
tsSum_adeq$adeqMin_2021 <- tsSum$adeqMin_2021
tsSum_adeq$adeqMin_2022 <- tsSum$adeqMin_2022
tsSum_adeq$adeqMin_2023 <- tsSum$adeqMin_2023

write.csv(tsSum_adeq, paste0(Results_dir, "/totals_timeseries_region.csv"),row.names=FALSE)

###################
# Process maximum
lv_ListMax <- list()
for(year in yearList){
  ###Livestock requirements
  cattleIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/cattleMER_min_MJ", year, ".tif")) #Max feed requirement will give max adequacy
  shoatsIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/shoatsMER_min_MJ", year, ".tif")) #Max feed requirement will give max adequacy
  horseDonkeyIntake_model_MJ <- raster(paste0(spatialDir, "/outputs/horseDonkeyMER_MJ_2020.tif"))
  
  FAOlvstPop <- read.csv(paste0(LivestockParams_dir, "/FAOSTAT_livestock_data.csv"))
  
  lv_ListMax[[year]] <- sum(horseDonkeyIntake_model_MJ, (cattleIntake_model_MJ + (cattleIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ + (shoatsIntake_model_MJ*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == "Shoats"])), na.rm = T)
}

tLv <- stack(lv_ListMax)

rm(cattleIntake_model_MJ, shoatsIntake_model_MJ, horseDonkeyIntake_model_MJ)

## Adequacy outputs
tFeed <- stack(list.files(path = paste0(spatialDir, "/outputs"), pattern="Feed_total_max_MJ",full.names = T))

zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))

zones <- bind_cols(select(zones, ECOZone), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqMEmin_2020", "lvstReqMEmin_2021", "lvstReqMEmin_2022", "lvstReqMEmin_2023", "feedME_max_2020", "feedME_max_2021", "feedME_max_2022", "feedME_max_2023")

tsSum$adeqMax_2020 <- tsSum$feedME_max_2020 / tsSum$lvstReqMEmin_2020
tsSum$adeqMax_2021 <- tsSum$feedME_max_2021 / tsSum$lvstReqMEmin_2021
tsSum$adeqMax_2022 <- tsSum$feedME_max_2022 / tsSum$lvstReqMEmin_2022
tsSum$adeqMax_2023 <- tsSum$feedME_max_2023 / tsSum$lvstReqMEmin_2023

tsSum_adeq <- read.csv(paste0(Results_dir, "/totals_timeseries_region.csv"))
tsSum_adeq$adeqMax_2020 <- tsSum$adeqMax_2020
tsSum_adeq$adeqMax_2021 <- tsSum$adeqMax_2021
tsSum_adeq$adeqMax_2022 <- tsSum$adeqMax_2022
tsSum_adeq$adeqMax_2023 <- tsSum$adeqMax_2023

write.csv(tsSum_adeq, paste0(Results_dir, "/totals_timeseries_region.csv"),row.names=FALSE)
