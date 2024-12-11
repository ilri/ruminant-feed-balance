gc()
rm(list=ls())
.libPaths(c(.libPaths()[1], .libPaths()[2], .libPaths()[3]))
# Prepare crop ME - Regional
# Author: Simon Fraval
# Last modified by John Mutua on 12/11/2024

# avoid scientific notation
options(scipen = 999)

# # Install required packages
# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("exactextractr")
# install.packages("purrr")

# Load libraries
library(raster)
library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)
library(purrr)

rasterOptions(tmpdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp") # Process needs > 40GB of temporary disk space
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results"); dir.create(Results_dir, F, T)
Outputs_dir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/outputs"); dir.create(Outputs_dir, F, T)

zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))
regions <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/regions.gpkg"))

# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
tsSumZone_List <- list()
tsSumRegion_List <- list()
for(year in yearList){
  
  cropME_HI_utilmean <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropME_HI_utilmean.tif"))
  cropMEmean <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropMEmean.tif"))
  feedQuality_item <- read.csv(paste0(CropParams_dir, "/feedQuality_item.csv"))
  feedCropBurn <- raster(paste0(spatialDir, "/inputs/Burned/", year , "/burnCropMonths.tif"))
  
  #croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
  croppingDays <- raster(paste0(CropParams_dir, "/Cropping_days/croppingDays_", year, ".tif"))
  croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
  croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
  croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
  dryDays <- 365 - croppingDays
  
  region <- raster(paste0(spatialDir, "/intermediate/regions.tif"))
  grassMEForest <- 7.9 #Adebayo et al 2020 https://njap.org.ng/index.php/njap/article/view/1264/1103 6.2-9.0 
  grassMEWetSavannah <- 6.5 #Rahimi et al. 2021
  grassMEDrySavannah <- 6.1 #Rahimi et al. 2021
  grassME <- calc(region, fun = function(x){ifelse(x == 1, grassMEDrySavannah, ifelse(x == 2, grassMEForest, grassMEWetSavannah))})
  grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")
  
  browseMEForest <- 9.8 #Anele et al. 2009 https://doi.org/10.1016/j.anifeedsci.2009.07.007 9.1-10.6
  browseMEWetSavannah <- 8.1 #Rahimi et al. 2021
  browseMEDrySavannah <- 6.0 #Rahimi et al. 2021
  browseME <- calc(region, fun = function(x){ifelse(x == 1, browseMEDrySavannah, ifelse(x == 2, browseMEForest, browseMEWetSavannah))})
  browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
  #browseMEmean <- 5
  
  grassFracDry <- 0.33 
  grassFracWet <- 0.55 
  browseFrac <- 0.16
  
  tCrop <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPcropmean_", year, ".tif"))
  tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
  tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmean #reclassify for crops to make sure there is a logical minimum cropping period
  # for(i in 1:length(names(tCrop))){
  #   tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
  # }
  
  tCrop <- overlay(tCrop, feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP)})
  writeRaster(tCrop, paste0(Outputs_dir, "/Feed_crop_MJ", year, ".tif"), overwrite = T) # Write tCrop outputs
  
  #tGrassWet <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassWetmean_2",full.names = T))
  tGrassWet <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassWetmean_", year, ".tif"))
  tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
  tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 
  
  #tGrassDry <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/', pattern="DMPgrassDrymean_2",full.names = T))
  tGrassDry <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassDrymean_", year, ".tif"))
  tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
  tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME
  
  tGrass <- tGrassWet + tGrassDry
  writeRaster(tGrass, paste0(Outputs_dir, "/Feed_grass_MJ", year, ".tif"), overwrite = T) # Write tGrass outputs
  
  #tBrowse <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPbrowsemean_2",full.names = T))
  tBrowse <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPbrowsemean_", year, ".tif"))
  tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
  tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
  tBrowseDry <- tBrowse*browseME*dryDays * browseFrac
  
  tBrowse <- tBrowseWet + tBrowseDry
  writeRaster(tBrowse, paste0(Outputs_dir, "/Feed_browse_MJ", year, ".tif"), overwrite = T) # Write tBrowse outputs
  
  #tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
  tAfter <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPaftermean_", year, ".tif"))
  tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
  tAfter <- tAfter*grassFracDry*feedQuality_item$ME[feedQuality_item$codeSPAM == "natPast"]*dryDays
  writeRaster(tAfter, paste0(Outputs_dir, "/Feed_after_MJ", year, ".tif"), overwrite = T) # Write tAfter outputs
  
  rm(dryDays, croppingDays, feedCropBurn)
  
  ## Feed output
  #tsSum <- data.frame(region = c(rep("(Agro)pastoral Sahel", 6), rep("Central mixed", 6), rep("Cropping", 6), rep("North mixed", 6), rep("South mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  tsSumZone <- data.frame(zone = c("(Agro)pastoral sahel", "Central mixed", "Forest mixed", "Northern mixed", "Southern mixed"), year = year, lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  
  cropME_mean <- exact_extract(tCrop, zones, "sum")
  tsSumZone$cropME_mean <- as.numeric(c(cropME_mean[1], cropME_mean[2], cropME_mean[3], cropME_mean[4], cropME_mean[5]))
  
  grassME_mean <- exact_extract(tGrass, zones, "sum")
  tsSumZone$grassME_mean <- as.numeric(c(grassME_mean[1], grassME_mean[2], grassME_mean[3], grassME_mean[4], grassME_mean[5]))
  
  browseME_mean <- exact_extract(tBrowse, zones, "sum")
  tsSumZone$browseME_mean <- as.numeric(c(browseME_mean[1], browseME_mean[2], browseME_mean[3], browseME_mean[4], browseME_mean[5]))
  
  afterME_mean <- exact_extract(tAfter, zones, "sum")
  tsSumZone$afterME_mean <- as.numeric(c(afterME_mean[1], afterME_mean[2], afterME_mean[3], afterME_mean[4], afterME_mean[5]))
  
  tsSumZone_List[[year]] <- tsSumZone
  
  cat("Completed processing zonal stats for: ", year, "\n")
  
  ##Calculate average ME
  tsSumRegion <- data.frame(region = c("Dry Savannah", "Forest", "Wet Savannah"), year = year)
  #tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)
  
  cropME_mean <- exact_extract(tCrop, regions, "sum")
  tsSumRegion$cropME_mean <- as.numeric(c(cropME_mean[1], cropME_mean[2], cropME_mean[3]))
  
  grassME_mean <- exact_extract(tGrass, regions, "sum")
  tsSumRegion$grassME_mean <- as.numeric(c(grassME_mean[1], grassME_mean[2], grassME_mean[3]))
  
  browseME_mean <- exact_extract(tBrowse, regions, "sum")
  tsSumRegion$browseME_mean <- as.numeric(c(browseME_mean[1], browseME_mean[2], browseME_mean[3]))
  
  afterME_mean <- exact_extract(tAfter, regions, "sum")
  tsSumRegion$afterME_mean <- as.numeric(c(afterME_mean[1], afterME_mean[2], afterME_mean[3]))
  
  ##Calculate ME of grass and browse by season
  grassMEwet_mean <- exact_extract(tGrassWet, regions, "sum")
  tsSumRegion$grassMEwet_mean <- as.numeric(c(grassMEwet_mean[1], grassMEwet_mean[2], grassMEwet_mean[3]))
  
  grassMEdry_mean <- exact_extract(tGrassDry, regions, "sum")
  tsSumRegion$grassMEdry_mean <- as.numeric(c(grassMEdry_mean[1], grassMEdry_mean[2], grassMEdry_mean[3]))
  
  browseMEwet_mean <- exact_extract(tBrowseWet, regions, "sum")
  tsSumRegion$browseMEwet_mean <- as.numeric(c(browseMEwet_mean[1], browseMEwet_mean[2], browseMEwet_mean[3]))
  
  browseMEdry_mean <- exact_extract(tBrowseDry, regions, "sum")
  tsSumRegion$browseMEdry_mean <- as.numeric(c(browseMEdry_mean[1], browseMEdry_mean[2], browseMEdry_mean[3]))
  
  #Add DM
  cropDM <- exact_extract(tCrop/cropMEmean, regions, "sum")
  tsSumRegion$cropDM <- as.numeric(c(cropDM[1], cropDM[2], cropDM[3]))
  
  grassDM <- exact_extract(tGrass/grassME, regions, "sum")
  tsSumRegion$grassDM <- as.numeric(c(grassDM[1], grassDM[2], grassDM[3]))
  
  grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
  tsSumRegion$grassDMwet <- as.numeric(c(grassDMwet[1], grassDMwet[2], grassDMwet[3]))
  
  grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
  tsSumRegion$grassDMdry <- as.numeric(c(grassDMdry[1], grassDMdry[2], grassDMdry[3]))
  
  browseDM_mean <- exact_extract(tBrowse/browseME, regions, "sum")
  tsSumRegion$browseDM <- as.numeric(c(browseDM_mean[1], browseDM_mean[2], browseDM_mean[3]))
  
  browseDMwet_mean <- exact_extract(tBrowseWet/browseME, regions, "sum")
  tsSumRegion$browseDMwet <- as.numeric(c(browseDMwet_mean[1], browseDMwet_mean[2], browseDMwet_mean[3]))
  
  browseDMdry_mean <- exact_extract(tBrowseDry/browseME, regions, "sum")
  tsSumRegion$browseDMdry <- as.numeric(c(browseDMdry_mean[1], browseDMdry_mean[2], browseDMdry_mean[3]))
  
  tsSumRegion$afterDM <- tsSumRegion$afterME_mean / feedQuality_item$ME[feedQuality_item$codeSPAM == "natPast"]
  
  tsSumRegion_List[[year]] <- tsSumRegion
  
  cat("Completed processing regional stats for: ", year, "\n")
  
}

# For zonal stats
tsSumZone_combined <- map_dfr(tsSumZone_List, ~ .x)
write.csv(tsSumZone_combined, paste0(Results_dir, "/disaggregated_timeseries.csv"), row.names=FALSE)

##Export total feed ME for adequacy estimates
feed_list <- c("crop", "grass", "browse", "after")
for (year in yearList){
  #tFeed <- stack(list.files(path = paste0(Outputs_dir), pattern = year, full.names = TRUE))
  tFeed <- paste0(Outputs_dir, "/Feed_", feed_list, "_MJ", year, ".tif")
  tFeed <- sum(stack(tFeed), na.rm = T)
  writeRaster(tFeed, paste0(Outputs_dir, "/Feed_total_mean_MJ", year, ".tif"), overwrite = T)
}

# For regional stats
tsSumRegion_combined <- map_dfr(tsSumRegion_List, ~ .x)
outMEmean <- tsSumRegion_combined[tsSumRegion_combined$year == 2023,] %>% 
  rowwise() %>% 
  mutate(ME_all = sum(cropME_mean, grassME_mean, browseME_mean, afterME_mean) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop = cropME_mean / cropDM, MEwet_all = sum(grassMEwet_mean, browseMEwet_mean) / sum(grassDMwet, browseDMwet), MEdry_all = sum(cropME_mean, grassMEdry_mean, browseMEdry_mean) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
write.csv(outMEmean, paste0(Results_dir, "/cropME_region.csv"), row.names=FALSE)
