gc()
rm(list=ls())
.libPaths(c(.libPaths()[1], .libPaths()[2], .libPaths()[3]))
# Prepare crop ME - Regional - minimum, maximum
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
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
Outputs_dir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/outputs")

zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))
regions <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/regions.gpkg"))

# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
tsSumZoneMin_List <- list()
tsSumZoneMax_List <- list()
tsSumRegionMin_List <- list()
tsSumRegionMax_List <- list()
for(year in yearList){
  
  # Processing minimum ME
  cropME_HI_utilmin <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropME_HI_utilmin.tif"))
  cropMEmin <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropMEmin.tif"))
  feedQuality_item <- read.csv(paste0(CropParams_dir, "/feedQuality_item.csv"))
  feedCropBurn <- raster(paste0(spatialDir, "/inputs/Burned/", year , "/burnCropMonths.tif"))
  
  #croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
  croppingDays <- raster(paste0(CropParams_dir, "/Cropping_days/croppingDays_", year, ".tif"))
  croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
  croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
  croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
  dryDays <- 365 - croppingDays
  
  region <- raster(paste0(spatialDir, "/intermediate/regions.tif"))
  grassMELowlands <- 6.2
  grassMESavannah <- 6.2
  grassMESahel <- 5.8
  grassME <- calc(region, fun = function(x){ifelse(x == 1, grassMELowlands, ifelse(x == 2, grassMESahel, grassMESavannah))})
  grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")
  
  browseMELowlands <- 8
  browseMESavannah <- 8
  browseMESahel <- 5.8
  browseME <- calc(region, fun = function(x){ifelse(x == 1, browseMELowlands, ifelse(x == 2, browseMESahel, browseMESavannah))})
  browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
  #browseMEmean <- 5
  
  grassFracDry <- 0.33 
  grassFracWet <- 0.55 
  browseFrac <- 0.05
  
  #tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
  
  tCrop <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPcropmean_", year, ".tif"))
  tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
  tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmin #reclassify for crops to make sure there is a logical minimum cropping period
  # for(i in 1:length(names(tCrop))){
  #   tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
  # }
  
  tCrop <- overlay(tCrop, feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP)})
  writeRaster(tCrop, paste0(Outputs_dir, "/Feed_crop_min_MJ", year, ".tif"), overwrite = T) # Write tCrop outputs
  
  #tGrassWet <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassWetmean_2",full.names = T))
  tGrassWet <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassWetmean_", year, ".tif"))
  tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
  tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 
  
  #tGrassDry <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/', pattern="DMPgrassDrymean_2",full.names = T))
  tGrassDry <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassDrymean_", year, ".tif"))
  tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
  tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME
  
  tGrass <- tGrassWet + tGrassDry
  writeRaster(tGrass, paste0(Outputs_dir, "/Feed_grass_min_MJ", year, ".tif"), overwrite = T) # Write tGrass outputs
  
  #tBrowse <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPbrowsemean_2",full.names = T))
  tBrowse <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPbrowsemean_", year, ".tif"))
  tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
  tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
  tBrowseDry <- tBrowse*browseME*dryDays * browseFrac
  
  tBrowse <- tBrowseWet + tBrowseDry
  writeRaster(tBrowse, paste0(Outputs_dir, "/Feed_browse_min_MJ", year, ".tif"), overwrite = T) # Write tBrowse outputs
  
  #tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
  tAfter <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPaftermean_", year, ".tif"))
  tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
  tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays
  writeRaster(tAfter, paste0(Outputs_dir, "/Feed_after_min_MJ", year, ".tif"), overwrite = T) # Write tAfter outputs
  
  rm(dryDays, croppingDays, feedCropBurn)
  
  ## Feed output
  #tsSum <- data.frame(region = c(rep("(Agro)pastoral Sahel", 6), rep("Central mixed", 6), rep("Cropping", 6), rep("North mixed", 6), rep("South mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  tsSumZoneMin <- data.frame(zone = c("(Agro)pastoral sahel", "Central mixed", "Lowland mixed", "Southern mixed"), year = year, lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  
  cropME_min <- exact_extract(tCrop, zones, "sum")
  tsSumZoneMin$cropME_min <- as.numeric(c(cropME_min[1], cropME_min[2], cropME_min[3], cropME_min[4]))
  
  grassME_min <- exact_extract(tGrass, zones, "sum")
  tsSumZoneMin$grassME_min <- as.numeric(c(grassME_min[1], grassME_min[2], grassME_min[3], grassME_min[4]))
  
  browseME_min <- exact_extract(tBrowse, zones, "sum")
  tsSumZoneMin$browseME_min <- as.numeric(c(browseME_min[1], browseME_min[2], browseME_min[3], browseME_min[4]))
  
  afterME_min <- exact_extract(tAfter, zones, "sum")
  tsSumZoneMin$afterME_min <- as.numeric(c(afterME_min[1], afterME_min[2], afterME_min[3], afterME_min[4]))
  
  tsSumZoneMin_List[[year]] <- tsSumZoneMin
  
  cat("Completed processing zonal minimum stats for: ", year, "\n")
  
  ##Calculate minimum ME -regional
  tsSumRegionMin <- data.frame(region = c("Lowlands", "Sahel", "Savannah"), year = year)
  #tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)
  
  cropME_min <- exact_extract(tCrop, regions, "sum")
  tsSumRegionMin$cropME_min <- as.numeric(c(cropME_min[1], cropME_min[2], cropME_min[3]))
  
  grassME_min <- exact_extract(tGrass, regions, "sum")
  tsSumRegionMin$grassME_min <- as.numeric(c(grassME_min[1], grassME_min[2], grassME_min[3]))
  
  browseME_min <- exact_extract(tBrowse, regions, "sum")
  tsSumRegionMin$browseME_min <- as.numeric(c(browseME_min[1], browseME_min[2], browseME_min[3]))
  
  afterME_min <- exact_extract(tAfter, regions, "sum")
  tsSumRegionMin$afterME_min <- as.numeric(c(afterME_min[1], afterME_min[2], afterME_min[3]))
  
  ##Calculate ME of grass and browse by season
  grassMEwet_min <- exact_extract(tGrassWet, regions, "sum")
  tsSumRegionMin$grassMEwet_min <- as.numeric(c(grassMEwet_min[1], grassMEwet_min[2], grassMEwet_min[3]))
  
  grassMEdry_min <- exact_extract(tGrassDry, regions, "sum")
  tsSumRegionMin$grassMEdry_min <- as.numeric(c(grassMEdry_min[1], grassMEdry_min[2], grassMEdry_min[3]))
  
  browseMEwet_min <- exact_extract(tBrowseWet, regions, "sum")
  tsSumRegionMin$browseMEwet_min <- as.numeric(c(browseMEwet_min[1], browseMEwet_min[2], browseMEwet_min[3]))
  
  browseMEdry_min <- exact_extract(tBrowseDry, regions, "sum")
  tsSumRegionMin$browseMEdry_min <- as.numeric(c(browseMEdry_min[1], browseMEdry_min[2], browseMEdry_min[3]))
  
  #Add DM
  cropDM <- exact_extract(tCrop/cropMEmin, regions, "sum")
  tsSumRegionMin$cropDM <- as.numeric(c(cropDM[1], cropDM[2], cropDM[3]))
  
  grassDM <- exact_extract(tGrass/grassME, regions, "sum")
  tsSumRegionMin$grassDM <- as.numeric(c(grassDM[1], grassDM[2], grassDM[3]))
  
  grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
  tsSumRegionMin$grassDMwet <- as.numeric(c(grassDMwet[1], grassDMwet[2], grassDMwet[3]))
  
  grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
  tsSumRegionMin$grassDMdry <- as.numeric(c(grassDMdry[1], grassDMdry[2], grassDMdry[3]))
  
  browseDM_min <- exact_extract(tBrowse/browseME, regions, "sum")
  tsSumRegionMin$browseDM <- as.numeric(c(browseDM_min[1], browseDM_min[2], browseDM_min[3]))
  
  browseDMwet_min <- exact_extract(tBrowseWet/browseME, regions, "sum")
  tsSumRegionMin$browseDMwet <- as.numeric(c(browseDMwet_min[1], browseDMwet_min[2], browseDMwet_min[3]))
  
  browseDMdry_min <- exact_extract(tBrowseDry/browseME, regions, "sum")
  tsSumRegionMin$browseDMdry <- as.numeric(c(browseDMdry_min[1], browseDMdry_min[2], browseDMdry_min[3]))
  
  tsSumRegionMin$afterDM <- tsSumRegionMin$afterME_min / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]
  
  tsSumRegionMin_List[[year]] <- tsSumRegionMin
  
  cat("Completed processing regional minimum stats for: ", year, "\n")
  
  ############
  # Processing maximum ME
  cropME_HI_utilmax <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropME_HI_utilmax.tif"))
  cropMEmax <- raster(paste0(spatialDir, "/intermediate/Crop_ME/", year , "/cropMEmax.tif"))
  feedQuality_item <- read.csv(paste0(CropParams_dir, "/feedQuality_item.csv"))
  feedCropBurn <- raster(paste0(spatialDir, "/inputs/Burned/", year , "/burnCropMonths.tif"))
  
  croppingDays <- raster(paste0(CropParams_dir, "/Cropping_days/croppingDays_", year, ".tif"))
  croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
  croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
  croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
  dryDays <- 365 - croppingDays
  
  region <- raster(paste0(spatialDir, "/intermediate/regions.tif"))
  grassMELowlands <- 6.8
  grassMESavannah <- 6.8
  grassMESahel <- 6.2
  grassME <- calc(region, fun = function(x){ifelse(x == 1, grassMELowlands, ifelse(x == 2, grassMESahel, grassMESavannah))})
  grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")
  
  browseMELowlands <- 8.2
  browseMESavannah <- 8.2
  browseMESahel <- 6.2
  browseME <- calc(region, fun = function(x){ifelse(x == 1, browseMELowlands, ifelse(x == 2, browseMESahel, browseMESavannah))})
  browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
  #browseMEmean <- 5
  
  grassFracDry <- 0.33 
  grassFracWet <- 0.55 
  browseFrac <- 0.38
  
  #tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
  
  tCrop <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPcropmean_", year, ".tif"))
  tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
  tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmax #reclassify for crops to make sure there is a logical minimum cropping period
  # for(i in 1:length(names(tCrop))){
  #   tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
  # }
  
  tCrop <- overlay(tCrop, feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP)})
  writeRaster(tCrop, paste0(Outputs_dir, "/Feed_crop_max_MJ", year, ".tif"), overwrite = T) # Write tCrop outputs
  
  #tGrassWet <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassWetmean_2",full.names = T))
  tGrassWet <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassWetmean_", year, ".tif"))
  tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
  tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 
  
  #tGrassDry <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/', pattern="DMPgrassDrymean_2",full.names = T))
  tGrassDry <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPgrassDrymean_", year, ".tif"))
  tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
  tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME
  
  tGrass <- tGrassWet + tGrassDry
  writeRaster(tGrass, paste0(Outputs_dir, "/Feed_grass_max_MJ", year, ".tif"), overwrite = T) # Write tGrass outputs
  
  #tBrowse <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPbrowsemean_2",full.names = T))
  tBrowse <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPbrowsemean_", year, ".tif"))
  tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
  tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
  tBrowseDry <- tBrowse*browseME*dryDays * browseFrac
  
  tBrowse <- tBrowseWet + tBrowseDry
  writeRaster(tBrowse, paste0(Outputs_dir, "/Feed_browse_max_MJ", year, ".tif"), overwrite = T) # Write tBrowse outputs
  
  #tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
  tAfter <- raster(paste0(spatialDir, "/inputs/Feed_quantity/DMPaftermean_", year, ".tif"))
  tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
  tAfter <- tAfter*grassFracDry*feedQuality_item$ME_max[feedQuality_item$codeSPAM == "natPast"]*dryDays
  writeRaster(tAfter, paste0(Outputs_dir, "/Feed_after_max_MJ", year, ".tif"), overwrite = T) # Write tAfter outputs
  
  rm(dryDays, croppingDays, feedCropBurn)
  
  ## Feed output
  #tsSum <- data.frame(region = c(rep("(Agro)pastoral Sahel", 6), rep("Central mixed", 6), rep("Cropping", 6), rep("North mixed", 6), rep("South mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  tsSumZoneMax <- data.frame(zone = c("(Agro)pastoral sahel", "Central mixed", "Lowland mixed", "Southern mixed"), year = year, lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
  
  cropME_max <- exact_extract(tCrop, zones, "sum")
  tsSumZoneMax$cropME_max <- as.numeric(c(cropME_max[1], cropME_max[2], cropME_max[3], cropME_max[4]))
  
  grassME_max <- exact_extract(tGrass, zones, "sum")
  tsSumZoneMax$grassME_max <- as.numeric(c(grassME_max[1], grassME_max[2], grassME_max[3], grassME_max[4]))
  
  browseME_max <- exact_extract(tBrowse, zones, "sum")
  tsSumZoneMax$browseME_max <- as.numeric(c(browseME_max[1], browseME_max[2], browseME_max[3], browseME_max[4]))
  
  afterME_max <- exact_extract(tAfter, zones, "sum")
  tsSumZoneMax$afterME_max <- as.numeric(c(afterME_max[1], afterME_max[2], afterME_max[3], afterME_max[4]))
  
  tsSumZoneMax_List[[year]] <- tsSumZoneMax
  
  cat("Completed processing zonal maximum stats for: ", year, "\n")
  
  ##Calculate maximum ME -regional
  tsSumRegionMax <- data.frame(region = c("Lowlands", "Sahel", "Savannah"), year = year)
  #tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)
  
  cropME_max <- exact_extract(tCrop, regions, "sum")
  tsSumRegionMax$cropME_max <- as.numeric(c(cropME_max[1], cropME_max[2], cropME_max[3]))
  
  grassME_max <- exact_extract(tGrass, regions, "sum")
  tsSumRegionMax$grassME_max <- as.numeric(c(grassME_max[1], grassME_max[2], grassME_max[3]))
  
  browseME_max <- exact_extract(tBrowse, regions, "sum")
  tsSumRegionMax$browseME_max <- as.numeric(c(browseME_max[1], browseME_max[2], browseME_max[3]))
  
  afterME_max <- exact_extract(tAfter, regions, "sum")
  tsSumRegionMax$afterME_max <- as.numeric(c(afterME_max[1], afterME_max[2], afterME_max[3]))
  
  ##Calculate ME of grass and browse by season
  grassMEwet_max <- exact_extract(tGrassWet, regions, "sum")
  tsSumRegionMax$grassMEwet_max <- as.numeric(c(grassMEwet_max[1], grassMEwet_max[2], grassMEwet_max[3]))
  
  grassMEdry_max <- exact_extract(tGrassDry, regions, "sum")
  tsSumRegionMax$grassMEdry_max <- as.numeric(c(grassMEdry_max[1], grassMEdry_max[2], grassMEdry_max[3]))
  
  browseMEwet_max <- exact_extract(tBrowseWet, regions, "sum")
  tsSumRegionMax$browseMEwet_max <- as.numeric(c(browseMEwet_max[1], browseMEwet_max[2], browseMEwet_max[3]))
  
  browseMEdry_max <- exact_extract(tBrowseDry, regions, "sum")
  tsSumRegionMax$browseMEdry_max <- as.numeric(c(browseMEdry_max[1], browseMEdry_max[2], browseMEdry_max[3]))
  
  #Add DM
  cropDM <- exact_extract(tCrop/cropMEmax, regions, "sum")
  tsSumRegionMax$cropDM <- as.numeric(c(cropDM[1], cropDM[2], cropDM[3]))
  
  grassDM <- exact_extract(tGrass/grassME, regions, "sum")
  tsSumRegionMax$grassDM <- as.numeric(c(grassDM[1], grassDM[2], grassDM[3]))
  
  grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
  tsSumRegionMax$grassDMwet <- as.numeric(c(grassDMwet[1], grassDMwet[2], grassDMwet[3]))
  
  grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
  tsSumRegionMax$grassDMdry <- as.numeric(c(grassDMdry[1], grassDMdry[2], grassDMdry[3]))
  
  browseDM_max <- exact_extract(tBrowse/browseME, regions, "sum")
  tsSumRegionMax$browseDM <- as.numeric(c(browseDM_max[1], browseDM_max[2], browseDM_max[3]))
  
  browseDMwet_max <- exact_extract(tBrowseWet/browseME, regions, "sum")
  tsSumRegionMax$browseDMwet <- as.numeric(c(browseDMwet_max[1], browseDMwet_max[2], browseDMwet_max[3]))
  
  browseDMdry_max <- exact_extract(tBrowseDry/browseME, regions, "sum")
  tsSumRegionMax$browseDMdry <- as.numeric(c(browseDMdry_max[1], browseDMdry_max[2], browseDMdry_max[3]))
  
  tsSumRegionMax$afterDM <- tsSumRegionMax$afterME_max / feedQuality_item$ME_max[feedQuality_item$codeSPAM == "natPast"]
  
  tsSumRegionMax_List[[year]] <- tsSumRegionMax
  
  cat("Completed processing regional maximum stats for: ", year, "\n")
  
}

# For zonal stats
## Feed output
tsSumZoneMean <- read.csv(paste0(Results_dir, "/disaggregated_timeseries.csv")) %>% mutate(across(-1, as.numeric))
tsSumZoneMin <- map_dfr(tsSumZoneMin_List, ~ .x) %>% mutate(across(-1, as.numeric))
tsSumZoneMeanMin <- tsSumZoneMean %>% mutate(across(everything(), ~ coalesce(.x, tsSumZoneMin[[cur_column()]])))
tsSumZoneMax <- map_dfr(tsSumZoneMax_List, ~ .x) %>% mutate(across(-1, as.numeric))
tsSumZoneMeanMinMax <- tsSumZoneMeanMin %>% mutate(across(everything(), ~ coalesce(.x, tsSumZoneMax[[cur_column()]])))
write.csv(tsSumZoneMeanMinMax, paste0(Results_dir, "/disaggregated_timeseries.csv"), row.names=FALSE)

##Export total feed ME - minimum for adequacy estimates
feed_list <- c("crop", "grass", "browse", "after")
for (year in yearList){
  #tFeed <- stack(list.files(path = paste0(Outputs_dir), pattern = year, full.names = TRUE))
  tFeed <- paste0(Outputs_dir, "/Feed_", feed_list, "_min_MJ", year, ".tif")
  tFeed <- sum(stack(tFeed), na.rm = T)
  writeRaster(tFeed, paste0(Outputs_dir, "/Feed_total_min_MJ", year, ".tif"), overwrite = T)
}

##Export total feed ME - maximum for adequacy estimates
feed_list <- c("crop", "grass", "browse", "after")
for (year in yearList){
  #tFeed <- stack(list.files(path = paste0(Outputs_dir), pattern = year, full.names = TRUE))
  tFeed <- paste0(Outputs_dir, "/Feed_", feed_list, "_max_MJ", year, ".tif")
  tFeed <- sum(stack(tFeed), na.rm = T)
  writeRaster(tFeed, paste0(Outputs_dir, "/Feed_total_max_MJ", year, ".tif"), overwrite = T)
}

# For regional stats
outMEmean <- read.csv(paste0(Results_dir, "/cropME_region.csv")) %>% mutate(across(-1, as.numeric))
tsSumRegionMin <- map_dfr(tsSumRegionMin_List, ~ .x) %>% filter(year == "2023") %>% mutate(across(-1, as.numeric))
outMEmin <- tsSumRegionMin %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_min = cropME_min / cropDM, MEwet_all_min = sum(grassMEwet_min, browseMEwet_min) / sum(grassDMwet, browseDMwet), MEdry_all_min = sum(cropME_min, grassMEdry_min, browseMEdry_min) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
outMEmean <- bind_cols(outMEmean, select(outMEmin, ME_all_min, ME_crop_min, MEwet_all_min, MEdry_all_min))
tsSumRegionMax <- map_dfr(tsSumRegionMax_List, ~ .x) %>% filter(year == "2023") %>% mutate(across(-1, as.numeric))
outMEmax <- tsSumRegionMax %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_max = cropME_max / cropDM, MEwet_all_max = sum(grassMEwet_max, browseMEwet_max) / sum(grassDMwet, browseDMwet), MEdry_all_max = sum(cropME_max, grassMEdry_max, browseMEdry_max) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
outMEmean <- bind_cols(outMEmean, select(outMEmax, ME_all_max, ME_crop_max, MEwet_all_max, MEdry_all_max))
write.csv(outMEmean, paste0(Results_dir, "/cropME_region.csv"), row.names=FALSE)
