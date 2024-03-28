#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

memory.limit(size = 16000)

zones <- st_read('SpatialData/intermediate/zones.gpkg')
regions <- st_read('SpatialData/intermediate/elevationRegions.gpkg')

cropMEmin <- raster('SpatialData/intermediate/cropMEmin.tif')
cropME_HI_utilmin <- raster('SpatialData/intermediate/cropME_HI_utilmin.tif')
feedQuality_item <- read.csv('CropParams/feedQuality_item.csv')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')

#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster('CropParams/croppingDays.tif')
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays


region <- raster('SpatialData/intermediate/regions.tif')
grassMEhigh <- 6.3
grassMEmid <- 7.4
grassMElow <- 5.63 #lm
grassME <- calc(region, fun = function(x){ifelse(x == 3, grassMEhigh, ifelse(x ==1, grassMElow, grassMEmid))})


browseME <- read.csv('BrowseParams/browseME.csv')
browseMEmin <- min(c(browseME$ME_dry_min, browseME$ME_wet_min))

grassFracDry <- 0.33 
grassFracWet <- 0.55 
browseFrac <- 0.05

tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmin
for(i in 1:length(names(tCrop))){
tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
}

tCropBleg <- tCrop
for(i in 1:length(names(tCrop))){
  tCropBleg[[i]] <- overlay(tCrop[[i]], croppingDays, fun = function(DMP, days){ifelse(days > 120, DMP*0.3, 0) }) #Assume 30% of crop residue is available in the earler season - 90% of production is in Kiremt, so an underestimate
}

tCropKiremt <- tCrop
for(i in 1:length(names(tCrop))){
  tCropKiremt[[i]] <- overlay(tCrop[[i]], croppingDays, fun = function(DMP, days){ifelse(days > 120, DMP*0.7, DMP) }) #Assume 30% of crop residue is available in the earler season - 90% of production is in Kiremt, so an underestimate
}

tGrassWet <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassWetmean_2",full.names = T))
tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 

tGrassDry <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassDrymean_2",full.names = T))
tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME

tGrass <- tGrassWet + tGrassDry
#rm(tGrassDry, tGrassWet)

tBrowse <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPbrowsemean_2",full.names = T))
tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
tBrowseWet <- tBrowse*browseMEmin*croppingDays * browseFrac
tBrowseDry <- tBrowse*browseMEmin*dryDays * browseFrac
tBrowse <- tBrowseWet + tBrowseDry

tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays

rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmin)

## Feed output
tsSum <- read.csv('Results/disaggregated_timeseries.csv')

cropME_min <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,], cropME_min[3,], cropME_min[4,]))

grassME_min <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,], grassME_min[3,], grassME_min[4,]))

browseME_min <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,], browseME_min[3,], browseME_min[4,]))

afterME_min <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,], afterME_min[3,], afterME_min[4,]))

write.csv(tsSum, 'Results/disaggregated_timeseries.csv')

##Export total feed ME for adequacy estimates
tFeed <- tGrass
tFeed$layer.1 <- sum(tCrop$layer.1, tGrass$layer.1, tBrowse$layer.1, tAfter$layer.1, na.rm = T)
tFeed$layer.2 <- sum(tCrop$layer.2, tGrass$layer.2, tBrowse$layer.2, tAfter$layer.2, na.rm = T)
tFeed$layer.3 <- sum(tCrop$layer.3, tGrass$layer.3, tBrowse$layer.3, tAfter$layer.3, na.rm = T)
tFeed$layer.4 <- sum(tCrop$layer.4, tGrass$layer.4, tBrowse$layer.4, tAfter$layer.4, na.rm = T)
tFeed$layer.5 <- sum(tCrop$layer.5, tGrass$layer.5, tBrowse$layer.5, tAfter$layer.5, na.rm = T)
tFeed$layer.6 <- sum(tCrop$layer.6, tGrass$layer.6, tBrowse$layer.6, tAfter$layer.6, na.rm = T)
#tFeed <- tCrop + tGrass + tBrowse + tAfter
writeRaster(tFeed$layer.1, 'SpatialData/outputs/Feed_total_min_MJ2014.tif', overwrite = T)
writeRaster(tFeed$layer.2, 'SpatialData/outputs/Feed_total_min_MJ2015.tif', overwrite = T)
writeRaster(tFeed$layer.3, 'SpatialData/outputs/Feed_total_min_MJ2016.tif', overwrite = T)
writeRaster(tFeed$layer.4, 'SpatialData/outputs/Feed_total_min_MJ2017.tif', overwrite = T)
writeRaster(tFeed$layer.5, 'SpatialData/outputs/Feed_total_min_MJ2018.tif', overwrite = T)
writeRaster(tFeed$layer.6, 'SpatialData/outputs/Feed_total_min_MJ2019.tif', overwrite = T)

##Calculate average ME
tsSum <- data.frame(region = c(rep("Highland", 6), rep("Lowland", 6)), year = c(2014:2019, 2014:2019))

cropME_min <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,]))

grassME_min <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,]))

browseME_min <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,]))

afterME_min <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,]))

rm(tAfter)

##Calculate ME of grass and browse by season
cropMEbleg_min <- exact_extract(tCropBleg, regions, "sum")
tsSum$cropMEbleg_min <- as.numeric(c(cropMEbleg_min[1,], cropMEbleg_min[2,]))

cropMEkiremt_min <- exact_extract(tCropKiremt, regions, "sum")
tsSum$cropMEkiremt_min <- as.numeric(c(cropMEkiremt_min[1,], cropMEkiremt_min[2,]))

grassMEwet_min <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_min <- as.numeric(c(grassMEwet_min[1,], grassMEwet_min[2,]))

grassMEdry_min <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_min <- as.numeric(c(grassMEdry_min[1,], grassMEdry_min[2,]))

browseMEwet_min <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_min <- as.numeric(c(browseMEwet_min[1,], browseMEwet_min[2,]))

browseMEdry_min <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_min <- as.numeric(c(browseMEdry_min[1,], browseMEdry_min[2,]))

#Add DM
tCrop <- tCrop/cropMEmin
cropDM_min <- exact_extract(tCrop, regions, "sum")
tsSum$cropDM_min <- as.numeric(c(cropDM_min[1,], cropDM_min[2,]))

rm(tCrop)

tCropBleg <- tCropBleg/cropMEmin
cropDMbleg_min <- exact_extract(tCropBleg, regions, "sum")
tsSum$cropDMbleg_min <- as.numeric(c(cropDMbleg_min[1,], cropDMbleg_min[2,]))

tCropKiremt <- tCropKiremt/cropMEmin
cropDMkiremt_min <- exact_extract(tCropKiremt, regions, "sum")
tsSum$cropDMkiremt_min <- as.numeric(c(cropDMkiremt_min[1,], cropDMkiremt_min[2,]))

rm(tCropBleg, tCropKiremt)

grassDM_min <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM_min <- as.numeric(c(grassDM_min[1,], grassDM_min[2,]))

grassDMwet_min <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet_min <- as.numeric(c(grassDMwet_min[1,], grassDMwet_min[2,]))

grassDMdry_min <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry_min <- as.numeric(c(grassDMdry_min[1,], grassDMdry_min[2,]))

browseDM_mean_min <- exact_extract(tBrowse/browseMEmin, regions, "sum")
tsSum$browseDM_min <- as.numeric(c(browseDM_mean_min[1,], browseDM_mean_min[2,]))

browseDMwet_min <- exact_extract(tBrowseWet/browseMEmin, regions, "sum")
tsSum$browseDMwet_min <- as.numeric(c(browseDMwet_min[1,], browseDMwet_min[2,]))

browseDMdry_min <- exact_extract(tBrowseDry/browseMEmin, regions, "sum")
tsSum$browseDMdry_min <- as.numeric(c(browseDMdry_min[1,], browseDMdry_min[2,]))

tsSum$afterDM_min <- tsSum$afterME_min / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv('Results/cropME_region.csv')

outMEmin <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM_min, grassDM_min, browseDM_min, afterDM_min), ME_crop_min = cropME_min / cropDM_min, MEwet_all_min = sum(cropMEbleg_min, cropMEkiremt_min, grassMEwet_min, browseMEwet_min) / sum(cropDMbleg_min, cropDMkiremt_min,  grassDMwet_min, browseDMwet_min), MEdry_all_min = sum(grassMEdry_min, browseMEdry_min) / sum(grassDMdry_min, browseDMdry_min)) 

outMEmean <- bind_cols(outMEmean, select(outMEmin, ME_all_min, ME_crop_min, MEwet_all_min, MEdry_all_min))
write.csv(outMEmean, 'Results/cropME_region.csv')

rm(list = ls())

####################################
#Duplicate for max values
zones <- st_read('SpatialData/intermediate/zones.gpkg')
regions <- st_read('SpatialData/intermediate/elevationRegions.gpkg')

cropMEmax <- raster('SpatialData/intermediate/cropMEmax.tif')
cropME_HI_utilmax <- raster('SpatialData/intermediate/cropME_HI_utilmax.tif')
feedQuality_item <- read.csv('CropParams/feedQuality_item.csv')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')


#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster('CropParams/croppingDays.tif')
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster('SpatialData/intermediate/regions.tif')
grassMEhigh <- 9.7
grassMEmid <- 8.2
grassMElow <- 9.86 #(0.01xOM (IVOMD + 12.9) 4.4 - 0.3)x0.82 (NRC, 2001); IVDMD was assumed to be a close proxy to IVOMD 
grassME <- calc(region, fun = function(x){ifelse(x == 3, grassMEhigh, ifelse(x ==1, grassMElow, grassMEmid))})



browseME <- read.csv('BrowseParams/browseME.csv')
browseMEmax <- weighted.mean(browseME$ME_wet_mean, browseME$N) #Set at weighted mean # max(c(browseME$ME_dry_max, browseME$ME_wet_max))

grassFracDry <- 0.33 
grassFracWet <- 0.55
browseFrac <- 0.38 #Rahimi et al. use 38%

tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmax
for(i in 1:length(names(tCrop))){
  tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
}

tCropBleg <- tCrop
for(i in 1:length(names(tCrop))){
  tCropBleg[[i]] <- overlay(tCrop[[i]], croppingDays, fun = function(DMP, days){ifelse(days > 120, DMP*0.3, 0) }) #Assume 30% of crop residue is available in the earler season - 70% of production is in Kiremt, so an underestimate
}

tCropKiremt <- tCrop
for(i in 1:length(names(tCrop))){
  tCropKiremt[[i]] <- overlay(tCrop[[i]], croppingDays, fun = function(DMP, days){ifelse(days > 120, DMP*0.7, DMP) }) #Assume 30% of crop residue is available in the earler season - >70% of production is in Kiremt, so an underestimate
}

tGrassWet <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassWetmean_2",full.names = T))
tGrassWet <- reclassify(tGrassWet, c(-Inf, 0, 0)) #Some negative DM in copernicus product
tGrassWet <- tGrassWet*grassFracWet*croppingDays * grassME 

tGrassDry <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPgrassDrymean_2",full.names = T))
tGrassDry <- reclassify(tGrassDry, c(-Inf, 0, 0)) 
tGrassDry <- tGrassDry*grassFracDry*dryDays * grassME

tGrass <- tGrassWet + tGrassDry
#rm(tGrassDry, tGrassWet)

tBrowse <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPbrowsemean_2",full.names = T))
tBrowse <- reclassify(tBrowse, c(-Inf, 0, 0)) 
tBrowseWet <- tBrowse*browseMEmax*croppingDays * browseFrac
tBrowseDry <- tBrowse*browseMEmax*dryDays * browseFrac
tBrowse <- tBrowseWet + tBrowseDry

tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays

rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmax)

## Feed output
tsSum <- read.csv('Results/disaggregated_timeseries.csv')

cropME_max <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,], cropME_max[3,], cropME_max[4,]))

grassME_max <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,], grassME_max[3,], grassME_max[4,]))

browseME_max <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,], browseME_max[3,], browseME_max[4,]))

afterME_max <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,], afterME_max[3,], afterME_max[4,]))

write.csv(tsSum, 'Results/disaggregated_timeseries.csv')

##Export total feed ME for adequacy estimates
tFeed <- tGrass
tFeed$layer.1 <- sum(tCrop$layer.1, tGrass$layer.1, tBrowse$layer.1, tAfter$layer.1, na.rm = T)
tFeed$layer.2 <- sum(tCrop$layer.2, tGrass$layer.2, tBrowse$layer.2, tAfter$layer.2, na.rm = T)
tFeed$layer.3 <- sum(tCrop$layer.3, tGrass$layer.3, tBrowse$layer.3, tAfter$layer.3, na.rm = T)
tFeed$layer.4 <- sum(tCrop$layer.4, tGrass$layer.4, tBrowse$layer.4, tAfter$layer.4, na.rm = T)
tFeed$layer.5 <- sum(tCrop$layer.5, tGrass$layer.5, tBrowse$layer.5, tAfter$layer.5, na.rm = T)
tFeed$layer.6 <- sum(tCrop$layer.6, tGrass$layer.6, tBrowse$layer.6, tAfter$layer.6, na.rm = T)
#tFeed <- tCrop + tGrass + tBrowse + tAfter
writeRaster(tFeed$layer.1, 'SpatialData/outputs/Feed_total_max_MJ2014.tif', overwrite = T)
writeRaster(tFeed$layer.2, 'SpatialData/outputs/Feed_total_max_MJ2015.tif', overwrite = T)
writeRaster(tFeed$layer.3, 'SpatialData/outputs/Feed_total_max_MJ2016.tif', overwrite = T)
writeRaster(tFeed$layer.4, 'SpatialData/outputs/Feed_total_max_MJ2017.tif', overwrite = T)
writeRaster(tFeed$layer.5, 'SpatialData/outputs/Feed_total_max_MJ2018.tif', overwrite = T)
writeRaster(tFeed$layer.6, 'SpatialData/outputs/Feed_total_max_MJ2019.tif', overwrite = T)

##Calculate average ME
tsSum <- data.frame(region = c(rep("Highland", 6), rep("Lowland", 6)), year = c(2014:2019, 2014:2019))

cropME_max <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,]))

grassME_max <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,]))

browseME_max <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,]))

afterME_max <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,]))

rm(tAfter)

##Calculate ME of grass and browse by season
cropMEbleg_max <- exact_extract(tCropBleg, regions, "sum")
tsSum$cropMEbleg_max <- as.numeric(c(cropMEbleg_max[1,], cropMEbleg_max[2,]))

cropMEkiremt_max <- exact_extract(tCropKiremt, regions, "sum")
tsSum$cropMEkiremt_max <- as.numeric(c(cropMEkiremt_max[1,], cropMEkiremt_max[2,]))

grassMEwet_max <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_max <- as.numeric(c(grassMEwet_max[1,], grassMEwet_max[2,]))

grassMEdry_max <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_max <- as.numeric(c(grassMEdry_max[1,], grassMEdry_max[2,]))

browseMEwet_max <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_max <- as.numeric(c(browseMEwet_max[1,], browseMEwet_max[2,]))

browseMEdry_max <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_max <- as.numeric(c(browseMEdry_max[1,], browseMEdry_max[2,]))

#Add DM
tCrop <- tCrop/cropMEmax
cropDM_max <- exact_extract(tCrop, regions, "sum")
tsSum$cropDM_max <- as.numeric(c(cropDM_max[1,], cropDM_max[2,]))

rm(tCrop)

tCropBleg <- tCropBleg/cropMEmax
cropDMbleg_max <- exact_extract(tCropBleg, regions, "sum")
tsSum$cropDMbleg_max <- as.numeric(c(cropDMbleg_max[1,], cropDMbleg_max[2,]))

tCropKiremt <- tCropKiremt/cropMEmax
cropDMkiremt_max <- exact_extract(tCropKiremt, regions, "sum")
tsSum$cropDMkiremt_max <- as.numeric(c(cropDMkiremt_max[1,], cropDMkiremt_max[2,]))

rm(tCropBleg, tCropKiremt)

grassDM_max <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM_max <- as.numeric(c(grassDM_max[1,], grassDM_max[2,]))

grassDMwet_max <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet_max <- as.numeric(c(grassDMwet_max[1,], grassDMwet_max[2,]))

grassDMdry_max <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry_max <- as.numeric(c(grassDMdry_max[1,], grassDMdry_max[2,]))

browseDM_max <- exact_extract(tBrowse/browseMEmax, regions, "sum")
tsSum$browseDM_max <- as.numeric(c(browseDM_max[1,], browseDM_max[2,]))

browseDMwet_max <- exact_extract(tBrowseWet/browseMEmax, regions, "sum")
tsSum$browseDMwet_max <- as.numeric(c(browseDMwet_max[1,], browseDMwet_max[2,]))

browseDMdry_max <- exact_extract(tBrowseDry/browseMEmax, regions, "sum")
tsSum$browseDMdry_max <- as.numeric(c(browseDMdry_max[1,], browseDMdry_max[2,]))

tsSum$afterDM_max <- tsSum$afterME_max / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv('Results/cropME_region.csv')

outMEmax <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM_max, grassDM_max, browseDM_max, afterDM_max), ME_crop_max = cropME_max / cropDM_max, MEwet_all_max = sum(cropMEbleg_max, grassMEwet_max, browseMEwet_max) / sum(cropDMbleg_max, grassDMwet_max, browseDMwet_max), MEdry_all_max = sum(cropMEkiremt_max, grassMEdry_max, browseMEdry_max) / sum(cropDMkiremt_max, grassDMdry_max, browseDMdry_max)) 
outMEmean <- bind_cols(outMEmean, select(outMEmax, ME_all_max, ME_crop_max, MEwet_all_max, MEdry_all_max))

outMEmean <- outMEmean %>% rowwise() %>% mutate(geomMean = sqrt(ME_crop_min*ME_crop_max))
outMEmean <- outMEmean %>% rowwise() %>% mutate(ME_crop_sd = (ME_crop/geomMean)*(sqrt((ME_crop^2)-(geomMean^2))))
write.csv(outMEmean, 'Results/cropME_region.csv')
