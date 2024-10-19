.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

zones <- st_read('SpatialData/intermediate/zones.gpkg')
regions <- st_read('SpatialData/intermediate/regions.gpkg')

cropME_HI_utilmean <- raster('SpatialData/intermediate/cropME_HI_utilmean.tif')
cropMEmean <- raster('SpatialData/intermediate/cropMEmean.tif')
feedQuality_item <- read.csv('CropParams/feedQuality_item.csv')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')


#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster('CropParams/croppingDays.tif')
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster('SpatialData/intermediate//regions.tif')
grassMESud <- 6.5
grassMESah <- 6.1
grassME <- calc(region, fun = function(x){ifelse(x == 4, grassMESah, grassMESud)})
grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")

browseMESud <- 8.1
browseMESah <- 6
browseME <- calc(region, fun = function(x){ifelse(x == 4, browseMESah, browseMESud)})
browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
#browseMEmean <- 5

grassFracDry <- 0.33 
grassFracWet <- 0.55 
browseFrac <- 0.16

tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmean #reclassify for crops to make sure there is a logical minimum cropping period
for(i in 1:length(names(tCrop))){
tCrop[[i]] <- overlay(tCrop[[i]], feedCropBurn, fun = function(DMP, burn){ifelse(burn > 0, 0, DMP) }) #, filename = 'SpatialData/outputs/Feed_crop_burn_MJ.tif', overwrite = T
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
tBrowseWet <- tBrowse*browseME*croppingDays * browseFrac
tBrowseDry <- tBrowse*browseME*dryDays * browseFrac

tBrowse <- tBrowseWet + tBrowseDry

tAfter <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="DMPaftermean_2",full.names = T))
tAfter <- reclassify(tAfter, c(-Inf, 0, 0)) 
tAfter <- tAfter*grassFracDry*feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]*dryDays

rm(dryDays, croppingDays, feedCropBurn)

## Feed output
tsSum <- data.frame(region = c(rep("(Agro)pastoral Sahel", 6), rep("Central mixed", 6), rep("Cropping", 6), rep("North mixed", 6), rep("South mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA)
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)

cropME_mean <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_mean <- as.numeric(c(cropME_mean[1,], cropME_mean[2,], cropME_mean[3,], cropME_mean[4,], cropME_mean[5,]))

grassME_mean <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_mean <- as.numeric(c(grassME_mean[1,], grassME_mean[2,], grassME_mean[3,], grassME_mean[4,], grassME_mean[5,]))

browseME_mean <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_mean <- as.numeric(c(browseME_mean[1,], browseME_mean[2,], browseME_mean[3,], browseME_mean[4,], browseME_mean[5,]))

afterME_mean <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_mean <- as.numeric(c(afterME_mean[1,], afterME_mean[2,], afterME_mean[3,], afterME_mean[4,], afterME_mean[5,]))


write.csv(tsSum, 'Results/disaggregated_timeseries.csv')
writeRaster(tCrop$layer.2, 'SpatialData/outputs/Feed_crop_MJ2019.tif', overwrite = T) #@odd naming of these layers due to overlay?
writeRaster(tGrass$layer.6, 'SpatialData/outputs/Feed_grass_MJ2019.tif', overwrite = T)
writeRaster(tBrowse$layer.6, 'SpatialData/outputs/Feed_browse_MJ2019.tif', overwrite = T)
writeRaster(tAfter$layer.6, 'SpatialData/outputs/Feed_after_MJ2019.tif', overwrite = T)

##Export total feed ME for adequacy estimates
tFeed <- tGrass
tFeed$layer.1 <- sum(tCrop$layer.1.1, tGrass$layer.1, tBrowse$layer.1, tAfter$layer.1, na.rm = T)
tFeed$layer.2 <- sum(tCrop$layer.2.1, tGrass$layer.2, tBrowse$layer.2, tAfter$layer.2, na.rm = T)
tFeed$layer.3 <- sum(tCrop$layer.1.2, tGrass$layer.3, tBrowse$layer.3, tAfter$layer.3, na.rm = T)
tFeed$layer.4 <- sum(tCrop$layer.2.2, tGrass$layer.4, tBrowse$layer.4, tAfter$layer.4, na.rm = T)
tFeed$layer.5 <- sum(tCrop$layer.1, tGrass$layer.5, tBrowse$layer.5, tAfter$layer.5, na.rm = T)
tFeed$layer.6 <- sum(tCrop$layer.2, tGrass$layer.6, tBrowse$layer.6, tAfter$layer.6, na.rm = T)
#tFeed <- tCrop + tGrass + tBrowse + tAfter
writeRaster(tFeed$layer.1, 'SpatialData/outputs/Feed_total_mean_MJ2014.tif', overwrite = T)
writeRaster(tFeed$layer.2, 'SpatialData/outputs/Feed_total_mean_MJ2015.tif', overwrite = T)
writeRaster(tFeed$layer.3, 'SpatialData/outputs/Feed_total_mean_MJ2016.tif', overwrite = T)
writeRaster(tFeed$layer.4, 'SpatialData/outputs/Feed_total_mean_MJ2017.tif', overwrite = T)
writeRaster(tFeed$layer.5, 'SpatialData/outputs/Feed_total_mean_MJ2018.tif', overwrite = T)
writeRaster(tFeed$layer.6, 'SpatialData/outputs/Feed_total_mean_MJ2019.tif', overwrite = T)


##Calculate average ME
tsSum <- data.frame(region = c(rep("Sahel", 6), rep("Sudanian", 6)), year = c(2014:2019, 2014:2019))
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)

cropME_mean <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_mean <- as.numeric(c(cropME_mean[1,], cropME_mean[2,]))

grassME_mean <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_mean <- as.numeric(c(grassME_mean[1,], grassME_mean[2,]))

browseME_mean <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_mean <- as.numeric(c(browseME_mean[1,], browseME_mean[2,]))

afterME_mean <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_mean <- as.numeric(c(afterME_mean[1,], afterME_mean[2,]))

##Calculate ME of grass and browse by season
grassMEwet_mean <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_mean <- as.numeric(c(grassMEwet_mean[1,], grassMEwet_mean[2,]))

grassMEdry_mean <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_mean <- as.numeric(c(grassMEdry_mean[1,], grassMEdry_mean[2,]))

browseMEwet_mean <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_mean <- as.numeric(c(browseMEwet_mean[1,], browseMEwet_mean[2,]))

browseMEdry_mean <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_mean <- as.numeric(c(browseMEdry_mean[1,], browseMEdry_mean[2,]))

#Add DM
cropDM <- exact_extract(tCrop/cropMEmean, regions, "sum")
tsSum$cropDM <- as.numeric(c(cropDM[1,], cropDM[2,]))

grassDM <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM <- as.numeric(c(grassDM[1,], grassDM[2,]))

grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet <- as.numeric(c(grassDMwet[1,], grassDMwet[2,]))

grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry <- as.numeric(c(grassDMdry[1,], grassDMdry[2,]))

browseDM_mean <- exact_extract(tBrowse/browseME, regions, "sum")
tsSum$browseDM <- as.numeric(c(browseDM_mean[1,], browseDM_mean[2,]))

browseDMwet_mean <- exact_extract(tBrowseWet/browseME, regions, "sum")
tsSum$browseDMwet <- as.numeric(c(browseDMwet_mean[1,], browseDMwet_mean[2,]))

browseDMdry_mean <- exact_extract(tBrowseDry/browseME, regions, "sum")
tsSum$browseDMdry <- as.numeric(c(browseDMdry_mean[1,], browseDMdry_mean[2,]))

tsSum$afterDM <- tsSum$afterME_mean / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all = sum(cropME_mean, grassME_mean, browseME_mean, afterME_mean) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop = cropME_mean / cropDM, MEwet_all = sum(grassMEwet_mean, browseMEwet_mean) / sum(grassDMwet, browseDMwet), MEdry_all = sum(cropME_mean, grassMEdry_mean, browseMEdry_mean) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
write.csv(outMEmean, 'Results/cropME_region.csv')
