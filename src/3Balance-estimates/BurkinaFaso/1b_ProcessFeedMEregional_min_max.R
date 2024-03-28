#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

zones <- st_read('SpatialData/intermediate/zones.gpkg')
regions <- st_read('SpatialData/intermediate/regions.gpkg')

cropME_HI_utilmin <- raster('SpatialData/intermediate/cropME_HI_utilmin.tif')
cropMEmin <- raster('SpatialData/intermediate/cropMEmin.tif')
feedQuality_item <- read.csv('CropParams/feedQuality_item.csv')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')


#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster('CropParams/croppingDays.tif')
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster('SpatialData/intermediate/regions.tif')
grassMESud <- 6.2 #min = 6.2, max = 6.8
grassMESah <- 5.8 #min = 5.8, max = 6.4
grassME <- calc(region, fun = function(x){ifelse(x == 4, grassMESah, grassMESud)})
grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")

browseMESud <- 5.8
browseMESah <- 8
browseME <- calc(region, fun = function(x){ifelse(x == 4, browseMESah, browseMESud)})
browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
#browseMEmean <- 5

grassFracDry <- 0.33 #max 0.55
grassFracWet <- 0.55 #max 0.55
browseFrac <- 0.05

tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmin
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

rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmin)

## Feed output
tsSum <- read.csv('Results/disaggregated_timeseries.csv')

cropME_min <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,], cropME_min[3,], cropME_min[4,], cropME_min[5,]))

grassME_min <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,], grassME_min[3,], grassME_min[4,], grassME_min[5,]))

browseME_min <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,], browseME_min[3,], browseME_min[4,], browseME_min[5,]))

afterME_min <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,], afterME_min[3,], afterME_min[4,], afterME_min[5,]))

write.csv(tsSum, 'Results/disaggregated_timeseries.csv')

##Export total feed ME for adequacy estimates
tFeed <- tCrop + tGrass + tBrowse + tAfter
writeRaster(tFeed$layer.1, 'SpatialData/outputs/Feed_total_min_MJ2014.tif', overwrite = T)
writeRaster(tFeed$layer.2, 'SpatialData/outputs/Feed_total_min_MJ2015.tif', overwrite = T)
writeRaster(tFeed$layer.3, 'SpatialData/outputs/Feed_total_min_MJ2016.tif', overwrite = T)
writeRaster(tFeed$layer.4, 'SpatialData/outputs/Feed_total_min_MJ2017.tif', overwrite = T)
writeRaster(tFeed$layer.5, 'SpatialData/outputs/Feed_total_min_MJ2018.tif', overwrite = T)
writeRaster(tFeed$layer.6, 'SpatialData/outputs/Feed_total_min_MJ2019.tif', overwrite = T)


##Calculate average ME
tsSum <- data.frame(region = c(rep("Sahel", 6), rep("Sudanian", 6)), year = c(2014:2019, 2014:2019))
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_min = NA, cropME_max = NA, grassME_mean = NA, grassME_min = NA, grassME_max = NA, browseME_mean = NA, browseME_min = NA, browseME_max = NA, afterME_mean = NA, afterME_min = NA, afterME_max = NA, adeq_mean = NA, adeq_min = NA, adeq_max = NA)

cropME_min <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_min <- as.numeric(c(cropME_min[1,], cropME_min[2,]))

grassME_min <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_min <- as.numeric(c(grassME_min[1,], grassME_min[2,]))

browseME_min <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_min <- as.numeric(c(browseME_min[1,], browseME_min[2,]))

afterME_min <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_min <- as.numeric(c(afterME_min[1,], afterME_min[2,]))

##Calculate ME of grass and browse by season
grassMEwet_min <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_min <- as.numeric(c(grassMEwet_min[1,], grassMEwet_min[2,]))

grassMEdry_min <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_min <- as.numeric(c(grassMEdry_min[1,], grassMEdry_min[2,]))

browseMEwet_min <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_min <- as.numeric(c(browseMEwet_min[1,], browseMEwet_min[2,]))

browseMEdry_min <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_min <- as.numeric(c(browseMEdry_min[1,], browseMEdry_min[2,]))

#Add DM

cropDM <- exact_extract(tCrop/cropMEmin, regions, "sum")
tsSum$cropDM <- as.numeric(c(cropDM[1,], cropDM[2,]))

grassDM <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM <- as.numeric(c(grassDM[1,], grassDM[2,]))

grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet <- as.numeric(c(grassDMwet[1,], grassDMwet[2,]))

grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry <- as.numeric(c(grassDMdry[1,], grassDMdry[2,]))

browseDM_min <- exact_extract(tBrowse/browseME, regions, "sum")
tsSum$browseDM <- as.numeric(c(browseDM_min[1,], browseDM_min[2,]))

browseDMwet_min <- exact_extract(tBrowseWet/browseME, regions, "sum")
tsSum$browseDMwet <- as.numeric(c(browseDMwet_min[1,], browseDMwet_min[2,]))

browseDMdry_min <- exact_extract(tBrowseDry/browseME, regions, "sum")
tsSum$browseDMdry <- as.numeric(c(browseDMdry_min[1,], browseDMdry_min[2,]))

tsSum$afterDM <- tsSum$afterME_min / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv('Results/cropME_region.csv')

outMEmin <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_min = cropME_min / cropDM, MEwet_all_min = sum(grassMEwet_min, browseMEwet_min) / sum(grassDMwet, browseDMwet), MEdry_all_min = sum(cropME_min, grassMEdry_min, browseMEdry_min) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
#outMEmin <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_min = sum(cropME_min, grassME_min, browseME_min, afterME_min) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_min = cropME_min / cropDM) 
outMEmean <- bind_cols(outMEmean, select(outMEmin, ME_all_min, ME_crop_min, MEwet_all_min, MEdry_all_min))
write.csv(outMEmean, 'Results/cropME_region.csv')


#######
##Duplicate for maximum
zones <- st_read('SpatialData/intermediate/zones.gpkg')
regions <- st_read('SpatialData/intermediate/regions.gpkg')

cropME_HI_utilmax <- raster('SpatialData/intermediate/cropME_HI_utilmax.tif')
cropMEmax <- raster('SpatialData/intermediate/cropMEmax.tif')
feedQuality_item <- read.csv('CropParams/feedQuality_item.csv')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')

#croppingDays <- sum(raster('CropParams/phenoCroppingDays1.tif'), raster('CropParams/phenoCroppingDays2.tif'), na.rm = T)
croppingDays <- raster('CropParams/croppingDays.tif')
croppingDays <- reclassify(croppingDays, c(-Inf, 0, 0)) 
croppingDays <- raster::resample(croppingDays, feedCropBurn, method = "ngb")
croppingDays <- reclassify(croppingDays, c(220, Inf, 220)) 
dryDays <- 365 - croppingDays

region <- raster('SpatialData/intermediate/regions.tif')
grassMESud <- 6.8 #min = 6.2, max = 6.8 #!change max/min 
grassMESah <- 6.4 #min = 5.8, max = 6.4 #!change max/min
grassME <- calc(region, fun = function(x){ifelse(x == 4, grassMESah, grassMESud)})
grassME <- raster::resample(grassME, feedCropBurn, method = "ngb")

browseMESud <- 8.2
browseMESah <- 6.2
browseME <- calc(region, fun = function(x){ifelse(x == 4, browseMESah, browseMESud)})
browseME <- raster::resample(browseME, feedCropBurn, method = "ngb")
#browseMEmean <- 5

grassFracDry <- 0.33 #max 0.55
grassFracWet <- 0.55 #max 0.55
browseFrac <- 0.38

tCrop <- stack(list.files(path = 'SpatialData/inputs/Feed_quantity/',pattern="cropmean_2",full.names = T))
tCrop <- reclassify(tCrop, c(-Inf, 0, 0)) 
tCrop <- tCrop*reclassify(croppingDays, c(0, 60, 60))*cropME_HI_utilmax
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

rm(dryDays, croppingDays, feedCropBurn, cropME_HI_utilmax)

## Feed output
tsSum <- read.csv('Results/disaggregated_timeseries.csv')

cropME_max <- exact_extract(tCrop, zones, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,], cropME_max[3,], cropME_max[4,], cropME_max[5,]))

grassME_max <- exact_extract(tGrass, zones, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,], grassME_max[3,], grassME_max[4,], grassME_max[5,]))

browseME_max <- exact_extract(tBrowse, zones, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,], browseME_max[3,], browseME_max[4,], browseME_max[5,]))

afterME_max <- exact_extract(tAfter, zones, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,], afterME_max[3,], afterME_max[4,], afterME_max[5,]))


write.csv(tsSum, 'Results/disaggregated_timeseries.csv')

##Export total feed ME for adequacy estimates
tFeed <- tCrop + tGrass + tBrowse + tAfter
writeRaster(tFeed$layer.1, 'SpatialData/outputs/Feed_total_max_MJ2014.tif', overwrite = T)
writeRaster(tFeed$layer.2, 'SpatialData/outputs/Feed_total_max_MJ2015.tif', overwrite = T)
writeRaster(tFeed$layer.3, 'SpatialData/outputs/Feed_total_max_MJ2016.tif', overwrite = T)
writeRaster(tFeed$layer.4, 'SpatialData/outputs/Feed_total_max_MJ2017.tif', overwrite = T)
writeRaster(tFeed$layer.5, 'SpatialData/outputs/Feed_total_max_MJ2018.tif', overwrite = T)
writeRaster(tFeed$layer.6, 'SpatialData/outputs/Feed_total_max_MJ2019.tif', overwrite = T)


##Calculate average ME
tsSum <- data.frame(region = c(rep("Sahel", 6), rep("Sudanian", 6)), year = c(2014:2019, 2014:2019))
#tsSum <- data.frame(region = c(rep("Highland (agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland (agro)pastoral", 6), rep("Lowland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), lvstReq = NA, cropME_mean = NA, cropME_max = NA, cropME_max = NA, grassME_mean = NA, grassME_max = NA, grassME_max = NA, browseME_mean = NA, browseME_max = NA, browseME_max = NA, afterME_mean = NA, afterME_max = NA, afterME_max = NA, adeq_mean = NA, adeq_max = NA, adeq_max = NA)

cropME_max <- exact_extract(tCrop, regions, "sum")
tsSum$cropME_max <- as.numeric(c(cropME_max[1,], cropME_max[2,]))

grassME_max <- exact_extract(tGrass, regions, "sum")
tsSum$grassME_max <- as.numeric(c(grassME_max[1,], grassME_max[2,]))

browseME_max <- exact_extract(tBrowse, regions, "sum")
tsSum$browseME_max <- as.numeric(c(browseME_max[1,], browseME_max[2,]))

afterME_max <- exact_extract(tAfter, regions, "sum")
tsSum$afterME_max <- as.numeric(c(afterME_max[1,], afterME_max[2,]))

##Calculate ME of grass and browse by season
grassMEwet_max <- exact_extract(tGrassWet, regions, "sum")
tsSum$grassMEwet_max <- as.numeric(c(grassMEwet_max[1,], grassMEwet_max[2,]))

grassMEdry_max <- exact_extract(tGrassDry, regions, "sum")
tsSum$grassMEdry_max <- as.numeric(c(grassMEdry_max[1,], grassMEdry_max[2,]))

browseMEwet_max <- exact_extract(tBrowseWet, regions, "sum")
tsSum$browseMEwet_max <- as.numeric(c(browseMEwet_max[1,], browseMEwet_max[2,]))

browseMEdry_max <- exact_extract(tBrowseDry, regions, "sum")
tsSum$browseMEdry_max <- as.numeric(c(browseMEdry_max[1,], browseMEdry_max[2,]))

#Add DM
cropDM <- exact_extract(tCrop/cropMEmax, regions, "sum")
tsSum$cropDM <- as.numeric(c(cropDM[1,], cropDM[2,]))

grassDM <- exact_extract(tGrass/grassME, regions, "sum")
tsSum$grassDM <- as.numeric(c(grassDM[1,], grassDM[2,]))

grassDMwet <- exact_extract(tGrassWet/grassME, regions, "sum")
tsSum$grassDMwet <- as.numeric(c(grassDMwet[1,], grassDMwet[2,]))

grassDMdry <- exact_extract(tGrassDry/grassME, regions, "sum")
tsSum$grassDMdry <- as.numeric(c(grassDMdry[1,], grassDMdry[2,]))

browseDM_max <- exact_extract(tBrowse/browseME, regions, "sum")
tsSum$browseDM <- as.numeric(c(browseDM_max[1,], browseDM_max[2,]))

browseDMwet_max <- exact_extract(tBrowseWet/browseME, regions, "sum")
tsSum$browseDMwet <- as.numeric(c(browseDMwet_max[1,], browseDMwet_max[2,]))

browseDMdry_max <- exact_extract(tBrowseDry/browseME, regions, "sum")
tsSum$browseDMdry <- as.numeric(c(browseDMdry_max[1,], browseDMdry_max[2,]))

tsSum$afterDM <- tsSum$afterME_max / feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"]

outMEmean <- read.csv('Results/cropME_region.csv')

outMEmax <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_max = cropME_max / cropDM, MEwet_all_max = sum(grassMEwet_max, browseMEwet_max) / sum(grassDMwet, browseDMwet), MEdry_all_max = sum(cropME_max, grassMEdry_max, browseMEdry_max) / sum(cropDM, grassDMdry, browseDMdry, afterDM)) 
#outMEmax <- tsSum[tsSum$year == 2019,] %>% rowwise() %>% mutate(ME_all_max = sum(cropME_max, grassME_max, browseME_max, afterME_max) / sum(cropDM, grassDM, browseDM, afterDM), ME_crop_max = cropME_max / cropDM) 
outMEmean <- bind_cols(outMEmean, select(outMEmax, ME_all_max, ME_crop_max, MEwet_all_max, MEdry_all_max))

outMEmean <- outMEmean %>% rowwise() %>% mutate(geomMean = sqrt(ME_crop_min*ME_crop_max))
outMEmean <- outMEmean %>% rowwise() %>% mutate(ME_crop_sd = (ME_crop/geomMean)*(sqrt((ME_crop^2)-(geomMean^2))))
write.csv(outMEmean, 'Results/cropME_region.csv')


