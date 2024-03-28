#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

#memory.limit(size = 16000)

zones <- st_read('SpatialData/intermediate/zones.gpkg')


###Livestock requirements
cattleIntake_model_MJ_2015 <- raster('SpatialData/outputs/cattleMER_MJ_2015_max.tif') #Max feed requirement will give min adequacy
shoatsIntake_model_MJ_2015 <- raster('SpatialData/outputs/shoatsMER_MJ_2015_max.tif')
horseDonkeyCamelIntake_model_MJ_2015 <- raster('SpatialData/outputs/horseDonkeyCamelMER_MJ_2015.tif')

FAOlvstPop <- read.csv('LivestockParams/FAOSTAT_livestock_data.csv')

lv2014 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2015 <- sum(horseDonkeyCamelIntake_model_MJ_2015, cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)
lv2016 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2017 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2018 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2019 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Shoats"])), na.rm = T)

tLv <- stack(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019)
rm(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019, shoatsIntake_model_MJ_2015, cattleIntake_model_MJ_2015, horseDonkeyCamelIntake_model_MJ_2015)


## Adequacy outputs
tFeed <- stack(list.files(path = 'SpatialData/outputs',pattern="Feed_total_min_MJ",full.names = T))
zones <- bind_cols(select(zones, grouping), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

tsSum <- zones
st_geometry(tsSum) <- NULL

tsSum <- data.frame(tsSum)

colnames(tsSum) <- c("NAME_1", "lvstReqME_2014", "lvstReqME_2015", "lvstReqME_2016", "lvstReqME_2017", "lvstReqME_2018", "lvstReqME_2019", "feedMEmin_2014", "feedMEmin_2015", "feedMEmin_2016", "feedMEmin_2017", "feedMEmin_2018", "feedMEmin_2019")

tsSum$adeqMin_2014 <- tsSum$feedMEmin_2014 / tsSum$lvstReqME_2014
tsSum$adeqMin_2015 <- tsSum$feedMEmin_2015 / tsSum$lvstReqME_2015
tsSum$adeqMin_2016 <- tsSum$feedMEmin_2016 / tsSum$lvstReqME_2016
tsSum$adeqMin_2017 <- tsSum$feedMEmin_2017 / tsSum$lvstReqME_2017
tsSum$adeqMin_2018 <- tsSum$feedMEmin_2018 / tsSum$lvstReqME_2018
tsSum$adeqMin_2019 <- tsSum$feedMEmin_2019 / tsSum$lvstReqME_2019

tsSum_adeq <- read.csv('Results/totals_timeseries_region.csv')
tsSum_adeq <- bind_cols(tsSum_adeq, select(tsSum, adeqMin_2014, adeqMin_2015, adeqMin_2016, adeqMin_2017, adeqMin_2018, adeqMin_2019))

write.csv(tsSum_adeq, 'Results/totals_timeseries_region.csv')


###Calculate admin zone level 1 totals for comparison
aoi1 <-  st_read('SpatialData/inputs/aoi1.gpkg')
out <- read.csv('Results/totals_compare_2019.csv', stringsAsFactors = F)

aoi1 <- bind_cols(dplyr::select(aoi1, NAME_1), lvst = exact_extract(tLv$layer.6, aoi1, fun = "sum"))
aoi1 <- bind_cols(aoi1, feed = exact_extract(tFeed$Feed_total_min_MJ2019, aoi1, fun = "sum"))

out$FeedAdeq_ME_common_min <- aoi1$feed / aoi1$lvst

write.csv(out, 'Results/totals_compare_2019.csv')

rm(list = ls())

######## 
#Duplicate for max values
zones <- st_read('SpatialData/intermediate/zones.gpkg')

###Livestock requirements
cattleIntake_model_MJ_2015 <- raster('SpatialData/outputs/cattleMER_MJ_2015_min.tif') #Min feed requirement will give max adequacy
shoatsIntake_model_MJ_2015 <- raster('SpatialData/outputs/shoatsMER_MJ_2015_min.tif')
horseDonkeyCamelIntake_model_MJ_2015 <- raster('SpatialData/outputs/horseDonkeyCamelMER_MJ_2015.tif')

FAOlvstPop <- read.csv('LivestockParams/FAOSTAT_livestock_data.csv')

lv2014 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2015 <- sum(horseDonkeyCamelIntake_model_MJ_2015, cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)
lv2016 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2017 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2018 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2019 <- sum(horseDonkeyCamelIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Shoats"])), na.rm = T)

tLv <- stack(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019)
rm(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019, shoatsIntake_model_MJ_2015, cattleIntake_model_MJ_2015, horseDonkeyCamelIntake_model_MJ_2015)


## Adequacy outputs

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
tFeed <- stack(list.files(path = 'SpatialData/outputs',pattern="Feed_total_max_MJ",full.names = T))
zones <- bind_cols(select(zones, grouping), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

tsSum <- zones
st_geometry(tsSum) <- NULL

tsSum <- data.frame(tsSum)

colnames(tsSum) <- c("NAME_1", "lvstReqMEmax_2014", "lvstReqMEmax_2015", "lvstReqMEmax_2016", "lvstReqMEmax_2017", "lvstReqMEmax_2018", "lvstReqMEmax_2019", "feedME_max_2014", "feedME_max_2015", "feedME_max_2016", "feedME_max_2017", "feedME_max_2018", "feedME_max_2019")

tsSum$adeqMax_2014 <- tsSum$feedME_max_2014 / tsSum$lvstReqMEmax_2014
tsSum$adeqMax_2015 <- tsSum$feedME_max_2015 / tsSum$lvstReqMEmax_2015
tsSum$adeqMax_2016 <- tsSum$feedME_max_2016 / tsSum$lvstReqMEmax_2016
tsSum$adeqMax_2017 <- tsSum$feedME_max_2017 / tsSum$lvstReqMEmax_2017
tsSum$adeqMax_2018 <- tsSum$feedME_max_2018 / tsSum$lvstReqMEmax_2018
tsSum$adeqMax_2019 <- tsSum$feedME_max_2019 / tsSum$lvstReqMEmax_2019

tsSum_adeq <- read.csv('Results/totals_timeseries_region.csv')
tsSum_adeq <- bind_cols(tsSum_adeq, select(tsSum, adeqMax_2014, adeqMax_2015, adeqMax_2016, adeqMax_2017, adeqMax_2018, adeqMax_2019))
write.csv(tsSum_adeq, 'Results/totals_timeseries_region.csv')


###Calculate admin zone level 1 totals for comparison
aoi1 <-  st_read('SpatialData/inputs/aoi1.gpkg')
out <- read.csv('Results/totals_compare_2019.csv', stringsAsFactors = F)

aoi1 <- bind_cols(dplyr::select(aoi1, NAME_1), lvst = exact_extract(tLv$layer.6, aoi1, fun = "sum"))
aoi1 <- bind_cols(aoi1, feed = exact_extract(tFeed$Feed_total_max_MJ2019, aoi1, fun = "sum"))

out$FeedAdeq_ME_common_max <- aoi1$feed / aoi1$lvst

write.csv(out, 'Results/totals_compare_2019.csv')
