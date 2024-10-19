#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
#library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

zones <- st_read('SpatialData/intermediate/zones.gpkg')


###Livestock requirements
cattleIntake_model_MJ_2015 <- raster('SpatialData/outputs/cattleMER_MJ_2015.tif')
shoatsIntake_model_MJ_2015 <- raster('SpatialData/outputs/shoatsMER_MJ_2015.tif')
horseDonkeyIntake_model_MJ_2015 <- raster('SpatialData/outputs/horseDonkeyMER_MJ_2015.tif')

FAOlvstPop <- read.csv('LivestockParams/FAOSTAT_livestock_data.csv')

lv2014 <- sum(horseDonkeyIntake_model_MJ_2015, (cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Cattle"])), (shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2014 & FAOlvstPop$Item == "Shoats"])), na.rm = T)
lv2015 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)
lv2016 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2016 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2017 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2017 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2018 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2018 & FAOlvstPop$Item == "Shoats"]), na.rm = T)
lv2019 <- sum(horseDonkeyIntake_model_MJ_2015, cattleIntake_model_MJ_2015 + (cattleIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Cattle"]), shoatsIntake_model_MJ_2015 + (shoatsIntake_model_MJ_2015*FAOlvstPop$PcChange[FAOlvstPop$Year == 2019 & FAOlvstPop$Item == "Shoats"]), na.rm = T)

tLv <- stack(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019)
rm(lv2014, lv2015, lv2016, lv2017, lv2018, lv2019, shoatsIntake_model_MJ_2015, cattleIntake_model_MJ_2015)


## Adequacy outputs
tFeed <- stack(list.files(path = 'SpatialData/outputs',pattern="Feed_total_mean_MJ",full.names = T))

#totalDM_2019 <- (tCrop$Feed_crop_burn_MJ.6*croppingDays) + (tGrassWet$layer.6*croppingDays) + (tGrassDry$layer.6*dryDays) + (tBrowse$DMPbrowsemean_2019 * 365)
zones <- bind_cols(select(zones, grouping), exact_extract(tLv, zones, fun = "sum"))
zones <- bind_cols(zones, exact_extract(tFeed, zones, fun = "sum"))

st_geometry(zones) <- NULL

tsSum <- data.frame(zones)

colnames(tsSum) <- c("NAME_1", "lvstReqME_2014", "lvstReqME_2015", "lvstReqME_2016", "lvstReqME_2017", "lvstReqME_2018", "lvstReqME_2019", "feedME_mean_2014", "feedME_mean_2015", "feedME_mean_2016", "feedME_mean_2017", "feedME_mean_2018", "feedME_mean_2019")

tsSum$adeq_2014 <- tsSum$feedME_mean_2014 / tsSum$lvstReqME_2014
tsSum$adeq_2015 <- tsSum$feedME_mean_2015 / tsSum$lvstReqME_2015
tsSum$adeq_2016 <- tsSum$feedME_mean_2016 / tsSum$lvstReqME_2016
tsSum$adeq_2017 <- tsSum$feedME_mean_2017 / tsSum$lvstReqME_2017
tsSum$adeq_2018 <- tsSum$feedME_mean_2018 / tsSum$lvstReqME_2018
tsSum$adeq_2019 <- tsSum$feedME_mean_2019 / tsSum$lvstReqME_2019

write.csv(tsSum, 'Results/totals_timeseries_region.csv')


###Calculate admin zone level 1 totals for comparison
aoi1 <-  st_read('SpatialData/inputs/gadm40_BFA_1.shp')

aoi1 <- bind_cols(dplyr::select(aoi1, NAME_1), lvst = exact_extract(tLv$layer.6, aoi1, fun = "sum"))
aoi1 <- bind_cols(aoi1, feed = exact_extract(tFeed$Feed_total_mean_MJ2019, aoi1, fun = "sum"))

aoi1$FeedAdeq_ME_common <- aoi1$feed / aoi1$lvst

Rahimi_feed_DM <- raster('AltAnalyses/Rahimi Supply_tDM/avg_supply/w001001.adf') *100*100 # convert from T DM ha-1 to T DM per grid square
aoi1$Rahimi_supply <- exact_extract(Rahimi_feed_DM, aoi1, 'sum')
aoi1$FeedAdeq_ME_Rahimi <- (aoi1$Rahimi_supply*1000*8) / aoi1$lvst


st_geometry(aoi1) <- NULL
aoi1 <- data.frame(aoi1)

write.csv(aoi1, 'Results/totals_compare_2019.csv')
