# Extract results at state

library(terra)
#library(stars)
library(sf)
library(exactextractr)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(stars)

# avoid scientific notation
options(scipen = 999)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")

aoi2 <-read_sf(paste0(spatialDir, "/inputs/aoi2.shp"))

# extract DM
feed_list <- c("crop", "grass", "browse")
tFeed <- rast(paste0(spatialDir, "/outputs/Feed_", feed_list, "DM2023.tif"))
feedDM <- exact_extract(tFeed, aoi2, "sum")
feedDM_stats <- cbind(LGA = aoi2$NAME_2, feedDM)
colnames(feedDM_stats)[2:4] <- c("cropDM_kg", "grassDM_kg", "browseDM_kg")
write.csv(feedDM_stats, paste0(Results_dir, "/FeedDM_LGA.csv"), row.names = FALSE)

# extract cropping and non-cropping days
season_list <- c("cropping", "dry")
tSeasons <- rast(paste0(spatialDir, "/outputs/", season_list, "Days_2023.tif"))
seasonLen <- exact_extract(tSeasons, aoi2, "mean")
seasonLen_stats <- cbind(LGA = aoi2$NAME_2, seasonLen)
colnames(seasonLen_stats)[2:3] <- c("croppingDays", "dryDays")
seasonLen_stats <- seasonLen_stats %>% mutate_if(is.numeric, round, digits = 0)
write.csv(seasonLen_stats, paste0(Results_dir, "/SeasonLength.csv"), row.names = FALSE)

# extract livestock population
lvCategories <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Lamb", "Goat", "Kid")
tLv <- rast(paste0(spatialDir, "/inputs/GLW4/Dissag_GLW4_2020_",lvCategories,".tif"))
names(tLv) <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Lamb", "Goat", "Kid")
tSumLv <- exact_extract(tLv, aoi2, "sum")
tSumLv <- cbind(LGA = aoi2$NAME_2, tSumLv)
colnames(tSumLv)[2:10] <- c("Bulls", "Cows", "Steers", "Heifers", "Calves", "Sheep", "Lambs", "Goats", "Kids")
tSumLv <- tSumLv %>% mutate_if(is.numeric, round, digits = 0)
# tSumLv$Cattle <- tSumLv$Bulls+tSumLv$Cows+tSumLv$Steers+tSumLv$Heifers+tSumLv$Calves
# sum(tSumLv$Cattle)
write.csv(tSumLv, paste0(Results_dir, "/LivestockPopulation.csv"), row.names = FALSE)

