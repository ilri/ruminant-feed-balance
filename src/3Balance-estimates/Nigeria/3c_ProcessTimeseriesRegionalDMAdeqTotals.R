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
library(tidyr)
library(dplyr)
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

# Produce DM requirments
# Energy intake in MJ
aoi0 <- st_read(paste0(spatialDir, "/inputs/aoi0.shp"))

lvMEReqTotal2023 <- rast(paste0(Outputs_dir, "/lvReqTotal2023.tif")) %>% 
  crop(., aoi0) %>% 
  mask(., mask=aoi0)

#global(lvReqTotal2023, fun = "sum", na.rm = TRUE)

# Feed composition (%)
proportions <- c(crop_residue = 32.6, grass = 38.2, browse = 9.5, other_biomass = 19.6)

# ME values (MJ per kg DM)
me_values <- c(crop_residue = 6.5, grass = 6.83, browse = 7.97, other_biomass = 6.5)

# Weighted average ME
weighted_ME <- sum(proportions * me_values) / 100

# Calculate total dry matter intake in kg
lvDMReqTotal2023 <- lvMEReqTotal2023 / weighted_ME

#global(lvDMReqTotal2023, fun = "sum", na.rm = TRUE)

# Bring in DM data
feed_list <- c("crop", "grass", "browse", "after")
tFeed <- rast(paste0(spatialDir, "/outputs/Feed_", feed_list, "DM2023.tif"))
tFeedTotal2023 <- sum(tFeed, na.rm=TRUE)

#global(tFeedTotal2023, fun = "sum", na.rm = TRUE)

# Extract data by zones
aggregation_zones <- c("country", "region", "state", "zone")
for (aggregation_zone in aggregation_zones) {
  
  if (aggregation_zone == "country") {
    zones <- st_read(paste0(spatialDir, "/inputs/aoi0.shp"))
    name_col <- "COUNTRY"
  } else if (aggregation_zone == "region") {
    zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/regions.gpkg"))
    name_col <- "ECORegion"
  } else if (aggregation_zone == "zone") {
    zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))
    name_col <- "ECOZone"
  } else if (aggregation_zone == "state") {
    zones <- st_read(paste0(spatialDir, "/inputs/aoi1.shp"))
    name_col <- "NAME_1"
  }
  
  DMReq <- exact_extract(lvDMReqTotal2023, zones, "sum")
  DMStats <- cbind(NAME = zones[[name_col]], DMReq) %>% as.data.frame()
  
  DMStats$DMReq <- as.numeric(DMStats$DMReq)
  DMSupply <- exact_extract(tFeedTotal2023, zones, "sum")
  DMStats$DMSupply <- DMSupply
  DMStats$adeq_2023 <- DMStats$DMSupply / DMStats$DMReq
  
  write.csv(DMStats, paste0(Results_dir, "/", aggregation_zone, "DMStats.csv"), row.names = FALSE)
  
  cat("Completed extracting stats for:", aggregation_zone, "\n")
}


