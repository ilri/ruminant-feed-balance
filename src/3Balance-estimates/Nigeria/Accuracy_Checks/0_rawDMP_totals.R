gc()
rm(list=ls())
# Check raw DMP total per year, ton/ha
# Author: John Mutua
# Last modified 5/1/2025

# avoid scientific notation
options(scipen = 999)

# Load libraries
library(sf)
library(dplyr)
library(exactextractr)
library(terra)
library(purrr)
library(readr)

terraOptions(tempdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "."

country <- "Nigeria"

spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
pathDMP <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP")

regionsArea <- read_csv(paste0(root, "/src/3Balance-estimates/Nigeria/Results/area_regions.csv")) %>% dplyr::select(ECORegion, area_hectares)

yearList <- c("2020", "2021", "2022", "2023")

DMPSumList <- list()
for(year in yearList){
  
  filesDMP <- list.files(path = pathDMP, pattern = paste0("RT6_", year, ".*\\.tif$"), full.names = TRUE)
  stDMP <- rast(filesDMP)
  
  stDMPTotal <- app(stDMP, fun="mean", na.rm=TRUE)*9*365
  
  zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/regions.gpkg"))
  zones <- bind_cols(dplyr::select(zones, ECORegion), exact_extract(stDMPTotal, zones, fun = "sum"))
  
  st_geometry(zones) <- NULL
  DMPSum <- data.frame(zones)
  
  colnames(DMPSum)[2] <- gsub(" ", "", paste("DM_", year))
  
  DMPSumList[[year]] <- DMPSum
  gc()
  
}

DMPSum_all <- reduce(DMPSumList, full_join, by = "ECORegion") %>% 
  left_join(regionsArea, by = "ECORegion") %>% 
  mutate(tonHa_2020 = DM_2020 / area_hectares/1000,
         tonHa_2021 = DM_2021 / area_hectares/1000,
         tonHa_2022 = DM_2022 / area_hectares/1000,
         tonHa_2023 = DM_2023 / area_hectares/1000)

write.csv(DMPSum_all, paste0(root, "/src/3Balance-estimates/Nigeria/Accuracy_Checks/DMPSum_ECOZone.csv"), row.names=FALSE)

# From feed DM potential layers
pathDMPPotential <- paste0(spatialDir, "/inputs/Feed_quantity")

DMPPotentialSumList <- list()
for(year in yearList){
  
  filesDMPPotential <- list.files(path = pathDMPPotential, pattern=paste0("mean_", year, "\\.tif$"),full.names = T)
  stDMPPotential <- rast(filesDMPPotential)
  
  stDMPPotentialTotal <- app(stDMPPotential, fun="mean", na.rm=TRUE)
  
  zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/regions.gpkg"))
  zones <- bind_cols(dplyr::select(zones, ECORegion), exact_extract(stDMPPotentialTotal, zones, fun = "sum"))
  
  st_geometry(zones) <- NULL
  DMPPotentialSum <- data.frame(zones)
  
  colnames(DMPPotentialSum)[2] <- gsub(" ", "", paste("DM_", year))
  
  DMPPotentialSumList[[year]] <- DMPPotentialSum
  gc()

}

DMPPotentialSum_all <- reduce(DMPPotentialSumList, full_join, by = "ECORegion") %>% 
  left_join(regionsArea, by = "ECORegion") %>% 
  mutate(tonHa_2020 = DM_2020 / area_hectares/1000,
         tonHa_2021 = DM_2021 / area_hectares/1000,
         tonHa_2022 = DM_2022 / area_hectares/1000,
         tonHa_2023 = DM_2023 / area_hectares/1000)

write.csv(DMPPotentialSum_all, paste0(root, "/src/3Balance-estimates/Nigeria/Accuracy_Checks/DMPPotentialSum_ECOZone.csv"), row.names=FALSE)
