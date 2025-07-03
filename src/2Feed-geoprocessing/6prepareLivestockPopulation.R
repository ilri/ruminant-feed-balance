# Prepare livestock population layers
# Layer provided at 10km spatial resolution, units: animals per km2
# Author: John Mutua
# Last modified on 11/11/2024

# # Install required packages
# install.packages("terra")

library(terra)
library(sf)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(stars)

country <- "Nigeria"

# root folder
root <- "."
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4")
outdir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/GLW4"); dir.create(outdir, F, T)
LivestockParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/LivestockParams")

aoi0 <- st_read(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))
aoi1 <- st_read(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi1.shp"))
aoi2 <- st_read(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi2.shp"))

FAOLivestock <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/Nigeria/LivestockParams/FAOSTAT_data_en_4-8-2025.csv"))

animalCategories <- c("CTL", "GTS", "PGS", "SHP", "HRS")

for(animalCategory in animalCategories){
  
  animalFile <- terra::rast(paste0(indir, "/GLW4-2020.D-DA.", animalCategory, ".tif"))
  animalFile <- terra::crop(animalFile, aoi0)
  
  if(animalCategory != "HRS"){ # Horse numbers are from 2025, already in animal per pixel
    animalFile_area <- terra::cellSize(animalFile, unit = "km") #calculate area per pixel
    animalFile <- animalFile*animalFile_area #total number of cattle per pixel taking into account the area per pixel
  }

  # masked_animalFile <- mask(animalFile, aoi2)
  # sum_values <- global(masked_animalFile, fun = "sum", na.rm = TRUE)
  # sum_values
  
  writeRaster(animalFile, paste0(outdir, "/", names(animalFile), ".tif"), overwrite=TRUE)

}

# # plotting
# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# cattlePop <- rast(paste0(outdir, "/GLW4-2020.D-DA.CTL.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) +
#   geom_spatraster(data = cattlePop) +
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) +
#   scale_fill_gradient(limits = c(0, 250), breaks = c(0, 50, 100, 250), low = "#FFFFFF", high = "brown", na.value = NA, name="Cattle (Head/sq km)")

# Align with latest figures
# Add 2023 data from ministry
FAOLivestock <- FAOLivestock %>%
  mutate(Value = if_else(Item == "Cattle" & Year == 2023, 58490000,
                         if_else(Item == "Goats" & Year == 2023, 124070000,
                                 if_else(Item == "Sheep" & Year == 2023, 63970000, Value))))

FAOLivestockMASTER <- FAOLivestock %>%
  #mutate(Item = case_when(Item == "Cattle" ~ "Cattle", Item == "Sheep" ~ "Shoats", Item == "Goats" ~ "Shoats", TRUE ~ NA)) %>%
  group_by(Item, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Item, Year) %>%
  group_by(Item) %>%
  mutate(PcChange = (Value - lag(Value))/lag(Value)) %>%
  ungroup()

write_csv(FAOLivestockMASTER, paste0(root, "/src/1Data-download/Tables/inputs/Nigeria/LivestockParams/FAOSTAT_livestock_data-MASTER.csv"), append = FALSE)

FAOLivestock <- FAOLivestock %>%
  mutate(Item = case_when(Item == "Cattle" ~ "Cattle", Item == "Sheep" ~ "Shoats", Item == "Goats" ~ "Shoats", TRUE ~ NA)) %>%
  group_by(Item, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Item, Year) %>%
  group_by(Item) %>%
  mutate(PcChange = (Value - lag(Value))/lag(Value)) %>%
  ungroup()

write_csv(FAOLivestock, paste0(root, "/src/1Data-download/Tables/inputs/Nigeria/LivestockParams/FAOSTAT_livestock_data.csv"), append = FALSE)

# Loop through years
yearList <- c("2021", "2022", "2023")
lvCategories <- c("CTL", "GTS", "SHP")
for(year in yearList){

  for(lvCategory in lvCategories){

    if(lvCategory == "CTL"){Species <- "Cattle"}else if(lvCategory == "GTS"){Species <- "Goats"}else if(lvCategory == "SHP"){Species <- "Sheep"}

    ###Livestock requirements
    lvCount <- rast(paste0(outdir, "/GLW4-2020.D-DA.",lvCategory,".tif"))

    FAOlvstPop <- read.csv("/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance/src/1Data-download/Tables/inputs/Nigeria/LivestockParams/FAOSTAT_livestock_data-MASTER.csv")

    lvCountNew <- lvCount + (lvCount*FAOlvstPop$PcChange[FAOlvstPop$Year == year & FAOlvstPop$Item == Species])
    lvCountNew <- lvCountNew %>% crop(., aoi0) %>% mask(., mask=aoi0)

    writeRaster(lvCountNew, paste0(outdir, "/GLW4-",year,".D-DA.", lvCategory, ".tif"), overwrite=TRUE)

  }

}
