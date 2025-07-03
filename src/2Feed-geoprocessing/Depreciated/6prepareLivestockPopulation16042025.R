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

nbsData <- readxl::read_xlsx(paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4/NBS_data.xlsx"), sheet = "Sheet1") %>% 
  dplyr::select(NAME_1, CTL, GTS, SHP) %>% 
  mutate(nbs_TLU = CTL*1+GTS*0.08+SHP*0.1) %>% 
  mutate_if(is.numeric, round, digits = 0)

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

colnames(nbsData)[2:4] <- paste0("nbs_", colnames(nbsData)[2:4])

# Now make new layers based on NBS data
lvCategories <- c("CTL", "GTS", "SHP")
for(lvCategory in lvCategories){
  
  ###Livestock requirements
  lvCount <- rast(paste0(outdir, "/GLW4-2020.D-DA.",lvCategory,".tif"))
  
  nbs_lvPop <- nbsData %>% 
    pivot_longer(nbs_CTL:nbs_TLU, names_to = "lvVariable", values_to = "lvValue") %>% 
    filter(lvVariable == paste0("nbs_", lvCategory)) %>% 
    dplyr::select(NAME_1, lvValue)
  
  colnames(nbs_lvPop)[2] <- paste0("nbs_", lvCategory)
  
  lvPop <- read_csv(paste0(root, "/src/3Balance-estimates/Nigeria/Results/", lvCategory, "_2020Population.csv")) %>% 
    rename(NAME_1=STATE) %>% 
    left_join(nbs_lvPop, by="NAME_1") %>% 
    mutate(PcChange = (.[[3]]-.[[2]])/.[[2]])
  
  aoi1lvPop <- aoi1 %>% left_join(lvPop, by="NAME_1")
  
  aoi1lvPopOut <- st_rasterize(sf = aoi1lvPop[, "PcChange"]) %>% 
    rast() %>% 
    crop(., ext(lvCount)) %>% 
    resample(., lvCount) %>% 
    mask(., mask = lvCount)
  
  if(lvCategory=="CTL"){
    writeRaster(aoi1lvPopOut, paste0(outdir, "/pcChange2020_2023Cattle.tif"), overwrite=TRUE)
  }
    
  lvCountNew <- lvCount + lvCount*aoi1lvPopOut
  
  writeRaster(lvCountNew, paste0(outdir, "/GLW4-2023.D-DA.", lvCategory, ".tif"), overwrite=TRUE)
  
}

# calculate pop change for shoats
nbsShoatsPop <- nbsData %>% 
  pivot_longer(nbs_CTL:nbs_TLU, names_to = "nbsVariable", values_to = "nbsValue") %>% 
  filter(nbsVariable %in% c("nbs_GTS", "nbs_SHP")) %>% 
  group_by(NAME_1) %>% 
  summarise(nbsValue = sum(nbsValue, na.rm = TRUE))

glwShoatsPop <- read_csv(paste0(root, "/src/3Balance-estimates/Nigeria/Results/GTS_2020Population.csv")) %>% rename(NAME_1=STATE) %>% 
  left_join(read_csv(paste0(root, "/src/3Balance-estimates/Nigeria/Results/SHP_2020Population.csv")) %>% rename(NAME_1=STATE), by="NAME_1") %>% 
  pivot_longer(GTS:SHP, names_to = "glwVariable", values_to = "glwValue") %>% 
  group_by(NAME_1) %>% 
  summarise(glwValue = sum(glwValue, na.rm = TRUE))

ShoatsPop <- nbsShoatsPop %>% 
  left_join(glwShoatsPop, by="NAME_1") %>% 
  mutate(PcChange = (.[[3]]-.[[2]])/.[[2]])

aoi1ShoatsPop <- aoi1 %>% left_join(ShoatsPop, by="NAME_1")

aoi1ShoatsPopOut <- st_rasterize(sf = aoi1ShoatsPop[, "PcChange"]) %>% 
  rast() %>% 
  crop(., ext(lvCount)) %>% 
  resample(., lvCount) %>% 
  mask(., mask = lvCount)


writeRaster(aoi1ShoatsPopOut, paste0(outdir, "/pcChange2020_2023Shoats.tif"), overwrite=TRUE)


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
