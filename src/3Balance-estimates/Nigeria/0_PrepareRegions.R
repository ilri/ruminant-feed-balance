# Prepare agro-ecological regions
# Author: Simon Fraval
# Last modified by John Mutua on 12/11/2024

# # Install required packages
# install.packages("stars")
# install.packages("dplyr")
# install.packages("ggplot2")

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(stars)

root <- "."

country <- "Nigeria"

indir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs")
outdirInt <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate"); dir.create(outdirInt, F, T)
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results"); dir.create(Results_dir, F, T)

ECOZONE <- st_read(paste0(indir, "/Ecological_and_Feed_Distribution.shp"))

sf_use_s2(FALSE)

ECOZONE <- ECOZONE %>%
  mutate(ECORegion = case_when(Ecological %in% c("Sahel Savannah", "Sudan Savannah") ~ "Dry Savannah",
                               Ecological %in% c("Southern Guinea Savannah", "Northern Guinea Savannah", "Mountain Vegetations")  ~ "Wet Savannah",
                               Ecological %in% c("Lowland Rainfall", "Fresh Water Swamp Forest", "Mangrove") ~ "Forest",
                               TRUE ~ Ecological),
         ECOZone = case_when(Ecological =="Sahel Savannah" ~ "(Agro)pastoral sahel",
                             Ecological == "Sudan Savannah" ~ "Northern mixed",
                             Ecological %in% c("Northern Guinea Savannah", "Mountain Vegetations")  ~ "Central mixed",
                             Ecological == "Southern Guinea Savannah" ~ "Southern mixed",
                             Ecological %in% c("Lowland Rainfall", "Fresh Water Swamp Forest", "Mangrove")  ~ "Forest mixed",
                             TRUE ~ Ecological)
  )

aoi_ECORegion <- ECOZONE %>%  group_by(ECORegion) %>% summarise()

st_write(aoi_ECORegion, paste0(outdirInt, "/regions.gpkg"), append = F)
st_rasterize(sf = aoi_ECORegion[, "ECORegion"], file = paste0(outdirInt, "/regions.tif"))

# ggplot() +
#   geom_sf(data = aoi_ECORegion, aes(fill = ECORegion)) +
#   coord_sf(xlim = c(2.1, 15.1), ylim = c(3.8, 14.3), expand = FALSE) +
#   labs(fill = "Ecological region")

aoi_ECOZone <- ECOZONE %>%  group_by(ECOZone) %>% summarise()

# ggplot() +
#   geom_sf(data = aoi_ECOZone, aes(fill = ECOZone)) +
#   coord_sf(xlim = c(2.1, 15.1), ylim = c(3.8, 14.3), expand = FALSE) +
#   labs(fill = "Ecological zone")

st_write(aoi_ECOZone, paste0(outdirInt, "/zones.gpkg"), append = F)

# Add area of regions and zones
zoneUTM <- aoi_ECOZone %>% st_transform(., crs = 3857)
zoneUTM <- zoneUTM %>% mutate(area_meters = as.numeric(st_area(.)),
                              area_acres = area_meters * 0.0002471054, # 1 m2 = 0.0002471054 acres 
                              area_hectares = area_meters * 0.0001) %>%  # 1m2 = 0.0001 hectares
  st_drop_geometry()
write.csv(zoneUTM, paste0(Results_dir, "/area_zones.csv"), row.names=FALSE)

# Add area of regions and zones
regionsUTM <- aoi_ECORegion %>% st_transform(., crs = 3857)
regionsUTM <- regionsUTM %>% mutate(area_meters = as.numeric(st_area(.)),
                              area_acres = area_meters * 0.0002471054, # 1 m2 = 0.0002471054 acres 
                              area_hectares = area_meters * 0.0001) %>%  # 1m2 = 0.0001 hectares
  st_drop_geometry()
write.csv(regionsUTM, paste0(Results_dir, "/area_regions.csv"), row.names=FALSE)

