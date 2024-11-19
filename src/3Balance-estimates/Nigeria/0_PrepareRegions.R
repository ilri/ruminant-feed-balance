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

root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

indir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs")
outdirInt <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate"); dir.create(outdirInt, F, T)

ECOZONE <- st_read(paste0(indir, "/Ecological_and_Feed_Distribution.shp"))

sf_use_s2(FALSE)

ECOZONE <- ECOZONE %>%
  mutate(Ecological = case_when(Ecological == "Fresh Water Swanmp Forest" ~ "Fresh Water Swamp Forest",
                                TRUE ~ Ecological),
         ECOZONECode = case_when(Ecological == "Sudan Savannah" ~ "SUSSAV",
                                 Ecological == "Sahel Savannah" ~ "SASSAV",
                                 Ecological == "Southern Guinea Savannah" ~ "SOGSAV",
                                 Ecological == "Lowland Rainfall" ~ "LORAIN",
                                 Ecological == "Fresh Water Swamp Forest" ~ "FRWASF",
                                 Ecological == "Northern Guinea Savannah" ~ "NOGSAV",
                                 Ecological == "Mountain Vegetations" ~ "MOUVEG",
                                 Ecological == "Mangrove" ~ "MANGGR",
                                 TRUE ~ Ecological),
         ECORegion = case_when(Ecological == "Sahel Savannah" ~ "Sahel",
                               Ecological %in% c("Sudan Savannah", "Southern Guinea Savannah", "Northern Guinea Savannah", "Mountain Vegetations")  ~ "Savannah",
                               Ecological %in% c("Lowland Rainfall", "Fresh Water Swamp Forest", "Mangrove") ~ "Lowlands",
                               TRUE ~ Ecological),
         ECOZone = case_when(Ecological == "Sahel Savannah" ~ "(Agro)pastoral sahel",
                             Ecological %in% c("Sudan Savannah", "Northern Guinea Savannah", "Mountain Vegetations")  ~ "Central mixed",
                             Ecological == "Southern Guinea Savannah" ~ "Southern mixed",
                             Ecological %in% c("Lowland Rainfall", "Fresh Water Swamp Forest", "Mangrove")  ~ "Lowland mixed",
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