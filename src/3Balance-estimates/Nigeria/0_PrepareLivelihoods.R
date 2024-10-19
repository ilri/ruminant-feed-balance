library(stars)
library(dplyr)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
outdirInt <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate"); dir.create(outdirInt, F, T)

country <- "Nigeria"

livelihood_zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/Ecological_and_Feed_Distribution.shp"))

livelihood_zones <- livelihood_zones %>% 
  mutate(Ecological = ifelse(Ecological == "Fresh Water Swanmp Forest", "Fresh Water Swamp Forest", Ecological),
         LZCODE = Ecological)

liveOut <- st_rasterize(sf = livelihood_zones[, "LZCODE"], file = paste0(outdirInt, "/livelihoodZones.tif"))