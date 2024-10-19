library(sf)
library(dplyr)

root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

indir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs")
outdirInt <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate"); dir.create(outdirInt, F, T)

LZ <- st_read(paste0(indir, "/Ecological_and_Feed_Distribution.shp"))

LZ <- LZ %>% 
  mutate(Ecological = ifelse(Ecological == "Fresh Water Swanmp Forest", "Fresh Water Swamp Forest", Ecological),
         grouping = Ecological)

# ggplot() +
#   geom_sf(data = LZ, aes(fill = LZCODE)) +
#   coord_sf(xlim = c(2.1, 15.1), ylim = c(3.8, 14.3), expand = FALSE) +
#   labs(fill = "Region")

sf_use_s2(FALSE)

##Export
st_write(LZ[, c("grouping", "geometry")], paste0(outdirInt, "/zones.gpkg"), append = F)
st_rasterize(sf = LZ[, "grouping"], file = paste0(outdirInt, "/zones.tif"))