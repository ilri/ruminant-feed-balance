# Prepare livestock population layers
# Author: John Mutua
# Last modified on 11/11/2024

# # Install required packages
# install.packages("terra")

library(terra)

country <- "Nigeria"

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4")
outdir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/GLW4"); dir.create(outdir, F, T)

# Fix extent
dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

animalCategories <- c("CTL", "GTS", "PGS", "SHP", "HRS")

for(animalCategory in animalCategories){
  
  animalFile <- rast(paste0(indir, "/GLW4-2020.D-DA.", animalCategory, ".tif"))
  animalFile <- crop(animalFile, ext(dmpTemp))
  animalFile <- resample(animalFile, dmpTemp, method="near")
  animalFile <- mask(animalFile, mask = dmpTemp)
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
