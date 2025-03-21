# Prepare livestock systems maps
# Author: John Mutua
# Last modified on 17/11/2024

# # Install required packages
# install.packages("terra")

library(terra)

country <- "Nigeria"

# root folder
root <- "."
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLPS")
outdir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/GLPS"); dir.create(outdir, F, T)

aoi2 <- st_read(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

glps <- rast(paste0(indir, "/glps_gleam_61113_10km/glps_gleam_61113_10km.tif"))
glps <- terra::crop(glps, aoi2)

writeRaster(glps, paste0(outdir, "/glps.tif"), overwrite=TRUE)

# # plotting
# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# cattlePop <- rast(paste0(outdir, "/GLW4-2020.D-DA.CTL.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = cattlePop) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(limits = c(0, 250), breaks = c(0, 50, 100, 250), low = "#FFFFFF", high = "brown", na.value = NA, name="Cattle (Head/sq km)")

