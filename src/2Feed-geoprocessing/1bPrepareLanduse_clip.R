# Prepare land use layers
# Author: John Mutua
# Last modified on 11/11/2024

# # Install required packages
# install.packages("terra")

# Load required packages
library(terra)

# study area
country <- "Nigeria"

# root folder
root <- "."
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/LandUse")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/LandUse"); dir.create(outdir, F, T)

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

luClasses <- c("Crops", "Grass", "Shrub", "Tree")

for(luClass in luClasses){
  
  iluFile <- rast(paste0(indir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", luClass,"-CoverFraction-layer_EPSG-4326.tif"))
  iluFile <- crop(iluFile, ext(iluFile))
  iluFile <- resample(iluFile, dmpTemp, method="near")
  iluFile <- mask(iluFile, mask = dmpTemp)
  iluFile <- iluFile/100 # make it 0-1
    
  names(iluFile) <- paste0("LU", tolower(luClass), "300")
    
  writeRaster(iluFile, paste0(outdir, "/LU", tolower(luClass), "300.tif"), overwrite=TRUE)
    
  rm(iluFile)
  gc()
}

# # plotting
# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# LUcrops300 <- rast(paste0(outdir, "/LUcrops300.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = LUcrops300) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="Crop cover (%)")
