# Check LU area over regions
# Author: John Mutua
# Last modified on 18/11/2024

library(terra)
library(sf)

country <- "Nigeria"

# root folder
root <- "."
indir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/LandUse")

zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))

luClasses <- c("crops", "grass", "shrub", "tree")

tempfile <- rast(paste0(indir, "/LUcrops300.tif"))

luCellSize <- terra::cellSize(tempfile, unit = "ha") #calculate area per pixel

lufiles <- rast(paste0(indir, "/LU", luClasses, "300.tif"))
lufilesArea <- lufiles*luCellSize
zones <- bind_cols(select(zones, ECOZone), exact_extract(lufilesArea, zones, fun = "sum"))
  
st_geometry(zones) <- NULL
luSum <- data.frame(zones)
colnames(luSum) <- c("NAME_1", "crop", "grass", "shrub", "tree")

write.csv(luSum, paste0(root, "/src/3Balance-estimates/Nigeria/QC_Checks/LU_area.csv"), row.names = F)  