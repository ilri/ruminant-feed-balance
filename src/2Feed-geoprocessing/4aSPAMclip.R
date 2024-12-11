# Clip SPAM layers
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("terra")

# Load libraries
library(terra)

terraOptions(tempdir = "/home/s2255815/scratch/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "."

# country
country <- "Nigeria"

# output folder
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020"); dir.create(outdir, F, T)

# read AOI
aoi <- vect(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

sPamfiles <- list.files(paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/CropType/spam2020V0r1_global_physical_area"), pattern = "_A.tif$", full.names = TRUE, recursive = TRUE)

lapply(sPamfiles, function(sPamfile){
  sPamfile_name <- tolower(gsub('.{4}$', '', basename(sPamfile)))
  
  isPamFile <- rast(sPamfile)
  isPamFile <- crop(isPamFile, ext(dmpTemp))
  isPamFile <- resample(isPamFile, dmpTemp, method="bilinear")
  isPamFile <- mask(isPamFile, mask = dmpTemp)
  
  isPamFile[is.nan(values(isPamFile))] <- NA
  
  names(isPamFile) <- sPamfile_name
  varnames(isPamFile) <- sPamfile_name
  
  # save as GeoTIFF
  terra::writeRaster(isPamFile, filename = paste0(outdir, "/", sPamfile_name, ".tif"), overwrite=TRUE)
  
  cat("Clipped: ", sPamfile_name, "\n")
  
  rm(isPamFile)
  gc()
})


# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# spamMaize <- rast(paste0(outdir, "/spam2020_v0r1_global_a_maiz_a.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = isPamFile) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="Maize area (Ha)")
