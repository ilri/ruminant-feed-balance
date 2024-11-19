# Prepare crop fraction layers
# Author: Simon Fraval
# Last modified by John Mutua on 12/11/2024

# # Install required packages
# install.packages("terra")

# Load libraries
library(terra)

terraOptions(tempdir = "/home/s2255815/scratch/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# country
country <- "Nigeria"

spamPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020")
pathSPAMInter <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020/intermediate")

# read AOI
aoi <- vect(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

sPamfiles <- list.files(path = pathSPAMInter, pattern="*frac.tif$",full.names = T)
#filenamesTifInter2 <- list.files(path = spamPath ,pattern="*frac.tif$",full.names = F)

lapply(sPamfiles, function(sPamfile){
  sPamfile_name <- tolower(gsub('.{4}$', '', basename(sPamfile)))
  
  isPamFile <- rast(sPamfile)
  isPamFile <- crop(isPamFile, ext(dmpTemp))
  isPamFile <- mask(isPamFile, mask = dmpTemp)
  
  names(isPamFile) <- sPamfile_name
  varnames(isPamFile) <- sPamfile_name
  
  # save as GeoTIFF
  terra::writeRaster(isPamFile, filename = paste0(spamPath, "/", sPamfile_name, ".tif"), overwrite=TRUE)
  
  rm(isPamFile)
  gc()
})
