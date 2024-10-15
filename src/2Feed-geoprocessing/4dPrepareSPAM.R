# library(raster)
# #library(sp)
# #library(sf)
# library(gdalUtils)
library(terra)

#rasterOptions(maxmemory = 1e+60)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

spamPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020")

# read AOI
aoi <- vect(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP_2023/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

sPamfiles <- list.files(path = spamPath, pattern="*frac.tif$",full.names = T)
#filenamesTifInter2 <- list.files(path = spamPath ,pattern="*frac.tif$",full.names = F)

lapply(sPamfiles, function(sPamfile){
  sPamfile_name <- tolower(gsub('.{4}$', '', basename(sPamfile)))
  
  isPamFile <- rast(sPamfile)
  isPamFile <- crop(isPamFile, ext(dmpTemp))
  
  names(isPamFile) <- sPamfile_name
  varnames(isPamFile) <- sPamfile_name
  
  # save as GeoTIFF
  terra::writeRaster(isPamFile, filename = paste0(spamPath, "/", sPamfile_name, ".tif"), overwrite=TRUE)
  
  rm(isPamFile)
  gc()
})
