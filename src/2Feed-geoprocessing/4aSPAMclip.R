library(terra)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# output folder
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020"); dir.create(outdir, F, T)

# read AOI
aoi <- vect(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

sPamfiles <- list.files(paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/CropType/spam2020V0r1_global_physical_area"), pattern = ".tif$", full.names = TRUE, recursive = TRUE)

lapply(sPamfiles, function(sPamfile){
  sPamfile_name <- tolower(gsub('.{4}$', '', basename(sPamfile)))
  
  isPamFile <- rast(sPamfile)
  isPamFile <- crop(isPamFile, ext(dmpTemp))
  isPamFile <- resample(isPamFile, dmpTemp, method="bilinear")
  isPamFile <- mask(isPamFile, mask = dmpTemp)
  
  names(isPamFile) <- sPamfile_name
  varnames(isPamFile) <- sPamfile_name
  
  # save as GeoTIFF
  terra::writeRaster(isPamFile, filename = paste0(outdir, "/", sPamfile_name, ".tif"), overwrite=TRUE)
  
  rm(isPamFile)
  gc()
})
