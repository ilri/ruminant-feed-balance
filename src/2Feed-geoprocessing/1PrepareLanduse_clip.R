library(terra)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/LandUse")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse"); dir.create(outdir, F, T)

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

luClasses <- c("Crops", "Grass", "Shrub", "Tree")

for(luClass in luClasses){
  
  if (!file.exists(paste0(outdir, "/LU", tolower(luClass), "300.tif"))){
    
    iluFile <- rast(paste0(indir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", luClass,"-CoverFraction-layer_EPSG-4326.tif"))
    iluFile <- crop(iluFile, ext(iluFile))
    iluFile <- resample(iluFile, dmpTemp, method="near")
    iluFile <- mask(iluFile, mask = dmpTemp)
    
    names(iluFile) <- paste0("LU", tolower(luClass), "300")
    
    writeRaster(iluFile, paste0(outdir, "/LU", tolower(luClass), "300.tif"), overwrite=TRUE)
    
    rm(iluFile)
    gc()
    
  }else{
    cat("Already cropped: ", tolower(luClass), "\n")
  }
}
