library(terra)

root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
phenPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/PhenologyModis")

dir.create(paste0(phenPath,"/outputTif/"), F, T)

filenamesTifInter <- list.files(path = paste0(phenPath, "/intermediate/"), pattern="^pheno.*\\.tif$",full.names = T)

#interpolate, resample and crop all rasters
width = 19

dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

phenoFiles <- gsub('.{4}$', '', basename(filenamesTifInter))

lapply(phenoFiles, function(phenoFile){
  iphenoFile <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/PhenologyModis/intermediate/", phenoFile, ".tif"))
  iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
  iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
  iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
  
  iphenoFile <- crop(iphenoFile, ext(iphenoFile))
  iphenoFile <- resample(iphenoFile, dmpTemp, method="near")
  iphenoFile <- mask(iphenoFile, mask = dmpTemp)
  iphenoFile <- app(iphenoFile, fun = function(x) as.integer(round(x)))
  
  names(iphenoFile) <- phenoFile; varnames(iphenoFile) <-phenoFile
  
  writeRaster(iphenoFile, paste0(phenPath, "/outputTif/", phenoFile, ".tif"), overwrite = T)
  rm(iphenoFile)
  gc()
})