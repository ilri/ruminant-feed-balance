# Fill phenology layers
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("exactextractr")
# install.packages("terra")
# install.packages("gdalUtils")

# Load libraries
library(terra)

# study area
country <- "Nigeria"

root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

yearList <- c("2020", "2021", "2022", "2023")

for (year in yearList[2:4]){
  
  phenPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/PhenologyModis/", year)

  dir.create(paste0(phenPath,"/outputTif/"), F, T)
  
  filenamesTifInter <- list.files(path = paste0(phenPath, "/intermediate/"), pattern="^pheno.*\\.tif$",full.names = T)
  
  #interpolate, resample and crop all rasters
  width = 19
  
  dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))
  
  phenoFiles <- gsub('.{4}$', '', basename(filenamesTifInter))
  
  lapply(phenoFiles, function(phenoFile){
    iphenoFile <- rast(paste0(phenPath, "/intermediate/", phenoFile, ".tif"))
    iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
    iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
    iphenoFile <- focal(iphenoFile, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
    gc()
    
    iphenoFile <- crop(iphenoFile, ext(iphenoFile))
    iphenoFile <- resample(iphenoFile, dmpTemp, method="near")
    iphenoFile <- mask(iphenoFile, mask = dmpTemp)
    iphenoFile <- app(iphenoFile, fun = function(x) as.integer(round(x)))
    
    names(iphenoFile) <- phenoFile; varnames(iphenoFile) <-phenoFile
    
    writeRaster(iphenoFile, paste0(phenPath, "/outputTif/", phenoFile, ".tif"), overwrite = T)
    rm(iphenoFile)
    gc()
  })
}

# library(tidyterra)
# library(sf)
# library(ggplot2)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# phenoGreenup1 <- rast(paste0(phenPath, "/outputTif/phenoGreenup1.tif"))
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = phenoGreenup1) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_gradient(low = "#CDDF4A", high = "#0BAE1C", na.value = NA, name="Date")
