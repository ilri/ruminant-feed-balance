library(dplyr)
library(raster)

library(rgdal)


args <- commandArgs(TRUE)

EDDIE_TMP <- as.character(args[1])
#print(EDDIE_TMP)
#print(args)

#Runs with 16gb ram and 40+gb hdd space
rasterOptions(tmpdir = EDDIE_TMP)
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)
##memory.limit(size = 56000) #Windows specific


setwd("")

aoi <- readOGR("SpatialData/inputs/aoi1.shp")

dmpPath <- 'SpatialData/inputs/Feed_DrySeason/DMP/'

filenamesTifInter <- list.files(path = dmpPath ,pattern="*1.0.1.tiff$",full.names = T)
filenamesTifInter2 <- list.files(path = dmpPath ,pattern="*1.0.1.tiff$",full.names = F)


##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  iDMP <- raster(filenamesTifInter[i])
  iDMP <- extend(iDMP, extent(aoi))
  iDMP <- crop(iDMP, extent(aoi))
  #iDMP <- mask(iDMP, aoi)
  writeRaster(iDMP, paste0(dmpPath, substr(filenamesTifInter2[i], 1, nchar(filenamesTifInter2[i])-5), "_clip.tif"), overwrite = T)
  print(i)
}
