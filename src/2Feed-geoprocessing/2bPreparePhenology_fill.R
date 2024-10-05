library(raster)
library(gdalUtils)
#library(rgdal)
#library(ncdf4)
#library(ncdf.helpers)

args <- commandArgs(TRUE)

#EDDIE_TMP <- as.character(args[1])
#print(EDDIE_TMP)
#print(args)

#Runs with 16gb ram and 40+gb hdd space
#rasterOptions(tmpdir = EDDIE_TMP)
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)
##memory.limit(size = 56000) #Windows specific


setwd("")


#LUgrass <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/PROBAV_LC100_global_v3.0.1_2015-base_Grass-CoverFraction-layer_EPSG-4326.tif')

phenPath <- 'SpatialData/inputs/Feed_DrySeason/PhenologyModis/'
filenames <- list.files(path = phenPath,pattern=".hdf$",full.names = T)


#intermedatePath <- list.files(path = paste0(phenPath, "/outputTif/") ,pattern=".tif$",full.names = T)
filenamesTifInter <- list.files(path = paste0(phenPath, "/outputTif/") ,pattern=".tif$",full.names = T)
filenamesTifInter2 <- list.files(path = paste0(phenPath, "/outputTif/") ,pattern=".tif$",full.names = F)


#interpolate, resample and crop all rasters
width = 19

phenoGreenup1 <- raster(filenamesTifInter[grep(pattern = "Greenup1.tif", filenamesTifInter)])
phenoGreenup1 <- focal(phenoGreenup1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
phenoGreenup1 <- focal(phenoGreenup1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
phenoGreenup1 <- focal(phenoGreenup1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
writeRaster(phenoGreenup1, paste0(phenPath, "/outputTif/phenoGreenup1.tif"), overwrite = T)
rm(phenoGreenup1)  
  
    
phenoPeak1 <- raster(filenamesTifInter[grep(pattern = "Peak1", filenamesTifInter)])
phenoPeak1 <- focal(phenoPeak1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
phenoPeak1 <- focal(phenoPeak1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
phenoPeak1 <- focal(phenoPeak1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
writeRaster(phenoPeak1, paste0(phenPath, "/outputTif/phenoPeak1.tif"), overwrite = T)
rm(phenoPeak1)


phenoSenescence1 <- raster(filenamesTifInter[grep(pattern = "Senescence1", filenamesTifInter)])
phenoSenescence1 <- focal(phenoSenescence1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
phenoSenescence1 <- focal(phenoSenescence1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
phenoSenescence1 <- focal(phenoSenescence1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
writeRaster(phenoSenescence1, paste0(phenPath, "/outputTif/phenoSenescence1.tif"), overwrite = T)
rm(phenoSenescence1)
 

phenoMaturity1 <- raster(filenamesTifInter[grep(pattern = "Maturity1", filenamesTifInter)])  
phenoMaturity1 <- focal(phenoMaturity1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
phenoMaturity1 <- focal(phenoMaturity1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
phenoMaturity1 <- focal(phenoMaturity1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
writeRaster(phenoMaturity1, paste0(phenPath, "/outputTif/phenoMaturity1.tif"), overwrite = T)
rm(phenoMaturity1)

  

phenoDormancy1 <- raster(filenamesTifInter[grep(pattern = "Dormancy1", filenamesTifInter)]) 
phenoDormancy1 <- focal(phenoDormancy1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE) #!!!Very rough interpolation
phenoDormancy1 <- focal(phenoDormancy1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)
phenoDormancy1 <- focal(phenoDormancy1, w=matrix(1,nrow=width, ncol=width), fun=mean, NAonly=TRUE, na.rm=TRUE)

writeRaster(phenoDormancy1, paste0(phenPath, "/outputTif/phenoDormancy1.tif"), overwrite = T)
#rm(phenoDormancy1)

  

