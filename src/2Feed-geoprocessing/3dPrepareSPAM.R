library(raster)
#library(sp)
#library(sf)
library(gdalUtils)

#rasterOptions(maxmemory = 1e+60)


setwd("")

spamPath <- 'SpatialData/inputs/SPAM2010cropspecies/'

filenamesTifInter <- list.files(path = spamPath ,pattern="*frac.tif$",full.names = T)
filenamesTifInter2 <- list.files(path = spamPath ,pattern="*frac.tif$",full.names = F)


##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(spamPath, filenamesTifInter2[i]), overwrite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = "aoi1.shp", crop_to_cutline = T) #0.00297619, 0.00297619
}  
