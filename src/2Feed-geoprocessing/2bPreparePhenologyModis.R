library(raster)
library(gdalUtils)
#library(rgdal)
#library(ncdf4)
#library(ncdf.helpers)

#memory.limit(size=64000)

rasterOptions(maxmemory = 1e+60)


setwd("")


phenPath <- 'SpatialData/inputs/Feed_DrySeason/PhenologyModis/'
filenames <- list.files(path = phenPath,pattern=".hdf$",full.names = T)

#Convert HDF4 files to tif, extracting both bands. netHDF packages don't work in this instance. gdal_translate doesn't make a perfect copy though.
for (filename in filenames)
{
  sds <- get_subdatasets(filename)
  gdal_translate(sds[grep(pattern = ":Greenup", sds)], b = 1, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_greenup1" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Peak", sds)], b = 1, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_peak1" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Senescence", sds)], b = 1, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_senescence1" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Maturity", sds)], b = 1, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_maturity1" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Dormancy", sds)], b = 1, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_dormancy1" ,".tif"))
  
  gdal_translate(sds[grep(pattern = ":Greenup", sds)], b = 2, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_greenup2" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Peak", sds)], b = 2, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_peak2" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Senescence", sds)], b = 2, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_senescence2" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Maturity", sds)], b = 2, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_maturity2" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "Dormancy", sds)], b = 2, dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_dormancy2" ,".tif"))
  
  gdal_translate(sds[grep(pattern = "NumCycles", sds)], dst_dataset=paste0(substr(filename, 1, nchar(filename)-4), "_numcycles" ,".tif"))
  
}

##Reproject all rasters
filenamesTif <- list.files(path = paste0(phenPath) ,pattern=".tif$",full.names = T)
#filenamesTif2 <- list.files(path = paste0(phenPath) ,pattern=".tif$",full.names = F)

dir.create(paste0(phenPath,"/intermediate/"))

#for(i in 1:length(filenamesTif)){
#gdalwarp(srcfile = filenamesTif[i], dstfile = paste0(phenPath, "/intermediate/", filenamesTif2[i]), overwite = T, s_srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", t_srs = "+proj=longlat +datum=WGS84 +no_defs", r = "bilinear")

#}

phaseList <- c("greenup1", "maturity1", "peak1", "senescence1", "dormancy1", "numcycles", "greenup2", "maturity2", "peak2", "senescence2", "dormancy2")
for(i in 1:length(phaseList)){
  gdalwarp(srcfile = filenamesTif[grep(pattern = phaseList[i],  filenamesTif)], dstfile = paste0(phenPath, "/intermediate/", phaseList[i], ".tif"), overwite = T, s_srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", t_srs = "+proj=longlat +datum=WGS84 +no_defs", r = "bilinear")
  
}

#intermedatePath <- list.files(path = paste0(phenPath, "/intermediate/") ,pattern=".tif$",full.names = T)
filenamesTifInter <- list.files(path = paste0(phenPath, "/intermediate/") ,pattern=".tif$",full.names = T)
filenamesTifInter2 <- list.files(path = paste0(phenPath, "/intermediate/") ,pattern=".tif$",full.names = F)

dir.create(paste0(phenPath,"/outputTif/"))
##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(phenPath, "/outputTif/", "pheno", toupper(substr(filenamesTifInter2[i], 1, 1)), substr(filenamesTifInter2[i], 2, nchar(filenamesTifInter2[i]))), overwite = T, tr = c(0.002976190476204010338, 0.002976190476189799483), r = "bilinear", cutline = "/exports/eddie/scratch/sfraval/feed-surfaces/SpatialData/inputs/aoi1.shp", crop_to_cutline = T) #0.00297619, 0.00297619
}  


