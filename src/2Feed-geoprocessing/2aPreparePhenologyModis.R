# Prepare phenology layers
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
library(raster)
library(gdalUtils)
library(sf)
#library(rgdal)
#library(ncdf4)
#library(ncdf.helpers)

#memory.limit(size=64000)

# study area
country <- "Nigeria"

rasterOptions(maxmemory = 1e+60)
root <- "."

# read AOI
aoi_path <- paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp")

yearList <- c("2020", "2021", "2022", "2023")

for(year in yearList){
  
  phenPath <- paste0(root, "/src/1Data-download/SpatialData/inputs/PhenologyModis/", year)
  
  filenames <- list.files(path = phenPath,pattern=".hdf$",full.names = T)
  
  #Convert HDF4 files to tif, extracting both bands. netHDF packages don't work in this instance. gdal_translate doesn't make a perfect copy though.
  for (filename in filenames){
    
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
  filenamesTif <- list.files(path = paste0(phenPath), pattern=".tif$",full.names = T)
  
  phenPathOut <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/PhenologyModis/", year); dir.create(phenPathOut, F, T)
  dir.create(paste0(phenPathOut,"/intermediate/"), F, T)
  
  phaseList <- c("greenup1", "maturity1", "peak1", "senescence1", "dormancy1", "numcycles", "greenup2", "maturity2", "peak2", "senescence2", "dormancy2")
  for(i in 1:length(phaseList)){
    gdalwarp(srcfile = filenamesTif[grep(pattern = phaseList[i],  filenamesTif)], dstfile = paste0(phenPathOut, "/intermediate/", phaseList[i], ".tif"), overwite = T, s_srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", t_srs = "+proj=longlat +datum=WGS84 +no_defs", r = "bilinear", overwrite = TRUE)
    
  }
  
  #intermedatePath <- list.files(path = paste0(phenPath, "/intermediate/") ,pattern=".tif$",full.names = T)
  filenamesTifInter <- list.files(path = paste0(phenPathOut, "/intermediate"), pattern=".tif$",full.names = T)
  #filenamesTifInter2 <- list.files(path = paste0(phenPathOut, "/intermediate/"), pattern=".tif$",full.names = F)
  
  # dir.create(paste0(phenPathOut,"/outputTif/"))
  ##@Resample and crop with gdal?
  for(i in 1:length(filenamesTifInter)){
    gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(phenPathOut, "/intermediate/", "pheno", toupper(substr(basename(filenamesTifInter[i]), 1, 1)), substr(basename(filenamesTifInter[i]), 2, nchar(basename(filenamesTifInter[i])))), overwite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = aoi_path, crop_to_cutline = T, overwrite = TRUE) #0.00297619, 0.00297619
  }  
  
}
