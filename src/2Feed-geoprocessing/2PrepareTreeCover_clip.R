library(raster)
library(stars)
#library(terra)
library(sf)
library(exactextractr)
library(terra)
library(gdalUtils)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
proPath <- paste0(root, "/src/1Data-download/SpatialData/inputs/TreeCover")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/TreeCover"); dir.create(outdir, F, T)

# read AOI
aoi_path <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp")

filenamesTifInter <- list.files(path = proPath ,pattern="*.tif$",full.names = T)
#filenamesTifInter2 <- list.files(path = proPath ,pattern="*.tif$",full.names = F)

##@Resample and crop with gdal?
for(i in 1:length(filenamesTifInter)){
  gdalwarp(srcfile = filenamesTifInter[i], dstfile = paste0(outdir, "/", basename(filenamesTifInter[i])), overwrite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = aoi_path, crop_to_cutline = T) #0.00297619, 0.00297619
} 

# Fix extent
dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

itreeCover <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/TreeCover/ps_africa_treecover_2019_100m_v1.0.tif"))
itreeCover <- crop(itreeCover, ext(dmpTemp))
itreeCover <- resample(itreeCover, dmpTemp, method="bilinear")
itreeCover <- mask(itreeCover, mask = dmpTemp)

writeRaster(itreeCover, paste0(outdir, "/treecover300m.tif"), overwrite=TRUE)