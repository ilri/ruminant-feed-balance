library(raster)
library(gdalUtils)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/LandUse")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse"); dir.create(outdir, F, T)

# read AOI
aoi_path <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp")

luClasses <- c("Crops", "Grass", "Shrub", "Tree")

for(luClass in luClasses){
  luFile <- paste0(indir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", luClass,"-CoverFraction-layer_EPSG-4326.tif")
  gdalwarp(srcfile = luFile, dstfile = paste0(outdir, "/LU", tolower(luClass), "300.tif"), overwrite = T, tr = c(0.00297619, 0.00297619), r = "bilinear", cutline = aoi_path, crop_to_cutline = T) #0.00297619, 0.00297619
}