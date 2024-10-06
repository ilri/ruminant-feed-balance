library(raster)
library(stars)
library(sf)
library(exactextractr)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/LandUse")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse"); dir.create(outdir, F, T)

# read AOI
aoi <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

dmpTemplate <- raster::raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP_2023/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

luClasses <- c("Crops", "Grass", "Shrub", "Tree")

for (luClass in luClasses){
  
  file_name <- basename(luClass)
  
  luFile <- raster::raster(paste0(indir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", luClass,"-CoverFraction-layer_EPSG-4326.tif"))
  
  if (!file.exists(paste0(outdir, "/LU", tolower(luClass), "300.tif", sep=""))){
    
    luFile <- extend(luFile, extent(aoi))
    luFile <- crop(luFile, extent(aoi))
    luFile <- resample(luFile, dmpTemplate)
    
    # save as GeoTIFF
    raster::writeRaster(iDMP, filename = paste0(outdir, "/LU", tolower(luClass), "300.tif", sep=""), overwrite=TRUE)
    
  }else{
    cat("Already cropped: ", nc_name, "\n")
  }
}







/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse


iCopernicustotalArea <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300.tif') #In hectares
DEA300 <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA_unproj.tif') #In 20m2
DEA300 <- projectRaster(DEA300, iCopernicustotalArea)
DEA300 <- resample(DEA300, iCopernicustotalArea, method="ngb")
writeRaster(DEA300, 'SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA.tif', overwrite = T)
