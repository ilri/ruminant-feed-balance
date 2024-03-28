library(raster)


iCopernicustotalArea <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300.tif') #In hectares
DEA300 <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA_unproj.tif') #In 20m2
DEA300 <- projectRaster(DEA300, iCopernicustotalArea)
DEA300 <- resample(DEA300, iCopernicustotalArea, method="ngb")
writeRaster(DEA300, 'SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA.tif', overwrite = T)
