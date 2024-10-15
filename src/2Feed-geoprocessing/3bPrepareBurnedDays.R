library(dplyr)
library(raster)
library(rgdal)
library(sf)
library(terra)


# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# read AOI
aoi <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

pathPhen <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif")
filesPhenology <- list.files(path = pathPhen,pattern=".tif$",full.names = T)

pathBurn <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Burned")
filesBurn <- list.files(path = pathBurn,pattern=".tif$",full.names = T)#[1:36] #Only 2020

#datesBurn <- as.vector(sapply(filesBurn, function(x) substr(x, start =(nchar(x) - 37), stop = (nchar(x) -30)))) #(nchar(x) - 41), stop = (nchar(x) -34))
datesBurn <- sub(".*NTC_(.{6}).*", "\\1", filesBurn)
#datesBurn <- as.Date(datesBurn, "%Y%m%d")
datesBurn <- as.Date(paste0(datesBurn, "01"), format = "%Y%m%d") # Convert it to a date, assuming the first day of the month
datesBurndiff <- as.numeric(datesBurn - as.Date("1970/01/01")) #convert to same date format as Modis phenology

pathLU <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse")
filesLU <- list.files(path = pathLU, pattern = "300.tif$", full.names = T)

rProtectedAreas <- raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPAGlobal.tif")) #Original shp is 4gb+ 
rNonProtectedAreas <- calc(rProtectedAreas, fun = function(x){ifelse(x == 0, 1, 0)})
rm(rProtectedAreas)

stLU <- stack(filesLU)

stPhen <- stack(filesPhenology)
stBurn <- stack(filesBurn)

gc()


##Crop to test area
stLU <- extend(stLU, extent(stBurn[[1]]))
stLU <- crop(stLU, extent(stBurn[[1]]))
stLU <- mask(stLU, aoi)
stPhen <- resample(stPhen, stBurn[[1]], method = "ngb")
stPhen <- extend(stPhen, extent(stBurn[[1]]))
stPhen <- crop(stPhen, extent(stBurn[[1]]))
#stPhen <- mask(stPhen, aoi)
rNonProtectedAreas <- resample(rNonProtectedAreas, stBurn[[1]], method = "ngb")
rNonProtectedAreas <- extend(rNonProtectedAreas, extent(stBurn[[1]]))
rNonProtectedAreas <- crop(rNonProtectedAreas, extent(stBurn[[1]]))
#rNonProtectedAreas <- mask(rNonProtectedAreas, aoi)
print("past 0")

names(stBurn) <- paste0("d", datesBurndiff)
stBurn <- reclassify(stBurn, c(100, 255, NA)) #NA values are 254 

stLU$LUtree300 <- reclassify(stLU$LUtree300, c(-Inf, 0, 0, 200, Inf, 0)) 
stLU$LUtree300[is.na(stLU$LUtree300)] <- 0


print("past 1")
gc()

#####Estimate total Burn dekads
funBurnGrass <- function(burnBin, crops, grass, forest, shrub, nonprotected, greenup, senesence, greenup2, senesence2) {ifelse((senesence >= datesBurndiff[i] & (grass+shrub) >0.25) | (greenup2 <= datesBurndiff[i] & senesence2 >= datesBurndiff[i] & (grass+shrub) >0.25), burnBin, NA) } 
funBurnCrops <- function(burnBin, crops, greenup, senesence, greenup2, senesence2) {ifelse((senesence < datesBurndiff[i] & senesence + 60 > datesBurndiff[i] & crops > 0.25) | (senesence2 < datesBurndiff[i] & crops > 0.25), burnBin, NA) } 


iBurnGrass <- stack()
iBurnCrops <- stack()
for(i in 1:length(names(stBurn))){
  
  test <- ifelse((senesence < datesBurndiff[i] & senesence + 60 > datesBurndiff[i] & crops > 0.25) | (senesence2 < datesBurndiff[i] & crops > 0.25), burnBin, NA) 
  
  iBurnCropsCon1 <- stPhen$phenoSenescence1 < datesBurndiff[i] & stPhen$phenoSenescence1 + 60 > datesBurndiff[i] & stLU$LUcrops300 > 0.25
  iBurnCropsCon2 <- stPhen$phenoSenescence2 < datesBurndiff[i] & stLU$LUcrops300 > 0.25
  iBurnCropsCon3 <- stLU$LUcrops300 > 0.25
  iBurnCropsCon4 <- iBurnCropsCon2 & iBurnCropsCon3
  iBurnCropsTemp <- ifelse(iBurnCropsCon1 | iBurnCropsCon4, stBurn[[i]], NA) 
  iBurnCrops <- stack(iBurnCrops, iBurnCropsTemp)
  
  
  #iBurnGrass <- stack(iBurnGrass, overlay(stBurn[[i]], stLU$LUcrops300, stLU$LUgrass300, stLU$LUtree300, stLU$LUshrub300, rNonProtectedAreas, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funBurnGrass))
  #writeRaster(iDMP, paste0('SpatialData/inputs/Feed_quantity/totalDMP', datesDMP[i], '.tif'), overwrite = TRUE)  
  
  iBurnCrops <- stack(iBurnCrops, overlay(stBurn[[i]], stLU$LUcrops300, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funBurnCrops))

  #writeRaster(iDMPCropGrowing, paste0('SpatialData/inputs/Feed_quantity/cropDMP', datesDMP[i], '.tif'), overwrite = TRUE)
  print(paste("cycle", i))
  gc()
}

gc()

burnGrassDekads <- sum(iBurnGrass, na.rm = T)
writeRaster(burnGrassDekads, 'SpatialData/inputs/burnGrassDekads.tif', overwrite = TRUE)

burnCropDekads <- sum(iBurnCrops, na.rm = T)
writeRaster(burnCropDekads, 'SpatialData/inputs/burnCropsDekads.tif', overwrite = TRUE)

