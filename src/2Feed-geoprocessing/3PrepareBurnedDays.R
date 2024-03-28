library(dplyr)
library(raster)
library(rgdal)


aoi <- readOGR("SpatialData/inputs/aoi1.shp")

pathPhen <- 'SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif/'
filesPhenology <- list.files(path = pathPhen,pattern=".tif$",full.names = T)
pathBurn <- 'SpatialData/inputs/Burned/'
filesBurn <- list.files(path = pathBurn,pattern=".tiff$",full.names = T)#[1:36] #Only 2015
datesBurn <- as.vector(sapply(filesBurn, function(x) substr(x, start =(nchar(x) - 37), stop = (nchar(x) -30)))) #(nchar(x) - 41), stop = (nchar(x) -34))
datesBurn <- as.Date(datesBurn, "%Y%m%d")
datesBurndiff <- as.numeric(datesBurn - as.Date("1970/01/01")) #convert to same date format as Modis phenology
pathLU <- 'SpatialData/inputs/Feed_DrySeason/LandUse/'
filesLU <- list.files(path = pathLU, pattern = "300.tif$", full.names = T)

rProtectedAreas <- raster('SpatialData/inputs/ProtectedAreas/WDPAGlobal.tif') #Original shp is 4gb+ 
rNonProtectedAreas <- calc(rProtectedAreas, fun = function(x){ifelse(x == 0, 1, 0)})
rm(rProtectedAreas)


stLU <- stack(filesLU)


stPhen <- stack(filesPhenology)
stBurn <- stack(filesBurn)


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

#####Estimate total Burn dekads
funBurnGrass <- function(burnBin, crops, grass, forest, shrub, nonprotected, greenup, senesence, greenup2, senesence2) {ifelse((senesence >= datesBurndiff[i] & (grass+shrub) >0.25) | (greenup2 <= datesBurndiff[i] & senesence2 >= datesBurndiff[i] & (grass+shrub) >0.25), burnBin, NA) } 
funBurnCrops <- function(burnBin, crops, greenup, senesence, greenup2, senesence2) {ifelse((senesence < datesBurndiff[i] & senesence + 60 > datesBurndiff[i] & crops > 0.25) | (senesence2 < datesBurndiff[i] & crops > 0.25), burnBin, NA) } 


iBurnGrass <- stack()
iBurnCrops <- stack()
for(i in 1:length(names(stBurn))){
  
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

