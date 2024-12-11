# Prepare burned layers
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("dplyr")
# install.packages("raster")
# install.packages("rgdal")

# Load libraries
library(dplyr)
library(raster)
library(rgdal)

rasterOptions(maxmemory = 1e+60)
rasterOptions(todisk=TRUE)
rasterOptions(tmpdir="/home/s2255815/scratch/AUTemp")

# root folder
root <- "."

# country
country <- "Nigeria"

# read AOI
aoi <- readOGR(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

dmpTemp <- terra::rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

yearList <- c("2020", "2021", "2022", "2023")

lapply(yearList[2:4], function(year){
  
  #year <- "2020"
  
  burnedOut <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Burned/", year); dir.create(burnedOut, F, T)
  pathPhen <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/PhenologyModis/", year, "/outputTif")
  filesPhenology <- list.files(path = pathPhen,pattern=".tif$",full.names = T)
  pathBurn <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Burned/", year)
  filesBurn <- list.files(path = pathBurn,pattern=".tif$",full.names = T)#[1:36] #Only 2015
  #datesBurn <- as.vector(sapply(filesBurn, function(x) substr(x, start =(nchar(x) - 37), stop = (nchar(x) -30)))) #(nchar(x) - 41), stop = (nchar(x) -34))
  datesBurn <- sub(".*NTC_(.{8}).*", "\\1", filesBurn)
  datesBurn <- as.Date(datesBurn, "%Y%m%d")
  datesBurndiff <- as.numeric(datesBurn - as.Date("1970/01/01")) #convert to same date format as Modis phenology
  pathLU <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/LandUse")
  filesLU <- list.files(path = pathLU, pattern = "300.tif$", full.names = T)
  
  rProtectedAreas <- raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/ProtectedAreas/WDPAGlobal.tif")) #Original shp is 4gb+ 
  rNonProtectedAreas <- calc(rProtectedAreas, fun = function(x){ifelse(x == 0, 1, 0)})
  rm(rProtectedAreas)
  
  stLU <- stack(filesLU)
  
  stPhen <- stack(filesPhenology)
  stBurn <- stack(filesBurn)
  
  # gc()
  # 
  # #Crop to test area
  # stLU <- extend(stLU, extent(stBurn[[1]]))
  # stLU <- crop(stLU, extent(stBurn[[1]]))
  # stLU <- mask(stLU, aoi)
  # stPhen <- resample(stPhen, stBurn[[1]], method = "ngb")
  # stPhen <- extend(stPhen, extent(stBurn[[1]]))
  # stPhen <- crop(stPhen, extent(stBurn[[1]]))
  # stPhen <- mask(stPhen, aoi)
  # rNonProtectedAreas <- resample(rNonProtectedAreas, stBurn[[1]], method = "ngb")
  # rNonProtectedAreas <- extend(rNonProtectedAreas, extent(stBurn[[1]]))
  # rNonProtectedAreas <- crop(rNonProtectedAreas, extent(stBurn[[1]]))
  # rNonProtectedAreas <- mask(rNonProtectedAreas, aoi)
  print("past 0")
  gc()
  
  names(stBurn) <- paste0("d", datesBurndiff)
  #stBurn <- reclassify(stBurn, c(100, 255, NA)) #NA values are 254 
  #stBurn <- reclassify(stBurn, c(-Inf, 0, 0, 367, Inf, 0))
  stBurn <- stack(stBurn)
  gc()
  
  stLU$LUtree300 <- reclassify(stLU$LUtree300, c(-Inf, 0, 0, 200, Inf, 0)) 
  stLU$LUtree300[is.na(stLU$LUtree300)] <- 0
  
  print("past 1")
  gc()
  
  #####Estimate total Burn dekads
  #funBurnGrass <- function(burnBin, crops, grass, forest, shrub, nonprotected, greenup, senesence, greenup2, senesence2) {ifelse((senesence >= datesBurndiff[i] & (grass+shrub) >0.25) | (greenup2 <= datesBurndiff[i] & senesence2 >= datesBurndiff[i] & (grass+shrub) >0.25), burnBin, NA) } 
  
  funBurnGrass <- function(burnBin, grass, shrub, greenup, senesence, greenup2, senesence2, burndate) {
    funBurnGrass1 <- overlay(senesence, grass, shrub, fun = function(a, b, c) {return((a >= burndate) & ((b + c) > 0.25))})
    funBurnGrass2 <- overlay(greenup2, senesence2, grass, shrub, fun = function(d, e, f, g) {return((d <= burndate) & (e >= burndate) & ((f + g) > 0.25))})
    funBurnGrass3 <- funBurnGrass1 | funBurnGrass2
    result <- mask(burnBin, funBurnGrass3, maskvalue = 0, updatevalue = NA)
    return(result)}
  
  #funBurnCrops <- function(burnBin, crops, greenup, senesence, greenup2, senesence2) {ifelse((senesence < datesBurndiff[i] & senesence + 60 > datesBurndiff[i] & crops > 0.25) | (senesence2 < datesBurndiff[i] & crops > 0.25), burnBin, NA) } 
  
  funBurnCrops <- function(burnBin, crops, greenup, senesence, greenup2, senesence2, burndate) {
    funBurnCrops1 <- overlay(senesence, crops, fun = function(a, b) {return((a < burndate & a + 60 > burndate) & (b > 0.25))})
    funBurnCrops2 <- overlay(senesence2, crops, fun = function(d, e) {return((d < burndate) & (e > 0.25))})
    funBurnCrops3 <- funBurnCrops1 | funBurnCrops2
    result <- mask(burnBin, funBurnCrops3, maskvalue = 0, updatevalue = NA)
    return(result)}
  
  iBurnGrass <- stack()
  iBurnCrops <- stack()
  for(i in 1:length(names(stBurn))){

    #iBurnGrass <- stack(iBurnGrass, overlay(stBurn[[i]], stLU$LUcrops300, stLU$LUgrass300, stLU$LUtree300, stLU$LUshrub300, rNonProtectedAreas, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funBurnGrass))
    #writeRaster(iDMP, paste0('SpatialData/inputs/Feed_quantity/totalDMP', datesDMP[i], '.tif'), overwrite = TRUE)  
    
    iBurnGrass <- stack(iBurnGrass, funBurnGrass(stBurn[[i]], stLU$LUgrass300, stLU$LUshrub300, stPhen$phenoGreenup1,stPhen$phenoSenescence1, stPhen$phenoGreenup2,stPhen$phenoSenescence2, datesBurndiff[i]))
    
    #iBurnCrops <- stack(iBurnCrops, overlay(stBurn[[i]], stLU$LUcrops300, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funBurnCrops))
    #writeRaster(iDMPCropGrowing, paste0('SpatialData/inputs/Feed_quantity/cropDMP', datesDMP[i], '.tif'), overwrite = TRUE)
    
    iBurnCrops <- stack(iBurnCrops, funBurnCrops(stBurn[[i]], stLU$LUcrops300, stPhen$phenoGreenup1,stPhen$phenoSenescence1, stPhen$phenoGreenup2,stPhen$phenoSenescence2, datesBurndiff[i]))
    
    print(paste("cycle", i))
    gc()
  }
  
  gc()
  
  #burnGrassMonths <- sum(iBurnGrass, na.rm = T)
  burnGrassMonths <- calc(iBurnGrass, fun = function(x) {sum(x > 1, na.rm = TRUE)})
  writeRaster(burnGrassMonths, paste0(burnedOut, "/burnGrassMonths.tif"), overwrite = TRUE)
  rm(burnGrassMonths)
  
  #burnCropMonths <- sum(iBurnCrops, na.rm = T)
  burnCropMonths <- calc(iBurnCrops, fun = function(x) {sum(x > 1, na.rm = TRUE)})
  writeRaster(burnCropMonths, paste0(burnedOut, "/burnCropMonths.tif"), overwrite = TRUE)
  rm(burnCropMonths)
  gc()
  
})
