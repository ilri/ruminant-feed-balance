library(raster)

setwd('')


cropLookup <- read.csv('CropParams/Crop classification_feed basket.csv')
filesSPAM <- list.files(path = 'SPAM2017', pattern = "_A.tif$", full.names = T)
stCrops <- stack(filesSPAM)

indexCere <- grep(pattern = toupper(paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped == "cere"], collapse = "|")), substr(filesSPAM, 29, 32))
indexRoots <- grep(pattern = toupper(paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped == "roots"], collapse = "|")), substr(filesSPAM, 29, 32))
indexLeg <- grep(pattern = toupper(paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped == "leg"], collapse = "|")), substr(filesSPAM, 29, 32))
indexOilc <- grep(pattern = toupper(paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped == "oilc"], collapse = "|")), substr(filesSPAM, 29, 32))

areaTotal <- overlay(stCrops, fun = sum, na.rm = T)
areaCere <- overlay(stCrops[[indexCere]], fun = sum, na.rm = T)
areaRoots <- overlay(stCrops[[indexRoots]], fun = sum, na.rm = T)
areaLeg <- overlay(stCrops[[indexLeg]], fun = sum, na.rm = T)
areaOilc <- overlay(stCrops[[indexOilc]], fun = sum, na.rm = T)

propCere <- areaCere / areaTotal
propRoots <- areaRoots / areaTotal
propLeg <- areaLeg / areaTotal
propOilc <- areaOilc / areaTotal

writeRaster(propCere, 'propCereSPAM.tif')
writeRaster(propRoots, 'propRootsSPAM.tif')
writeRaster(propLeg, 'propLegSPAM.tif')
writeRaster(propOilc, 'propOilcSPAM.tif')