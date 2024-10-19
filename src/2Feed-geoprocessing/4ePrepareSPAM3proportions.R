gc()
library(terra)
library(readr)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

spamPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020")

cropLookup <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/CropParams/Crop classification_feed basket.csv"))
filesSPAM <- list.files(path = spamPath, pattern = "_a.tif$", full.names = T)
stCrops <- rast(filesSPAM)

extracted_value <- sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM)

indexCere <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "cere"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexRoots <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "roots"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexLeg <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "leg"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexOilc <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "oilc"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))

areaTotal <- app(stCrops, fun = sum, na.rm = T)
areaCere <- app(stCrops[[indexCere]], fun = sum, na.rm = T)
areaRoots <- app(stCrops[[indexRoots]], fun = sum, na.rm = T)
areaLeg <- app(stCrops[[indexLeg]], fun = sum, na.rm = T)
areaOilc <- app(stCrops[[indexOilc]], fun = sum, na.rm = T)

propCere <- areaCere / areaTotal
propRoots <- areaRoots / areaTotal
propLeg <- areaLeg / areaTotal
propOilc <- areaOilc / areaTotal

writeRaster(propCere, paste0(spamPath, "/propCereSPAM.tif"), overwrite=TRUE)
writeRaster(propRoots, paste0(spamPath, "/propRootsSPAM.tif"), overwrite=TRUE)
writeRaster(propLeg, paste0(spamPath, "/propLegSPAM.tif"), overwrite=TRUE)
writeRaster(propOilc, paste0(spamPath, "/propOilcSPAM.tif"), overwrite=TRUE)
