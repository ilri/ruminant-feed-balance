# Prepare feed proportions
# Author: Simon Fraval
# Last modified by John Mutua on 12/11/2024

# # Install required packages
# install.packages("terra")
# install.packages("readr")

# Load libraries
library(terra)
library(readr)

terraOptions(tempdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "."

# country
country <- "Nigeria"

spamPath <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020")

cropLookup <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/", country, "/CropParams/Crop classification_feed basket.csv"))
filesSPAM <- list.files(path = spamPath, pattern = "_a.tif$", full.names = T)
stCrops <- rast(filesSPAM)

#extracted_value <- sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM)

# Index major feed types
indexCere <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "cere"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexRoots <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "roots"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexLeg <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "leg"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))
indexOilc <- grep(pattern = paste(cropLookup$codeSPAM[cropLookup$codeBasket_grouped_NGA == "oilc"], collapse = "|"), sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM))

# Extraction area for major feed categories
areaTotal <- app(stCrops, fun = "sum", na.rm = T)
areaCere <- app(stCrops[[indexCere]], fun = "sum", na.rm = T)
areaRoots <- app(stCrops[[indexRoots]], fun = "sum", na.rm = T)
areaLeg <- app(stCrops[[indexLeg]], fun = "sum", na.rm = T)
areaOilc <- app(stCrops[[indexOilc]], fun = "sum", na.rm = T)

# Calculate proportions
propCere <- areaCere / areaTotal
propRoots <- areaRoots / areaTotal
propLeg <- areaLeg / areaTotal
propOilc <- areaOilc / areaTotal

# Write outputs
writeRaster(propCere, paste0(spamPath, "/propCereSPAM.tif"), overwrite=TRUE)
writeRaster(propRoots, paste0(spamPath, "/propRootsSPAM.tif"), overwrite=TRUE)
writeRaster(propLeg, paste0(spamPath, "/propLegSPAM.tif"), overwrite=TRUE)
writeRaster(propOilc, paste0(spamPath, "/propOilcSPAM.tif"), overwrite=TRUE)
gc()
