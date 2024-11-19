# Prepare SPAM harvest index
# Author: Simon Fraval
# Last modified by John Mutua on 11/11/2024

# # Install required packages
# install.packages("terra")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("rgdal")

# Load libraries
library(dplyr)
library(raster)
library(rgdal)
library(readr)

#Runs with 16gb ram and 40+gb hdd space
rasterOptions(tmpdir="/home/s2255815/scratch/AUTemp")
rasterOptions(maxmemory = 1e+60)
rasterOptions(todisk = TRUE)
#memory.limit(size = 56000) #Windows specific

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

# country
country <- "Nigeria"

#setwd("/exports/eddie/scratch/sfraval/feed-surfaces/")
cropHI <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/CropParams/crop_harvest index.csv"))

pathSPAM <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020")
pathSPAMInter <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020/intermediate")
#From SPAM documentation: *_TA	all technologies together, ie complete crop; *_TI	irrigated portion of crop; *_TH	rainfed high inputs portion of crop; *_TL	rainfed low inputs portion of crop; *_TS	rainfed subsistence portion of crop; *_TR	rainfed portion of crop (= TA - TI, or TH + TL + TS)
#end of file name should be physical area_cropname_a -> last a standing for all tech together.
filesSPAM <- list.files(path = pathSPAM, pattern = "_a.tif$", full.names = T)
stSPAM <- stack(filesSPAM)
gc()

#@Fill NAs of SPAM layers? At the moment all crops are assumed to be animal digestable if NA

### produce harvest index layer
#crops <- c('bana','barl','bean','cass','chic','cowp','grou','lent','maiz','ocer','opul','orts','pmil','pige','plnt','pota','rape','rice','sesa','smil','sorg','soyb','sugb','sugc','sunf','swpo','temf','trof','vege','whea','yams')
crops <-sub(".*_a_(.*?)_a\\.tif$", "\\1", filesSPAM)

# crops2 <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/CropParams/crop_harvest index.csv")) %>%
#   pull(codeSPAM) %>%
#   unique()

tmpCropIndex <- grep(pattern = paste(crops, collapse = "|"), names(stSPAM))
#iSPAMcropArea <- overlay(stSPAM[[tmpCropIndex]], fun = sum, na.rm=TRUE) #this could not work??
iSPAMcropArea <- calc(stack(stSPAM[[tmpCropIndex]]), fun = sum, na.rm = TRUE)

print("past 1")

stHI <- stack()
stSPAMcropProp <- stack()
gc()
for(i in 1: length(crops)){
  tmpCropIndex <- grep(pattern = paste(crops[i], collapse = "|"), names(stSPAM))
  iSPAMtmpArea <- overlay(stSPAM[[tmpCropIndex]], fun = sum, na.rm=TRUE)
  icrop <- stSPAM[[tmpCropIndex]]
  icrop[icrop >0] <- (1 - cropHI$harvest_index[cropHI$codeSPAM == crops[i]])
  stHI <- stack(stHI, icrop)
  #stHI <- stack(stHI, overlay(iSPAMtmpArea, fun = function((x){ifelse(!is.na(x), (1 - cropHI$harvest_index[cropHI$name == crops[i]]), NA)}))
  
  stSPAMcropProp <- stack(stSPAMcropProp, overlay(iSPAMtmpArea, iSPAMcropArea, fun = function(x, y){(x/y)}))
  
  print(paste("Loop", i))
}
gc()

iSPAMcropResFrac <- weighted.mean(stHI, stSPAMcropProp, na.rm = T)
iSPAMcropResFrac <- reclassify(iSPAMcropResFrac, cbind(NA, NA, 0.8), right=FALSE) #Replace NA with 0.8 - assume that 80% is available for animals

print("past mean")

writeRaster(iSPAMcropResFrac, paste0(pathSPAMInter, "/crop_res_frac.tif"), overwrite = T)