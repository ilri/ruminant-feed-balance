gc()
library(dplyr)
library(readr)
library(terra)

args <- commandArgs(TRUE)

#EDDIE_TMP <- as.character(args[1])

#Runs with 16gb ram and 40+gb hdd space
terraOptions(memmax = 1e+60)
terraOptions(todisk = TRUE)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

pathLU <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse")
filesLU <- list.files(path = pathLU, pattern = ".tif$", full.names = T)

pathSPAM <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020")
pathSPAMInter <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020/intermediate"); dir.create(pathSPAMInter, F, T)
#From SPAM documentation: *_TA	all technologies together, ie complete crop; *_TI	irrigated portion of crop; *_TH	rainfed high inputs portion of crop; *_TL	rainfed low inputs portion of crop; *_TS	rainfed subsistence portion of crop; *_TR	rainfed portion of crop (= TA - TI, or TH + TL + TS)
#end of file name should be physical area_cropname_a -> last a standing for all tech together.
filesSPAM <- list.files(path = pathSPAM, pattern = "_a.tif$", full.names = T)

#stSPAM <- stack(filesSPAM)
stSPAM <- rast(filesSPAM)

###Calculate non-feed crops proportion from SPAM model.
#@Fill NAs of SPAM layers? At the moment all crops are assumed to be animal digestable if NA
tmpNonFeed <- read_csv(paste0(root, "/src/1Data-download/Tables/inputs/CropParams/crop_harvest index.csv")) %>% filter(Excluded != "0") %>%
  pull(codeSPAM) %>%
  unique()
tmpNonFeedIndex <- grep(pattern = paste(tmpNonFeed, collapse = "|"), names(stSPAM))
##iSPAMtotalArea <- sum(stSPAM, na.rm=T)
##iSPAMnonFeedArea <- sum(stSPAM[[tmpNonFeedIndex]], na.rm=T)
iSPAMtotalArea <- app(stSPAM, fun = sum, na.rm=TRUE)
#iSPAMtotalArea <- resample(iSPAMtotalArea, stLU$LUcrops300, method = "bilinear")
iSPAMnonFeedArea <- app(stSPAM[[tmpNonFeedIndex]], fun = sum, na.rm=TRUE)
#iSPAMnonFeedArea <- resample(iSPAMnonFeedArea, stLU$LUcrops300, method = "bilinear")

iSPAMnonFeedFrac <- (iSPAMnonFeedArea / iSPAMtotalArea)
#iSPAMnonFeedFrac[is.na(iSPAMnonFeedFrac[])] <- 0
iSPAMnonFeedFrac <- classify(iSPAMnonFeedFrac, cbind(NA, NA, 0), right=FALSE) # Replace missing values with 0
iSPAMAnimalDigestCropFrac <- 1 - iSPAMnonFeedFrac

iSPAMAnimalDigestCropFrac <- writeRaster(iSPAMAnimalDigestCropFrac, paste0(pathSPAMInter, "/animal_digest_frac.tif"), overwrite = T)