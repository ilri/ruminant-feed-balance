#     .-.                                    ,-.
#  .-(   )-.                              ,-(   )-.
# (     __) )-.                        ,-(_      __)
#  `-(       __)                      (_    )  __)-'
#  - -  :   :  - - Dry matter feed potential
#      / `-' \     v0.1
#    ,    |   .    Simon Fraval
#         .        R 3.6.1           _
#                                  >')
#                                  (\\         (W)
#                                   = \     -. `|'
#                                   = ,-      \(| ,-
#                                 ( |/  _______\|/____
#                                \|,-'::::::::::::::
#            _                 ,----':::::::::::::::::
#         {><_'c   _      _.--':MJP:::::::::::::::::::
#__,'`----._,-. {><_'c  _-':::::::::::::::::::::::::::
#:.:.:.:.:.:.:.\_    ,-'.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:
#.:.:.:.:.:.:.:.:`--'.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.
#.....................................................

yearOffset <- (0*365) # Base year = 2019

library(dplyr)
library(raster)
library(rgdal)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

cropOutdir <- paste0(root, "/src/3Balance-estimates/Nigeria/SpatialData/inputs/Cropping_days"); dir.create(cropOutdir, F, T)
FeedQuantityOutdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/Feed_quantity"); dir.create(FeedQuantityOutdir, F, T)
FeedQuantityMeansOutdir <- paste0(root, "/src/3Balance-estimates/Nigeria/SpatialData/inputs/Feed_quantity"); dir.create(FeedQuantityMeansOutdir, F, T)

# read AOI
aoi <- readOGR(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi0.shp"))

pathPhen <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif")
filesPhenology <- list.files(path = pathPhen,pattern=".tif$",full.names = T)

pathDMP <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP")

yearList <- c("2020", "2021", "2022", "2023")

#filesDMP <- list.files(path = pathDMP, pattern=".tif$",full.names = T)#[1:36] #2019 is tif with 1 F

for (year in yearList){
  
  year<-"2022"
  
  filesDMP <- list.files(path = pathDMP, pattern = paste0("RT6_", year, ".*\\.tif$"), full.names = TRUE)
  stDMP <- stack(filesDMP)
  
  #datesDMP <- as.vector(sapply(filesDMP, function(x) substr(x, start =(nchar(x) - 37), stop = (nchar(x) -30)))) #(nchar(x) - 41), stop = (nchar(x) -34))
  datesDMP <- sub(".*RT6_(.{8}).*", "\\1", filesDMP)
  datesDMP <- as.Date(datesDMP, "%Y%m%d")
  datesDMPdiff <- as.numeric(datesDMP - as.Date("1970/01/01")) #convert to same date format as Modis phenology
  pathLU <-  paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse")
  filesLU <- list.files(path = pathLU, pattern = "300.tif$", full.names = T)
  pathSPAM <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020")
  #From SPAM documentation: *_TA	all technologies together, ie complete crop; *_TI	irrigated portion of crop; *_TH	rainfed high inputs portion of crop; *_TL	rainfed low inputs portion of crop; *_TS	rainfed subsistence portion of crop; *_TR	rainfed portion of crop (= TA - TI, or TH + TL + TS)
  #end of file name should be physical area_cropname_a -> last a standing for all tech together.
  #filesSPAM <- list.files(path = pathSPAM, pattern = "_a.tif$", full.names = T)
  iSPAMAnimalDigestCropFrac <- raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020/animal_digest_frac.tif"))
  
  rProtectedAreas <- stack(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPAGlobal.tif")) #Original shp is 4gb+ 
  rNonProtectedAreas <- calc(rProtectedAreas, fun = function(x){ifelse(x == 0, 1, 0)})
  rm(rProtectedAreas)
  
  print("past protected")
  
  stLU <- stack(filesLU)
  #LUcrops300DEA <- raster(paste0(pathLU, "LUcrops300DEA.tif"))
  
  
  # stPhen <- stack(raster('SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif/phenoGreenup1.tif'), 
  #                 raster('SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif/phenoSenescence1.tif'), 
  #                 raster('SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif/phenoGreenup2.tif'), 
  #                 raster('SpatialData/inputs/Feed_DrySeason/PhenologyModis/outputTif/phenoSenescence2.tif'))
  
  stPhen <- stack(raster(grep("phenoGreenup1.tif", filesPhenology, value=TRUE)), 
                  raster(grep("phenoSenescence1.tif", filesPhenology, value=TRUE)), 
                  raster(grep("phenoGreenup2.tif", filesPhenology, value=TRUE)), 
                  raster(grep("phenoSenescence2.tif", filesPhenology, value=TRUE)))
  
  gc()
  
  # ##Crop land use to test area
  # LUcrops300DEA <- extend(LUcrops300DEA, extent(stDMP[[1]]))
  # LUcrops300DEA <- crop(LUcrops300DEA, extent(stDMP[[1]]))
  # LUcrops300DEA <- mask(LUcrops300DEA, aoi)
  # stLU <- extend(stLU, extent(stDMP[[1]]))
  # stLU <- crop(stLU, extent(stDMP[[1]]))
  # stLU <- mask(stLU, aoi)
  # #stLU <- stack(stLU, LUcrops300DEA)
  
  # ##Revise grass and shrub area 
  # diffCrop <- LUcrops300DEA - stLU$LUcrops300
  stLU$LUgrassShrub300 <- sum(stLU$LUgrass300, stLU$LUshrub300, na.rm = T)
  #stLU$LUgrassShrub300 <- stLU$LUgrassShrub300 - LUcrops300DEA
  
  # stLU$LUcrops300 <- LUcrops300DEA
  
  # ##Crop phenology to test area
  # #stPhen <- ext(stPhen, ext(stDMP[[1]]))
  # stPhen <- crop(stPhen, ext(stDMP[[1]]))
  # stPhen <- resample(stPhen, stDMP[[1]], method = "near")
  stPhen <- stPhen + yearOffset #!Change to + or - as needed
  #stPhen$phenoGreenup2 <- overlay(stPhen$phenoGreenup2, stPhen$phenoGreenup1, stPhen$phenoSenescence2, fun = function(x, g1, s2){ifelse(x > max(datesDMPdiff)+30 & x-365 < g1 & s2-365 < g1, x-365, x)})
  #stPhen$phenoSenescence2 <- overlay(stPhen$phenoSenescence2, stPhen$phenoGreenup1, fun = function(x, g1){ifelse(x > max(datesDMPdiff)+30 & x-365 < g1, x-365, x)})
  
  stPhen$phenoGreenup2 <- calc(stPhen$phenoGreenup2, fun = function(x){ifelse(x > max(datesDMPdiff)+30, NA, x)})
  stPhen$phenoSenescence2 <- calc(stPhen$phenoSenescence2, fun = function(x){ifelse(x > max(datesDMPdiff)+30, NA, x)})
  stPhen$phenoGreenup1 <- calc(stPhen$phenoGreenup1, fun = function(x){ifelse(x < min(datesDMPdiff)-30, NA, x)})
  stPhen$phenoSenescence1 <- calc(stPhen$phenoSenescence1, fun = function(x){ifelse(x < min(datesDMPdiff)-30, NA, x)})
  gc()
  
  growing2 <- (stPhen$phenoSenescence2 - stPhen$phenoGreenup2)
  growing2 <- reclassify(growing2, c(365, Inf, 0))
  growingDays <- sum((stPhen$phenoSenescence1 - stPhen$phenoGreenup1), growing2, na.rm = T)
  growingDays <- reclassify(growingDays, c(300, Inf, 300))
  gc()
  
  writeRaster(growingDays, paste0(cropOutdir, "/croppingDays.tif"), overwrite = T)
  
  #stPhen <- mask(stPhen, aoi)
  rNonProtectedAreas <- resample(rNonProtectedAreas, stDMP[[1]], method = "ngb")
  #rNonProtectedAreas <- extend(rNonProtectedAreas, extent(stDMP[[1]]))
  rNonProtectedAreas <- crop(rNonProtectedAreas, extent(stDMP[[1]]))
  #rNonProtectedAreas <- mask(rNonProtectedAreas, aoi)
  print("past 0")
  
  names(stDMP) <- paste0("d", datesDMPdiff)
  
  stLU$LUtree300 <- reclassify(stLU$LUtree300, c(-Inf, 0, 0, 200, Inf, 0)) 
  stLU$LUtree300[is.na(stLU$LUtree300)] <- 0
  
  print("past 1")
  
  #####Estimate total DMP per ha
  #grazeForestFrac <- 0.1 #@Assume 10% of dry matter is extracted from forests for animal feed.
  grassFracDry <- 1# 0.33 #max 0.55
  grassFracWet <- 1# 0.55 #max 0.55
  browseShrubFrac <- 1 #0.38 #max 0.38
  browseForestFrac <- 1
  iResidueUtil <- 1 #max 0.6
  #iResidueUtil <- raster('SpatialData/inputs/SPAM2010cropspecies/crop_fed_frac.tif')
  iSPAMHarvestResidueFrac <- raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/SPAM2020/crop_res_frac.tif"))
  gc()
  
  #residueFrac <- calc(iSPAMHarvestResidueFrac, fun = function(x){x * iResidueUtil})
  #residueFrac <- overlay(iSPAMHarvestResidueFrac, iResidueUtil, fun = function(x, y){x * y})  #harvest index and utilised
  print("past overlay 1")
  
  #res1 <- res(residueFrac)[1]
  #res2 <- res(stDMP[[1]])[1]
  #residueFrac <- resample(iSPAMHarvestResidueFrac, stDMP[[1]], method = "near") 
  residueFrac <- iSPAMHarvestResidueFrac
  
  # residueFrac <- extend(residueFrac, extent(stDMP[[1]]))
  # residueFrac <- crop(residueFrac, extent(stDMP[[1]]))
  #residueFrac <- mask(residueFrac, aoi)
  
  #stLU$LUgrass300 <- sum(stLU$LUgrass300, stLU$LUshrub300, na.rm = T)
  
  #shrubFrac <- raster('SpatialData/inputs/TreeCover/treecover_imp.tif')/100
  shrubFrac <- raster(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/TreeCover/treecover300m.tif"))/100
  
  #shrubFrac[is.na(shrubFrac)] <- 0.13
  #shrubFrac <- reclassify(shrubFrac, c(NA, NA, 0.13), right = FALSE) 
  # shrubFrac <- resample(shrubFrac, stDMP[[1]], method = "ngb")
  # shrubFrac <- extend(shrubFrac, extent(stDMP[[1]]))
  # shrubFrac <- crop(shrubFrac, extent(stDMP[[1]]))
  gc()
  
  funGrowingGrassWet <- function(dmp, crops, grassShrub, forest, shrubFrac, greenup, senesence, greenup2, senesence2, nonprotected) {ifelse((greenup <= datesDMPdiff[i] & senesence >= datesDMPdiff[i]) | (greenup2 <= datesDMPdiff[i] & senesence2 >= datesDMPdiff[i]), (dmp*9*grassShrub*grassFracWet*(1-shrubFrac))+(dmp*9*forest*grassFracWet*(1-shrubFrac)*nonprotected), NA) } #@feedFrac is the proportion of crops grown that have feedable residues - i.e. excluding coffee, tea, ect.
  funGrowingGrassDry <- function(dmp, crops, grassShrub, forest,  shrubFrac, greenup, senesence, greenup2, senesence2, nonprotected) {ifelse((greenup > datesDMPdiff[i]) | (senesence < datesDMPdiff[i] & senesence + 60 > datesDMPdiff[i]) | (senesence2 < datesDMPdiff[i]), (dmp*9*grassShrub*grassFracDry*(1-shrubFrac))+(dmp*9*forest*grassFracDry*(1-shrubFrac)*nonprotected), NA) } #@feedFrac is the proportion of crops grown that have feedable residues - i.e. excluding coffee, tea, ect.
  funGrowingBrowse <- function(dmp, crops, grassShrub, forest,  shrubFrac, nonprotected) {(dmp*9*grassShrub*shrubFrac*browseShrubFrac)+(dmp*9*forest*nonprotected*shrubFrac*browseForestFrac)} 
  funGrowingCrops <- function(dmp, crops, greenup, senesence, feedFrac, resFrac, greenup2, senesence2) {ifelse((greenup <= datesDMPdiff[i] & senesence >= datesDMPdiff[i]) | (greenup2 <= datesDMPdiff[i] & senesence2 >= datesDMPdiff[i]), (dmp*9*crops), NA) } #@feedFrac is the proportion of crops grown that have feedable residues - i.e. excluding coffee, tea, ect.
  funGrowingAftermath <- function(dmp, crops, greenup, senesence, greenup2, senesence2, nonprotected) {ifelse((greenup > datesDMPdiff[i]) | (senesence < datesDMPdiff[i] & senesence + 60 > datesDMPdiff[i]) | (senesence2 < datesDMPdiff[i]), (dmp*9*crops), NA) } #@feedFrac is the proportion of crops grown that have feedable residues - i.e. excluding coffee, tea, ect.
  
  for(i in 1:length(names(stDMP))){
    
    iDMPGrassGrowing <- overlay(stDMP[[i]], stLU$LUcrops300, stLU$LUgrassShrub300, stLU$LUtree300, shrubFrac, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, rNonProtectedAreas, fun = funGrowingGrassWet)
    writeRaster(iDMPGrassGrowing, paste0(FeedQuantityOutdir, "/grassWetDMP", datesDMP[i], ".tif"), overwrite = TRUE)  
    rm(iDMPGrassGrowing)
    gc()
    
    iDMPGrassDry <- overlay(stDMP[[i]], stLU$LUcrops300, stLU$LUgrassShrub300, stLU$LUtree300, shrubFrac, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, rNonProtectedAreas, fun = funGrowingGrassDry)
    writeRaster(iDMPGrassDry, paste0(FeedQuantityOutdir, "/grassDryDMP", datesDMP[i], ".tif"), overwrite = TRUE)  
    rm(iDMPGrassDry)
    gc()
    
    iDMPBrowse <- overlay(stDMP[[i]], stLU$LUcrops300, stLU$LUgrassShrub300, stLU$LUtree300, shrubFrac, rNonProtectedAreas, fun = funGrowingBrowse)
    writeRaster(iDMPBrowse, paste0(FeedQuantityOutdir, "/browseDMP", datesDMP[i], ".tif"), overwrite = TRUE)  
    rm(iDMPBrowse)
    gc()
    
    iDMPCropGrowing <- overlay(stDMP[[i]], stLU$LUcrops300, stPhen$phenoGreenup1, stPhen$phenoSenescence1, iSPAMAnimalDigestCropFrac, residueFrac, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funGrowingCrops) 
    writeRaster(iDMPCropGrowing, paste0(FeedQuantityOutdir, "/cropDMP", datesDMP[i], ".tif"), overwrite = TRUE)
    rm(iDMPCropGrowing)
    gc()
    
    iDMPAftermath <- overlay(stDMP[[i]], stLU$LUcrops300, stPhen$phenoGreenup1, stPhen$phenoSenescence1, stPhen$phenoGreenup2, stPhen$phenoSenescence2, fun = funGrowingAftermath)
    writeRaster(iDMPAftermath, paste0(FeedQuantityOutdir, "/aftermathDMP", datesDMP[i], ".tif"), overwrite = TRUE)  
    rm(iDMPAftermath)
    gc()
    print(paste("cycle", i))
    
  }
  
  #rm(list = ls())
  gc()
  
  iDMPgrassWet <- stack(list.files(path = paste0(FeedQuantityOutdir), pattern="grassWet",full.names = T))
  DMPgrassmeanWet <- mean(iDMPgrassWet, na.rm = T)
  writeRaster(DMPgrassmeanWet, paste0(FeedQuantityMeansOutdir, "/DMPgrassWetmean_2014.tif"), overwrite = TRUE)
  
  iDMPgrassDry <- stack(list.files(path = paste0(FeedQuantityOutdir), pattern="grassDry",full.names = T))
  DMPgrassmeanDry <- mean(iDMPgrassDry, na.rm = T)
  writeRaster(DMPgrassmeanDry, paste0(FeedQuantityMeansOutdir, "/DMPgrassDrymean_2014.tif"), overwrite = TRUE)
  
  iDMPbrowse <- stack(list.files(path = paste0(FeedQuantityOutdir), pattern="browse",full.names = T))
  DMPbrowsemean <- mean(iDMPbrowse, na.rm = T)
  writeRaster(DMPbrowsemean, paste0(FeedQuantityMeansOutdir, "/DMPbrowsemean_2014.tif"), overwrite = TRUE)
  
  iDMPCropGrowing <- stack(list.files(path = paste0(FeedQuantityOutdir), pattern="crop",full.names = T))
  DMPcropmean <- mean(iDMPCropGrowing, na.rm = T)
  writeRaster(DMPcropmean, paste0(FeedQuantityMeansOutdir, "/DMPcropmean_2014.tif"), overwrite = TRUE)
  
  iDMPAftermath <- stack(list.files(path = paste0(FeedQuantityOutdir), pattern="aftermath",full.names = T))
  DMPAftermean <- mean(iDMPAftermath, na.rm = T)
  writeRaster(DMPAftermean, paste0(FeedQuantityMeansOutdir, "/DMPaftermean_2014.tif"), overwrite = TRUE)
  
  
}









