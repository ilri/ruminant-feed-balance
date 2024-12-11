library(dplyr)
library(raster)
library(sf)
library(exactextractr)

elevation <- raster('SpatialData/inputs/Elevation/elev1250_seived.tif')
#Add admin boundaries
sf_use_s2(FALSE)
aoi1 <-  st_read('SpatialData/inputs/ET_LHZ_2018.shp')
aoi1$elev <- exact_extract(elevation, aoi1, "mean")
#aoi1$grouping <- ifelse(grepl("astoral", aoi1$LZTYPE), "Lowland", "Highland")
aoi1$grouping <- ifelse(grepl("astoral", aoi1$LZTYPE) & aoi1$elev < 0.5, "Lowland (agro)pastoral", 
                        ifelse(grepl("astoral", aoi1$LZTYPE) & aoi1$elev >= 0.5, "Highland (agro)pastoral", 
                               ifelse(!grepl("astoral", aoi1$LZTYPE) & aoi1$elev >= 0.5, "Highland mixed", "Lowland mixed")))
aoi1 <- aoi1 %>%  group_by(grouping) %>% summarise() #%>% ungroup()

pathLU <- 'SpatialData/inputs/Land_use/'
filesLU <- list.files(path = pathLU, pattern = ".tif$", full.names = T)

pathSPAM <- 'SpatialData/inputs/SPAM2017/'
#From SPAM documentation: *_TA	all technologies together, ie complete crop; *_TI	irrigated portion of crop; *_TH	rainfed high inputs portion of crop; *_TL	rainfed low inputs portion of crop; *_TS	rainfed subsistence portion of crop; *_TR	rainfed portion of crop (= TA - TI, or TH + TL + TS)
#end of file name should be physical area_cropname_a -> last a standing for all tech together.
filesSPAM <- list.files(path = pathSPAM, pattern = "_A_clip.tif$", full.names = T)

iCopernicustotalArea <- raster('SpatialData/inputs/Land_use/LUcrops300.tif') #In hectares
stSPAM <- stack(filesSPAM) #In hectares https://www.mapspam.info/methodology/
iDEAtotalArea <- raster('SpatialData/inputs/Land_use/DEA_crop_Ethiopia_20_reproj_clip.tif') #In 20m2

#iDEAtotalArea <- iDEAtotalArea * 0.04 #400 m2 (20x20m) to ha

#Harmonised land area calculation
#iCopernicustotalArea <- overlay(stLU, fun = sum)
iSPAMtotalArea <- overlay(stSPAM, fun = sum)

iCopernicustotalArea <- mask(iCopernicustotalArea, aoi1)
iDEAtotalArea <- mask(iDEAtotalArea, aoi1)
iSPAMtotalArea <- mask(iSPAMtotalArea, aoi1)

aoi1$CropCope <- exact_extract(iCopernicustotalArea, aoi1, "sum")
aoi1$CropDEA <- exact_extract(iDEAtotalArea, aoi1, "sum")
aoi1$CropSPAM <- exact_extract(iSPAMtotalArea, aoi1, "sum")


##Total crop area comparison - bar chart with horizontal bar at total area
#ESRI crops - 190000 km2
#ESA crops - 220000 km2
#DEA crops - 250000 km2
cellStats(iCopernicustotalArea*0.01, sum) #Ha to km2. 22000 km2
cellStats(iDEAtotalArea*0.04*0.01, sum) # #20x20m to ha. Ha to km2
cellStats(iSPAMtotalArea*0.01, sum) #Ha to km2. 122000 km2

##Crop biomass productivity estimates from us and other sources


##Grass biomass productivity estimates - comparing ours with other sources, using different area estimates


iCopernicustotalArea <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300.tif') #In hectares
DEA300 <- raster('SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA_unproj.tif') #In 20m2
DEA300 <- projectRaster(DEA300, iCopernicustotalArea)
DEA300 <- resample(DEA300, iCopernicustotalArea, method="ngb")
writeRaster(DEA300, 'SpatialData/inputs/Feed_DrySeason/LandUse/LUcrops300DEA.tif')
