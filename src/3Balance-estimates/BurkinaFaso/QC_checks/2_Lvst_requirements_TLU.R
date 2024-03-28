.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(terra)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr) #For zonal statistics


##CSIRO, 2007, Nutrient Requirements of Domesticated Ruminants 
#https://vdocuments.net/nutrient-requirements-of-domesticated-ruminants.html?page=1

###Spatial data
##Add production system layer
systemMixedCrop <- raster("SpatialData/inputs/Lvst_system/w001001.adf") #1-4 = rangelands; 5-8 = mixed systems; 9-14 = irrigated

#Add level 3 admin boundaries
aoi1 <-  st_read('SpatialData/inputs/gadm40_BFA_1.shp')

livelihood <- rast('SpatialData/intermediate/livelihoodZones.tif')
lvCattle <- rast('SpatialData/inputs/GLW4/Ct_2015_10k.tif')
lvSheep <- rast('SpatialData/inputs/GLW4/Sh_2015_10k.tif')
lvGoat <- rast('SpatialData/inputs/GLW4/Gt_2015_0k.tif')

lv <- c(lvCattle, lvSheep, lvGoat) #Number per 10km pixel

livelihood <- terra::resample(livelihood, lv, method = 'near')
lv <- terra::crop(lv, livelihood)
lv <- c(lv, livelihood)

lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = 'SpatialData/inputs/GLW4/TLU.tif', overwrite = T)


###Admin level approach - comparing average daily requirements
outIntake_TLU_DM <- lapp(lvTLU, fun = function(TLU){((TLU)*(250*0.03))}, filename = 'SpatialData/inputs/tmp/outIntake.tif', overwrite = T) #, filename = 'D:/outIntake.tif', overwrite = T

#lvTLU <- raster('SpatialData/inputs/GLW4/TLU.tif')
#lvCattle <- raster('SpatialData/inputs/GLW4/Ct_2015_10k.tif')
#lvSheep <- raster('SpatialData/inputs/GLW4/Sh_2015_10k.tif')
#lvGoat <- raster('SpatialData/inputs/GLW4/Gt_2015_0k.tif')

#aoi1tlu <- exact_extract(lvTLU, aoi3, 'sum')
#aoi1cattle <- exact_extract(lvCattle, aoi3, 'sum')
#aoi1sheep <- exact_extract(lvSheep, aoi3, 'sum')
#aoi1goat <- exact_extract(lvGoat, aoi3, 'sum')


param_ME <- read.csv('LivestockParams/Livestock_energy_requirement.csv', stringsAsFactors = F)
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))

ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
EC <- 20 #energy content of the tissue=MJ/kg
#LLcow <- 210 #Lactation length
#LLsheep <- 135
#LLgoat <- 240
#MDMPcow <- 3.1 #MDMP = Mean Daily Milk Production
#MDMPshoat <- 0.6
#WS_days_ <- 110
#DS_days <- 255


#MERm <- K*S*M*((0.26*(MLW^0.75) *exp(-0.03*Age))/((0.02*M.D)+0.5))
#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#MERl <- (DMY*ECM)/((0.02*M.D)+0.04)
#Positive
#MERg=(TMY*0.92*EC)/(0.043*M.D)
#Negative
#MERg=(TMY*0.92*EC)/(0.8)

#livelihood == 8 #sah

##Intermediate MER calculations
#################
#Sahelian zone
#MERm Sah cattle 
MERm_WS_Sah_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

MERm_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"])+0.5)))

#dry season
MERm_DS_Sah_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))

MERm_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Sah_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

MERt_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"] * 0.0026)

#Dry season
MERt_DS_Sah_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)

MERt_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

MERl_WS_Sah_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

MERl_WS_Sah_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
   +0.04)

#Dry season
MERl_DS_Sah_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)

MERl_DS_Sah_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)

MERl_DS_Sah_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"])
   +0.04)


#Pregnancy
#MEp = (BCexp(-Ct)*SBW*exp(A - B*exp(-Ct)))/0.133 #(Eq. 1.25 & 1.26)
#Cows
t <- 280 - (280-(12*7)) #last 12 weeks# #days since gravid uterus = 22
A <- 349.22 #Table 1.9 - undefined
B <- 349.16 #Table 1.9 - undefined
C <- (5.76*10^-5) #Table 1.9 - undefined
SBW <- 1 # Ratio of expected birth weight
Y <- (SBW*exp(A - B*exp(-C*t))) #Eq. 1.26
MERp_cow_daily_term <- B * C * exp(-C*t) * Y / 0.133 #Eq. 1.25. Daily requirement at one point in time - at term

t <- (280-(12*7)):280 #last 12 weeks #22:280
MEcow <- function(x){B * C * exp(-C*t) * (SBW*exp(A - B*exp(-C*t))) / 0.133}
y <- MEcow(t)
MERp_cow_fullPreg <- sum(y)
#f <- function(t,y) approxfun(y)(t)
#MERp_cow_fullPreg <- integrate(f,min(t),max(t),y)

#Shoats
t <- 147 - (147-(12*7)) #last 12 weeks #days since gravid uterus = 12 DOI: 10.1530/rep.1.00398 https://pubmed.ncbi.nlm.nih.gov/15579583/
A <- 7.64 #Table 1.9 - undefined
B <- 11.46 #Table 1.9 - undefined
C <- (6.43*10^-3) #Table 1.9 - undefined
SBW <- 1 # Ratio of expected birth weight
Y <- (SBW*exp(A - B*exp(-C*t))) #Eq. 1.26
MERp_shoat_daily_term <- ((B * C * exp(-C*t)*Y)/0.133) #Eq. 1.25. Daily requirement at one point in time - at term

t <- (147-(12*7)):147
MEshoat <- function(x){B * C * exp(-C*t) * (SBW*exp(A - B*exp(-C*t))) / 0.133}
y <- MEshoat(t)
MERp_shoat_fullPreg  <- sum(y)
#MERp_shoat_fullPreg <- integrate(f,min(t),max(t),y)

#MERg
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Sah_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / (0.8))

MERg_WS_Sah_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / (0.8))

MERg_WS_Sah_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_WS_Sah_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 0.8)

MERg_WS_Sah_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_WS_Sah_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_WS_Sah_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 0.8)

MERg_WS_Sah_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_WS_Sah_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

#Dry season
MERg_DS_Sah_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / (0.8))

MERg_DS_Sah_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / (0.8))

MERg_DS_Sah_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_DS_Sah_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                                0.92*EC) / 0.8)

MERg_DS_Sah_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_DS_Sah_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)

MERg_DS_Sah_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                               0.92*EC) / 0.8)

MERg_DS_Sah_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERg_DS_Sah_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sah"]*
                              0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Sah_cow <- (MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERl_WS_Sah_cow + MERt_WS_Sah_cow) * 110
#MERtotal_DS_Sah_cow <- (MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERl_DS_Sah_cow + MERt_DS_Sah_cow) * 228
MERtotal_WS_Sah_bull <- (MERm_WS_Sah_bull + MERg_WS_Sah_bull + MERt_WS_Sah_bull)
MERtotal_WS_Sah_steer <- (MERm_WS_Sah_steer + MERg_WS_Sah_steer + MERt_WS_Sah_steer)
MERtotal_WS_Sah_calf <- (MERm_WS_Sah_calf + MERg_WS_Sah_calf + MERt_WS_Sah_calf)
MERtotal_WS_Sah_heifer <- (MERm_WS_Sah_heifer + MERg_WS_Sah_heifer + MERt_WS_Sah_heifer)
#MERtotal_WS_Sah_cow <- (MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERl_WS_Sah_cow + MERt_WS_Sah_cow)
MERtotal_WS_Sah_lamb <- (MERm_WS_Sah_lamb + MERg_WS_Sah_lamb + MERt_WS_Sah_lamb)
#MERtotal_WS_Sah_sheep <- (MERm_WS_Sah_sheep + MERg_WS_Sah_sheep + MERl_WS_Sah_sheep + MERt_WS_Sah_sheep)
MERtotal_WS_Sah_kid <- (MERm_WS_Sah_kid + MERg_WS_Sah_kid + MERt_WS_Sah_kid)
#MERtotal_WS_Sah_goat <- (MERm_WS_Sah_goat + MERg_WS_Sah_goat + MERl_WS_Sah_goat + MERt_WS_Sah_goat)

MERtotal_DS_Sah_bull <- (MERm_DS_Sah_bull + MERg_DS_Sah_bull + MERt_DS_Sah_bull)
MERtotal_DS_Sah_steer <- (MERm_DS_Sah_steer + MERg_DS_Sah_steer + MERt_DS_Sah_steer)
MERtotal_DS_Sah_calf <- (MERm_DS_Sah_calf + MERg_DS_Sah_calf + MERt_DS_Sah_calf)
MERtotal_DS_Sah_heifer <- (MERm_DS_Sah_heifer + MERg_DS_Sah_heifer + MERt_DS_Sah_heifer)
#MERtotal_DS_Sah_cow <- (MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERl_DS_Sah_cow + MERt_DS_Sah_cow)
MERtotal_DS_Sah_lamb <- (MERm_DS_Sah_lamb + MERg_DS_Sah_lamb + MERt_DS_Sah_lamb)
#MERtotal_DS_Sah_sheep <- (MERm_DS_Sah_sheep + MERg_DS_Sah_sheep + MERl_DS_Sah_sheep + MERt_DS_Sah_sheep)
MERtotal_DS_Sah_kid <- (MERm_DS_Sah_kid + MERg_DS_Sah_kid + MERt_DS_Sah_kid)
#MERtotal_DS_Sah_goat <- (MERm_DS_Sah_goat + MERg_DS_Sah_goat + MERl_DS_Sah_goat + MERt_DS_Sah_goat)

##Total pop requirement calcs
MERtotalYr_WS_Sah_cattle <- lapp(lv[[c(1,4)]], fun = function(cattle, livelihood) { 
  ifelse(livelihood ==8, #8 is the Sahel
         (MERtotal_WS_Sah_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sah"]*
            param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Bull" & param_ME$Region == "Sah"]) +
           (MERtotal_WS_Sah_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Steer" & param_ME$Region == "Sah"]) +
           (MERtotal_WS_Sah_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Calf" & param_ME$Region == "Sah"]) +
           (MERtotal_WS_Sah_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]) +
           ((MERm_WS_Sah_cow + MERg_WS_Sah_cow + MERt_WS_Sah_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"]) +
           (MERl_WS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Sah"
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Sah_shoats <- lapp(lv[[c(2,3,4)]], fun = function(sheep, goats, livelihood) { 
  ifelse(livelihood ==8, #8 is the Sahel
         (MERtotal_WS_Sah_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]*
            param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]) +
           ((MERm_WS_Sah_sheep + MERg_WS_Sah_sheep + MERt_WS_Sah_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]) +
           (MERl_WS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Sah_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Kid" & param_ME$Region == "Sah"]) +
           ((MERm_WS_Sah_goat + MERg_WS_Sah_goat + MERt_WS_Sah_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"]) +
           (MERl_WS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Sah_cattle <- lapp(lv[[c(1,4)]], fun = function(cattle, livelihood) { 
  ifelse(livelihood ==8, #8 is the Sahel
         (MERtotal_DS_Sah_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sah"]*
            param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Bull" & param_ME$Region == "Sah"]) +
           (MERtotal_DS_Sah_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Steer" & param_ME$Region == "Sah"]) +
           (MERtotal_DS_Sah_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Calf" & param_ME$Region == "Sah"]) +
           (MERtotal_DS_Sah_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Heifer" & param_ME$Region == "Sah"]) +
           ((MERm_DS_Sah_cow + MERg_DS_Sah_cow + MERt_DS_Sah_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"]) +
           (MERl_DS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Sah_shoats <- lapp(lv[[c(2,3,4)]], fun = function(sheep, goats, livelihood) { 
  ifelse(livelihood ==8, #8 is the Sahel
         (MERtotal_DS_Sah_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]*
            param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Lamb" & param_ME$Region == "Sah"]) +
           ((MERm_DS_Sah_sheep + MERg_DS_Sah_sheep + MERt_DS_Sah_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]) +
           (MERl_DS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Sah_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Kid" & param_ME$Region == "Sah"]) +
           ((MERm_DS_Sah_goat + MERg_DS_Sah_goat + MERt_DS_Sah_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"]) +
           (MERl_DS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})


#######
#Sudanian zone  
#MERm Sud cattle

MERm_WS_Sud_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

MERm_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"])+0.5)))

#dry season
MERm_DS_Sud_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))

MERm_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Sud_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

MERt_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"] * 0.0026)

#Dry season
MERt_DS_Sud_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)

MERt_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Sud_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Sud_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

MERl_WS_Sud_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"])
   +0.04)

#Dry season
MERl_DS_Sud_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Sud_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

MERl_DS_Sud_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"])
   +0.04)

#MERg #!x10 greater than Rahimi et al.
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Sud_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / (0.8))

MERg_WS_Sud_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / (0.8))

MERg_WS_Sud_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_WS_Sud_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 0.8)

MERg_WS_Sud_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_WS_Sud_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_WS_Sud_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 0.8)

MERg_WS_Sud_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_WS_Sud_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

#Dry season
MERg_DS_Sud_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / (0.8))

MERg_DS_Sud_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / (0.8))

MERg_DS_Sud_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_DS_Sud_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                                0.92*EC) / 0.8)

MERg_DS_Sud_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_DS_Sud_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)

MERg_DS_Sud_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                               0.92*EC) / 0.8)

MERg_DS_Sud_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                             0.92*EC) / 0.8)

MERg_DS_Sud_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Sud"]*
                              0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Sud_cow <- (MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERl_WS_Sud_cow + MERt_WS_Sud_cow) * 110
#MERtotal_DS_Sud_cow <- (MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERl_DS_Sud_cow + MERt_DS_Sud_cow) * 228
MERtotal_WS_Sud_bull <- (MERm_WS_Sud_bull + MERg_WS_Sud_bull + MERt_WS_Sud_bull)
MERtotal_WS_Sud_steer <- (MERm_WS_Sud_steer + MERg_WS_Sud_steer + MERt_WS_Sud_steer)
MERtotal_WS_Sud_calf <- (MERm_WS_Sud_calf + MERg_WS_Sud_calf + MERt_WS_Sud_calf)
MERtotal_WS_Sud_heifer <- (MERm_WS_Sud_heifer + MERg_WS_Sud_heifer + MERt_WS_Sud_heifer)
#MERtotal_WS_Sud_cow <- (MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERl_WS_Sud_cow + MERt_WS_Sud_cow)
MERtotal_WS_Sud_lamb <- (MERm_WS_Sud_lamb + MERg_WS_Sud_lamb + MERt_WS_Sud_lamb)
#MERtotal_WS_Sud_sheep <- (MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERl_WS_Sud_sheep + MERt_WS_Sud_sheep)
MERtotal_WS_Sud_kid <- (MERm_WS_Sud_kid + MERg_WS_Sud_kid + MERt_WS_Sud_kid)
#MERtotal_WS_Sud_goat <- (MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERl_WS_Sud_goat + MERt_WS_Sud_goat)

MERtotal_DS_Sud_bull <- (MERm_DS_Sud_bull + MERg_DS_Sud_bull + MERt_DS_Sud_bull)
MERtotal_DS_Sud_steer <- (MERm_DS_Sud_steer + MERg_DS_Sud_steer + MERt_DS_Sud_steer)
MERtotal_DS_Sud_calf <- (MERm_DS_Sud_calf + MERg_DS_Sud_calf + MERt_DS_Sud_calf)
MERtotal_DS_Sud_heifer <- (MERm_DS_Sud_heifer + MERg_DS_Sud_heifer + MERt_DS_Sud_heifer)
#MERtotal_DS_Sud_cow <- (MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERl_DS_Sud_cow + MERt_DS_Sud_cow)
MERtotal_DS_Sud_lamb <- (MERm_DS_Sud_lamb + MERg_DS_Sud_lamb + MERt_DS_Sud_lamb)
#MERtotal_DS_Sud_sheep <- (MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERl_DS_Sud_sheep + MERt_DS_Sud_sheep)
MERtotal_DS_Sud_kid <- (MERm_DS_Sud_kid + MERg_DS_Sud_kid + MERt_DS_Sud_kid)
#MERtotal_DS_Sud_goat <- (MERm_DS_Sud_goat + MERg_DS_Sud_goat + MERl_DS_Sud_goat + MERt_DS_Sud_goat)

##Total pop requirement calcs
MERtotalYr_WS_Sud_cattle <- lapp(lv[[c(1,4)]], fun = function(cattle, livelihood) { #!Including pregnancy
  ifelse(livelihood !=8, #8 is the Sahel
         (MERtotal_WS_Sud_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sud"]*
            param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Bull" & param_ME$Region == "Sud"]) +
           (MERtotal_WS_Sud_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Steer" & param_ME$Region == "Sud"]) +
           (MERtotal_WS_Sud_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Calf" & param_ME$Region == "Sud"]) +
           (MERtotal_WS_Sud_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]) +
           ((MERm_WS_Sud_cow + MERg_WS_Sud_cow + MERt_WS_Sud_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"]) +
           (MERl_WS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Sud_shoats <- lapp(lv[[c(2,3,4)]], fun = function(sheep, goats, livelihood) { #! including pregnancy
  ifelse(livelihood !=8, #8 is the Sahel
         (MERtotal_WS_Sud_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]*
            param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]) +
           ((MERm_WS_Sud_sheep + MERg_WS_Sud_sheep + MERt_WS_Sud_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]) +
           (MERl_WS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Sud_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Kid" & param_ME$Region == "Sud"]) +
           ((MERm_WS_Sud_goat + MERg_WS_Sud_goat + MERt_WS_Sud_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"]) +
           (MERl_WS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Sud"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Sud_cattle <- lapp(lv[[c(1,4)]], fun = function(cattle, livelihood) { 
  ifelse(livelihood !=8, #8 is the Sahel
         (MERtotal_DS_Sud_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Sud"]*
            param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Bull" & param_ME$Region == "Sud"]) +
           (MERtotal_DS_Sud_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Steer" & param_ME$Region == "Sud"]) +
           (MERtotal_DS_Sud_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Calf" & param_ME$Region == "Sud"]) +
           (MERtotal_DS_Sud_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Heifer" & param_ME$Region == "Sud"]) +
           ((MERm_DS_Sud_cow + MERg_DS_Sud_cow + MERt_DS_Sud_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"]) +
           (MERl_DS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Sud_shoats <- lapp(lv[[c(2,3,4)]], fun = function(sheep, goats, livelihood) { 
  ifelse(livelihood !=8, #8 is the Sahel
         (MERtotal_DS_Sud_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]*
            param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Lamb" & param_ME$Region == "Sud"]) +
           ((MERm_DS_Sud_sheep + MERg_DS_Sud_sheep + MERt_DS_Sud_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]) +
           (MERl_DS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Sud_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Kid" & param_ME$Region == "Sud"]) +
           ((MERm_DS_Sud_goat + MERg_DS_Sud_goat + MERt_DS_Sud_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"]) +
           (MERl_DS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

##################
###TLU approach - comparing average daily requirements
#Assume 8Mj ME per kg of liveweight. Assume 8 Mj ME and 10% DM for every litre of milk 
lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = 'SpatialData/inputs/GLW4/TLU.tif', overwrite = T)
lvstIntake_TLU_MJ <- lapp(lvTLU, fun = function(TLU){((TLU)*(250 * 0.02 * 8 * 365))}, filename = 'SpatialData/outputs/Lvst_intake_TLU_MJ_2015.tif', overwrite = T) #, filename = 'D:/outIntake.tif', overwrite = T
lactationIntake_TLU_MJ <- lapp(lv[[1:4]], fun = function(cattle, sheep, goats, livelihood) { 
  (8 * 0.1 * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
     param_ME$value[param_ME$Variable=="TMY" & param_ME$name == "Cow" & param_ME$Region == "Sah"])+
    (8 * 0.1 * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
       param_ME$value[param_ME$Variable=="TMY" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]) + # & param_ME$Region == "Sah"]
    (8 * 0.1 * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
       param_ME$value[param_ME$Variable=="TMY" & param_ME$name == "Goat" & param_ME$Region == "Sah"])
}, filename = 'SpatialData/outputs/Lactation_intake_TLU_MJ_2015.tif', overwrite = T)

#Alt calculation
lactationIntake_model_MJ <- lapp(lv[[1:4]], fun = function(cattle, sheep, goats, livelihood) { 
  ifelse(livelihood ==8, #8 is the Sahel #Base on WS yield for simplicity
         ((MERl_WS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/2)+
           (MERl_WS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/2) + # & param_ME$Region == "Sah"]
           (MERl_WS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/2)+# & param_ME$Region == "Sah"
           (MERl_DS_Sah_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/2)+
           (MERl_DS_Sah_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/2) + # & param_ME$Region == "Sah"]
           (MERl_DS_Sah_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sah"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/2)
            ),
           ((MERl_WS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/2) +
           (MERl_WS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/2) + # & param_ME$Region == "Sud"]
           (MERl_WS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Sud"
           (MERl_DS_Sud_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/2) +
           (MERl_DS_Sud_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]/2) + # & param_ME$Region == "Sud"]
           (MERl_DS_Sud_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Sud"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]/2) 
         ))#Close if
}, filename = 'SpatialData/outputs/Lactation_intake_model_MJ_2015.tif', overwrite = T)


##############
#Total requirements in MJ
#MERall <- sum(eval(parse(text = ls(pattern = "MERtotalYr"))), na.rm = T)
MERcattle <- sum(MERtotalYr_DS_Sah_cattle, MERtotalYr_DS_Sud_cattle, MERtotalYr_WS_Sah_cattle, MERtotalYr_WS_Sud_cattle, na.rm = T)
terra::writeRaster(MERcattle, 'SpatialData/outputs/cattleMER_MJ_2015.tif', overwrite = T)

MERshoats <- sum(MERtotalYr_DS_Sah_shoats, MERtotalYr_DS_Sud_shoats, MERtotalYr_WS_Sah_shoats, MERtotalYr_WS_Sud_shoats, na.rm = T)
terra::writeRaster(MERshoats, 'SpatialData/outputs/shoatsMER_MJ_2015.tif', overwrite = T)

MERall <- sum(MERtotalYr_DS_Sah_cattle, MERtotalYr_DS_Sah_shoats, MERtotalYr_DS_Sud_cattle, MERtotalYr_DS_Sud_shoats, MERtotalYr_WS_Sah_cattle, MERtotalYr_WS_Sah_shoats, MERtotalYr_WS_Sud_cattle, MERtotalYr_WS_Sud_shoats, na.rm = T)
terra::writeRaster(MERall, 'SpatialData/outputs/livestockMER_MJ_2015.tif', overwrite = T)


###Check: comparison with Rahimi et al., 2021
#MERall <- raster('SpatialData/outputs/livestockMER_MJ_2015.tif')
#Feed_Tj_Demand <- raster('AltAnalyses/Rahimi Demand_Tj ME/avg_demand/w001001.adf')
#Feed_Mj_Demand <- Feed_Tj_Demand*1000000
##Feed_Mj_Demand <- terra::crop(Feed_Mj_Demand, MERall)
##Feed_Mj_Demand <- terra::resample(Feed_Mj_Demand, MERall)

#aoi1$Feed_MjIntake <- exact_extract(Feed_Mj_Demand, aoi1, 'sum')
#aoi1$MERall <- exact_extract(MERall, aoi1, 'sum')
#
#aoi1$diff <- aoi1$Feed_MjIntake - aoi1$MERall #(aoi1$MERall0)
#hist(aoi1$diff)
