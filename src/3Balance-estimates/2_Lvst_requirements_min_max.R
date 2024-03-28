#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(terra)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr) #For zonal statistics


##CSIRO, 2007, Nutrient Requirements of Domesticated Ruminants 
#https://vdocuments.net/nutrient-requirements-of-domesticated-ruminants.html?page=1
param_ME <- read.csv('LivestockParams/Livestock_energy_requirement_min.csv', stringsAsFactors = F)
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))
ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
EC <- 20 #energy content of the tissue=MJ/kg

###Spatial data
#Add level 3 admin boundaries
aoi1 <-  st_read('SpatialData/inputs/aoi1.gpkg')

lvCattle <- rast('SpatialData/inputs/GLW4/Ct_2015_10k.tif')
lvSheep <- rast('SpatialData/inputs/GLW4/Sh_2015_10k.tif')
lvGoat <- rast('SpatialData/inputs/GLW4/Gt_2015_10k.tif')
lvHorse <- rast('SpatialData/inputs/GLW4/Ho_2015_10k.tif')
lvCattle <- terra::crop(lvCattle, aoi1, mask = T)
lvSheep <- terra::crop(lvSheep, aoi1, mask = T)
lvGoat <- terra::crop(lvGoat, aoi1, mask = T)
lvHorse <- terra::crop(lvHorse, aoi1, mask = T)


lv <- c(lvCattle, lvSheep, lvGoat) #Number per 10km pixel

lvHorse <- terra::crop(lvHorse, aoi1, mask = T)

r <- init(lvCattle, NA)
nc <- vect('SpatialData/inputs/aoi1.gpkg')

r.ext <- terra::extract(r, nc)
r.ext <- group_by(r.ext, ID)
nc$cells <- summarise(r.ext, fun = n())$fun
nc$donkey_cell <- nc$Donkeys / nc$cells

#Calculate ME for maintenance and work. Assuming a weight of 275 in Afar and 400 elsewhere (FAO, 2018, Report on feed inventory and feed balance). 
#Work was assumed to be 40% of maintenance, where a little over 15% of camels were estimated to work (CSA, 2022)
nc$camelME_cell <- ifelse(nc$NAME_1 == "Afar", ((nc$Camels*0.435*(275^0.75)*365) + (nc$Camels*0.435*(275^0.75)*180)*0.1548*0.4) / nc$cells, ((nc$Camels*0.435*(400^0.75)*365) + (nc$Camels*0.435*(400^0.75)*180)*0.1548*0.4) / nc$cells)

lvDonkey <- rasterize(nc, r, field = "donkey_cell")
MERcamel <- rasterize(nc, r, field = "camelME_cell")


##Add production system and livelihood layers
livelihood <- rast('SpatialData/inputs/Elevation/elev1500.tif')
livelihood <- terra::crop(livelihood, aoi1, mask = T)
livelihood <- terra::resample(livelihood, lv, method = 'near')
systemMixedCrop <- rast("SpatialData/inputs/Lvst_system/w001001.adf") #1-4 = rangelands; 5-8 = mixed systems; 9-14 = irrigated
systemMixedCrop <- terra::resample(systemMixedCrop, lv, method = 'near')
systemMixedCrop <- terra::crop(systemMixedCrop, livelihood, mask = T)
periurban <- systemMixedCrop == 13
lv <- c(lv, livelihood)
lv <- c(lv, periurban)

lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = 'SpatialData/inputs/GLW4/TLU.tif', overwrite = T)

#Add season data
wetSSN <- rast('cropParams/croppingDays.tif')
wetSSN <- terra::crop(wetSSN, aoi1, mask = T)
wetSSN <- terra::resample(wetSSN, lv, method = 'near')
wetSSN[is.na(wetSSN)] <- global(wetSSN, 'mean', na.rm = T) #Fill NAs and 0s with global mean
wetSSN[wetSSN ==0] <- global(wetSSN, 'mean', na.rm = T)
drySSN <- 365 - wetSSN
lv <- c(lv, wetSSN)
lv <- c(lv, drySSN)


##Intermediate MER calculations
#################
#Sahelian zone
#MERm Sah cattle 
MERm_WS_Lowland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

#dry season
MERm_DS_Lowland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Lowland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

#Dry season
MERt_DS_Lowland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

#Dry season
MERl_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
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


MERg_WS_Lowland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / (0.8))

MERg_WS_Lowland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / (0.8))

MERg_WS_Lowland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Lowland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 0.8)

MERg_WS_Lowland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_WS_Lowland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Lowland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Lowland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_WS_Lowland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >=0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

#Dry season
MERg_DS_Lowland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / (0.8))

MERg_DS_Lowland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / (0.8))

MERg_DS_Lowland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Lowland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 0.8)

MERg_DS_Lowland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_DS_Lowland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Lowland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Lowland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_DS_Lowland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >=0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Lowland_cow <- (MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERl_WS_Lowland_cow + MERt_WS_Lowland_cow) * 110
#MERtotal_DS_Lowland_cow <- (MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERl_DS_Lowland_cow + MERt_DS_Lowland_cow) * 228
MERtotal_WS_Lowland_bull <- (MERm_WS_Lowland_bull + MERg_WS_Lowland_bull + MERt_WS_Lowland_bull)
MERtotal_WS_Lowland_steer <- (MERm_WS_Lowland_steer + MERg_WS_Lowland_steer + MERt_WS_Lowland_steer)
MERtotal_WS_Lowland_calf <- (MERm_WS_Lowland_calf + MERg_WS_Lowland_calf + MERt_WS_Lowland_calf)
MERtotal_WS_Lowland_heifer <- (MERm_WS_Lowland_heifer + MERg_WS_Lowland_heifer + MERt_WS_Lowland_heifer)
#MERtotal_WS_Lowland_cow <- (MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERl_WS_Lowland_cow + MERt_WS_Lowland_cow)
MERtotal_WS_Lowland_lamb <- (MERm_WS_Lowland_lamb + MERg_WS_Lowland_lamb + MERt_WS_Lowland_lamb)
#MERtotal_WS_Lowland_sheep <- (MERm_WS_Lowland_sheep + MERg_WS_Lowland_sheep + MERl_WS_Lowland_sheep + MERt_WS_Lowland_sheep)
MERtotal_WS_Lowland_kid <- (MERm_WS_Lowland_kid + MERg_WS_Lowland_kid + MERt_WS_Lowland_kid)
#MERtotal_WS_Lowland_goat <- (MERm_WS_Lowland_goat + MERg_WS_Lowland_goat + MERl_WS_Lowland_goat + MERt_WS_Lowland_goat)

MERtotal_DS_Lowland_bull <- (MERm_DS_Lowland_bull + MERg_DS_Lowland_bull + MERt_DS_Lowland_bull)
MERtotal_DS_Lowland_steer <- (MERm_DS_Lowland_steer + MERg_DS_Lowland_steer + MERt_DS_Lowland_steer)
MERtotal_DS_Lowland_calf <- (MERm_DS_Lowland_calf + MERg_DS_Lowland_calf + MERt_DS_Lowland_calf)
MERtotal_DS_Lowland_heifer <- (MERm_DS_Lowland_heifer + MERg_DS_Lowland_heifer + MERt_DS_Lowland_heifer)
#MERtotal_DS_Lowland_cow <- (MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERl_DS_Lowland_cow + MERt_DS_Lowland_cow)
MERtotal_DS_Lowland_lamb <- (MERm_DS_Lowland_lamb + MERg_DS_Lowland_lamb + MERt_DS_Lowland_lamb)
#MERtotal_DS_Lowland_sheep <- (MERm_DS_Lowland_sheep + MERg_DS_Lowland_sheep + MERl_DS_Lowland_sheep + MERt_DS_Lowland_sheep)
MERtotal_DS_Lowland_kid <- (MERm_DS_Lowland_kid + MERg_DS_Lowland_kid + MERt_DS_Lowland_kid)
#MERtotal_DS_Lowland_goat <- (MERm_DS_Lowland_goat + MERg_DS_Lowland_goat + MERl_DS_Lowland_goat + MERt_DS_Lowland_goat)

##Total pop requirement calcs
MERtotalYr_WS_Lowland_cattle <- lapp(lv[[c(1,4,6)]], fun = function(cattle, livelihood, wetSSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_WS_Lowland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Lowland"]*
            wetSSN) +
           (MERtotal_WS_Lowland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Lowland"]*
             wetSSN) +
           (MERtotal_WS_Lowland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERtotal_WS_Lowland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Lowland"]*
              wetSSN) +
           ((MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERt_WS_Lowland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Lowland"
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Lowland_shoats <- lapp(lv[[c(2,3,4,6)]], fun = function(sheep, goats, livelihood, wetSSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_WS_Lowland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Lowland"]*
            wetSSN) +
           ((MERm_WS_Lowland_sheep + MERg_WS_Lowland_sheep + MERt_WS_Lowland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Lowland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Lowland"]*
              wetSSN) +
           ((MERm_WS_Lowland_goat + MERg_WS_Lowland_goat + MERt_WS_Lowland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Lowland_cattle <- lapp(lv[[c(1,4,7)]], fun = function(cattle, livelihood, drySSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_DS_Lowland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Lowland"]*
            drySSN) +
           (MERtotal_DS_Lowland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERtotal_DS_Lowland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERtotal_DS_Lowland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Lowland"]*
              drySSN) +
           ((MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERt_DS_Lowland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Lowland_shoats <- lapp(lv[[c(2,3,4,7)]], fun = function(sheep, goats, livelihood, drySSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_DS_Lowland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Lowland"]*
            drySSN) +
           ((MERm_DS_Lowland_sheep + MERg_DS_Lowland_sheep + MERt_DS_Lowland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Lowland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Lowland"]*
              drySSN) +
           ((MERm_DS_Lowland_goat + MERg_DS_Lowland_goat + MERt_DS_Lowland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})


#######
#Highland zone  
#MERm Sud cattle

MERm_WS_Highland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

#dry season
MERm_DS_Highland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Highland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

#Dry season
MERt_DS_Highland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

MERl_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

MERl_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

#Dry season
MERl_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

MERl_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

MERl_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

#MERg #!x10 greater than Rahimi et al.
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Highland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / (0.8))

MERg_WS_Highland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / (0.8))

MERg_WS_Highland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Highland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 0.8)

MERg_WS_Highland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Highland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Highland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 0.8)

MERg_WS_Highland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Highland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

#Dry season
MERg_DS_Highland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / (0.8))

MERg_DS_Highland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / (0.8))

MERg_DS_Highland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Highland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 0.8)

MERg_DS_Highland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Highland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Highland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 0.8)

MERg_DS_Highland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Highland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Highland_cow <- (MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERl_WS_Highland_cow + MERt_WS_Highland_cow) * 110
#MERtotal_DS_Highland_cow <- (MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERl_DS_Highland_cow + MERt_DS_Highland_cow) * 228
MERtotal_WS_Highland_bull <- (MERm_WS_Highland_bull + MERg_WS_Highland_bull + MERt_WS_Highland_bull)
MERtotal_WS_Highland_steer <- (MERm_WS_Highland_steer + MERg_WS_Highland_steer + MERt_WS_Highland_steer)
MERtotal_WS_Highland_calf <- (MERm_WS_Highland_calf + MERg_WS_Highland_calf + MERt_WS_Highland_calf)
MERtotal_WS_Highland_heifer <- (MERm_WS_Highland_heifer + MERg_WS_Highland_heifer + MERt_WS_Highland_heifer)
#MERtotal_WS_Highland_cow <- (MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERl_WS_Highland_cow + MERt_WS_Highland_cow)
MERtotal_WS_Highland_lamb <- (MERm_WS_Highland_lamb + MERg_WS_Highland_lamb + MERt_WS_Highland_lamb)
#MERtotal_WS_Highland_sheep <- (MERm_WS_Highland_sheep + MERg_WS_Highland_sheep + MERl_WS_Highland_sheep + MERt_WS_Highland_sheep)
MERtotal_WS_Highland_kid <- (MERm_WS_Highland_kid + MERg_WS_Highland_kid + MERt_WS_Highland_kid)
#MERtotal_WS_Highland_goat <- (MERm_WS_Highland_goat + MERg_WS_Highland_goat + MERl_WS_Highland_goat + MERt_WS_Highland_goat)

MERtotal_DS_Highland_bull <- (MERm_DS_Highland_bull + MERg_DS_Highland_bull + MERt_DS_Highland_bull)
MERtotal_DS_Highland_steer <- (MERm_DS_Highland_steer + MERg_DS_Highland_steer + MERt_DS_Highland_steer)
MERtotal_DS_Highland_calf <- (MERm_DS_Highland_calf + MERg_DS_Highland_calf + MERt_DS_Highland_calf)
MERtotal_DS_Highland_heifer <- (MERm_DS_Highland_heifer + MERg_DS_Highland_heifer + MERt_DS_Highland_heifer)
#MERtotal_DS_Highland_cow <- (MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERl_DS_Highland_cow + MERt_DS_Highland_cow)
MERtotal_DS_Highland_lamb <- (MERm_DS_Highland_lamb + MERg_DS_Highland_lamb + MERt_DS_Highland_lamb)
#MERtotal_DS_Highland_sheep <- (MERm_DS_Highland_sheep + MERg_DS_Highland_sheep + MERl_DS_Highland_sheep + MERt_DS_Highland_sheep)
MERtotal_DS_Highland_kid <- (MERm_DS_Highland_kid + MERg_DS_Highland_kid + MERt_DS_Highland_kid)
#MERtotal_DS_Highland_goat <- (MERm_DS_Highland_goat + MERg_DS_Highland_goat + MERl_DS_Highland_goat + MERt_DS_Highland_goat)

##Total pop requirement calcs
MERtotalYr_WS_Highland_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_WS_Highland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
            wetSSN) +
           #Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
           (MERm_WS_Highland_bull * 0.4 * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
           param_ME$value[param_ME$Variable=="Work" & param_ME$name == "Bull" & param_ME$Region == "Highland"] *
           wetSSN) +
           (MERtotal_WS_Highland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERtotal_WS_Highland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERtotal_WS_Highland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Highland"]*
              wetSSN) +
           ((MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERt_WS_Highland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Highland_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_WS_Highland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Highland"]*
            wetSSN) +
           ((MERm_WS_Highland_sheep + MERg_WS_Highland_sheep + MERt_WS_Highland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Highland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Highland"]*
              wetSSN) +
           ((MERm_WS_Highland_goat + MERg_WS_Highland_goat + MERt_WS_Highland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Highland"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Highland_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_DS_Highland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
            drySSN) +
           (MERtotal_DS_Highland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERtotal_DS_Highland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERtotal_DS_Highland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Highland"]*
              drySSN) +
           ((MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERt_DS_Highland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Highland_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_DS_Highland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Highland"]*
            drySSN) +
           ((MERm_DS_Highland_sheep + MERg_DS_Highland_sheep + MERt_DS_Highland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Highland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Highland"]*
              drySSN) +
           ((MERm_DS_Highland_goat + MERg_DS_Highland_goat + MERt_DS_Highland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

#######
#Highland zone - periurban
#MERm cattle

MERm_WS_Periurban_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

#dry season
MERm_DS_Periurban_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                         param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                        param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                        ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                            exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                           ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                      param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                      ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                          exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                         ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                       param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                       ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                           exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                          ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

#Dry season
MERt_DS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

#Dry season
MERl_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                      ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                        ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                       ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)


MERg_WS_Periurban_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / (0.8))

MERg_WS_Periurban_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / (0.8))

MERg_WS_Periurban_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)

MERg_WS_Periurban_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                0.92*EC) / 0.8)

MERg_WS_Periurban_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 0.8)

MERg_WS_Periurban_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)

MERg_WS_Periurban_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 0.8)

MERg_WS_Periurban_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 0.8)

MERg_WS_Periurban_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)

#Dry season
MERg_DS_Periurban_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / (0.8))

MERg_DS_Periurban_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >=0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / (0.8))

MERg_DS_Periurban_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)

MERg_DS_Periurban_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                0.92*EC) / 
                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                0.92*EC) / 0.8)

MERg_DS_Periurban_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 0.8)

MERg_DS_Periurban_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)

MERg_DS_Periurban_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 
                              (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                            (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                               0.92*EC) / 0.8)

MERg_DS_Periurban_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             0.92*EC) / 0.8)

MERg_DS_Periurban_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >=0,
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 
                             (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                           (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              0.92*EC) / 0.8)


##MERtotal by animal class
#Testing total
#MERtotal_WS_Periurban_cow <- (MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERl_WS_Periurban_cow + MERt_WS_Periurban_cow) * 110
#MERtotal_DS_Periurban_cow <- (MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERl_DS_Periurban_cow + MERt_DS_Periurban_cow) * 228
MERtotal_WS_Periurban_bull <- (MERm_WS_Periurban_bull + MERg_WS_Periurban_bull + MERt_WS_Periurban_bull)
MERtotal_WS_Periurban_steer <- (MERm_WS_Periurban_steer + MERg_WS_Periurban_steer + MERt_WS_Periurban_steer)
MERtotal_WS_Periurban_calf <- (MERm_WS_Periurban_calf + MERg_WS_Periurban_calf + MERt_WS_Periurban_calf)
MERtotal_WS_Periurban_heifer <- (MERm_WS_Periurban_heifer + MERg_WS_Periurban_heifer + MERt_WS_Periurban_heifer)
#MERtotal_WS_Periurban_cow <- (MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERl_WS_Periurban_cow + MERt_WS_Periurban_cow)
MERtotal_WS_Periurban_lamb <- (MERm_WS_Periurban_lamb + MERg_WS_Periurban_lamb + MERt_WS_Periurban_lamb)
#MERtotal_WS_Periurban_sheep <- (MERm_WS_Periurban_sheep + MERg_WS_Periurban_sheep + MERl_WS_Periurban_sheep + MERt_WS_Periurban_sheep)
MERtotal_WS_Periurban_kid <- (MERm_WS_Periurban_kid + MERg_WS_Periurban_kid + MERt_WS_Periurban_kid)
#MERtotal_WS_Periurban_goat <- (MERm_WS_Periurban_goat + MERg_WS_Periurban_goat + MERl_WS_Periurban_goat + MERt_WS_Periurban_goat)

MERtotal_DS_Periurban_bull <- (MERm_DS_Periurban_bull + MERg_DS_Periurban_bull + MERt_DS_Periurban_bull)
MERtotal_DS_Periurban_steer <- (MERm_DS_Periurban_steer + MERg_DS_Periurban_steer + MERt_DS_Periurban_steer)
MERtotal_DS_Periurban_calf <- (MERm_DS_Periurban_calf + MERg_DS_Periurban_calf + MERt_DS_Periurban_calf)
MERtotal_DS_Periurban_heifer <- (MERm_DS_Periurban_heifer + MERg_DS_Periurban_heifer + MERt_DS_Periurban_heifer)
#MERtotal_DS_Periurban_cow <- (MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERl_DS_Periurban_cow + MERt_DS_Periurban_cow)
MERtotal_DS_Periurban_lamb <- (MERm_DS_Periurban_lamb + MERg_DS_Periurban_lamb + MERt_DS_Periurban_lamb)
#MERtotal_DS_Periurban_sheep <- (MERm_DS_Periurban_sheep + MERg_DS_Periurban_sheep + MERl_DS_Periurban_sheep + MERt_DS_Periurban_sheep)
MERtotal_DS_Periurban_kid <- (MERm_DS_Periurban_kid + MERg_DS_Periurban_kid + MERt_DS_Periurban_kid)
#MERtotal_DS_Periurban_goat <- (MERm_DS_Periurban_goat + MERg_DS_Periurban_goat + MERl_DS_Periurban_goat + MERt_DS_Periurban_goat)

##Total pop requirement calcs
MERtotalYr_WS_Periurban_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_WS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            wetSSN) +
           (MERtotal_WS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERt_WS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_WS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            wetSSN) +
           ((MERm_WS_Periurban_sheep + MERg_WS_Periurban_sheep + MERt_WS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Periurban_goat + MERg_WS_Periurban_goat + MERt_WS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Periurban"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_DS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            drySSN) +
           (MERtotal_DS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERt_DS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_DS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            drySSN) +
           ((MERm_DS_Periurban_sheep + MERg_DS_Periurban_sheep + MERt_DS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Periurban_goat + MERg_DS_Periurban_goat + MERt_DS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

##################
##############
#Total requirements in MJ
#Horse and donkey liveweights from FAO, 2018 Report on feed inventory and feed balance
#Percentage of animals used for work from CSA, 2022. Assumption that work is an additional 40% of maintenance from FAO, 2018 - Report on feed inventory and feed balance
MERhorse <- (((87/1000) * (250^0.75)) * 8.3 * lvHorse *365) + (0.1466*((87/1000) * (250^0.75)) * 8.3 * lvHorse *180 * 0.4) #8.3 is a conversion from DM to ME #Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false

MERdonkey <- (((87/1000) * (150^0.75)) * 8.3 * lvDonkey * 365) + (0.7609*((87/1000) * (150^0.75)) * 8.3 * lvDonkey * 180 * 0.4) #Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false

MERcattle <- sum(MERtotalYr_DS_Lowland_cattle, MERtotalYr_DS_Highland_cattle, MERtotalYr_WS_Lowland_cattle, MERtotalYr_WS_Highland_cattle, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
terra::writeRaster(MERcattle, 'SpatialData/outputs/cattleMER_MJ_2015_min.tif', overwrite = T)

MERshoats <- sum(MERtotalYr_DS_Lowland_shoats, MERtotalYr_DS_Highland_shoats, MERtotalYr_WS_Lowland_shoats, MERtotalYr_WS_Highland_shoats, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Periurban_shoats, na.rm = T)
terra::writeRaster(MERshoats, 'SpatialData/outputs/shoatsMER_MJ_2015_min.tif', overwrite = T)

MERall <- sum(MERhorse, MERdonkey, MERcamel, MERtotalYr_DS_Lowland_cattle, MERtotalYr_DS_Lowland_shoats, MERtotalYr_DS_Highland_cattle, MERtotalYr_DS_Highland_shoats, MERtotalYr_WS_Lowland_cattle, MERtotalYr_WS_Lowland_shoats, MERtotalYr_WS_Highland_cattle, MERtotalYr_WS_Highland_shoats, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
terra::writeRaster(MERall, 'SpatialData/outputs/livestockMER_MJ_2015_min.tif', overwrite = T)



###
#Weighted mean requirements
#Bull total
weighted.mean(c(MERm_DS_Highland_bull, MERm_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_bull, MERg_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_bull, MERt_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
(MERm_WS_Highland_bull * 0.4)
#total = 107.0

weighted.mean(c(MERm_DS_Lowland_bull, MERm_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_bull, MERg_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_bull, MERt_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
(MERm_WS_Lowland_bull* 0.4)
#total = 67.9

#Maintenance
weighted.mean(c(MERm_DS_Highland_cow, MERm_WS_Highland_cow, MERm_DS_Periurban_cow, MERm_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Highland_sheep, MERm_WS_Highland_sheep, MERm_DS_Periurban_sheep, MERm_WS_Periurban_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Highland_goat, MERm_WS_Highland_goat, MERm_DS_Periurban_goat, MERm_WS_Periurban_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))

weighted.mean(c(MERm_DS_Lowland_cow, MERm_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Lowland_sheep, MERm_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Lowland_goat, MERm_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

#Growth - !check sheep and goat inputs
weighted.mean(c(MERg_DS_Highland_cow, MERg_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_sheep, MERg_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_goat, MERg_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERg_DS_Lowland_cow, MERg_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_sheep, MERg_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_goat, MERg_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

#Lactation
weighted.mean(c(MERl_DS_Highland_cow, MERl_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Highland_sheep, MERl_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Highland_goat, MERl_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERl_DS_Lowland_cow, MERl_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Lowland_sheep, MERl_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Lowland_goat, MERl_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERm_DS_Periurban_cow, MERm_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365

#Locomotion
weighted.mean(c(MERt_DS_Highland_cow, MERt_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_sheep, MERt_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_goat, MERt_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Lowland_cow, MERt_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_sheep, MERt_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_goat, MERt_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Periurban_cow, MERt_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))

#Gestation
#mean(MERp_cow_fullPreg)
MERp_cow_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/365

################################################
##
##
##
##Duplicate for max
param_ME <- read.csv('LivestockParams/Livestock_energy_requirement_max.csv', stringsAsFactors = F)
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))
ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
EC <- 20 #energy content of the tissue=MJ/kg

###Spatial data
#Add level 3 admin boundaries
aoi1 <-  st_read('SpatialData/inputs/aoi1.gpkg')

lvCattle <- rast('SpatialData/inputs/GLW4/Ct_2015_10k.tif')
lvSheep <- rast('SpatialData/inputs/GLW4/Sh_2015_10k.tif')
lvGoat <- rast('SpatialData/inputs/GLW4/Gt_2015_10k.tif')
lvCattle <- terra::crop(lvCattle, aoi1, mask = T)
lvSheep <- terra::crop(lvSheep, aoi1, mask = T)
lvGoat <- terra::crop(lvGoat, aoi1, mask = T)

lv <- c(lvCattle, lvSheep, lvGoat) #Number per 10km pixel

lvHorse <- terra::crop(lvHorse, aoi1, mask = T)

r <- init(lvCattle, NA)
nc <- vect('SpatialData/inputs/aoi1.gpkg')

r.ext <- terra::extract(r, nc)
r.ext <- group_by(r.ext, ID)
nc$cells <- summarise(r.ext, fun = n())$fun
nc$donkey_cell <- nc$Donkeys / nc$cells

#Calculate ME for maintenance and work. Assuming a weight of 275 in Afar and 400 elsewhere (FAO, 2018, Report on feed inventory and feed balance). 
#Work was assumed to be 40% of maintenance, where a little over 15% of camels were estimated to work (CSA, 2022)
nc$camelME_cell <- ifelse(nc$NAME_1 == "Afar", ((nc$Camels*0.435*(275^0.75)*365) + (nc$Camels*0.435*(275^0.75)*180)*0.1548*0.4) / nc$cells, ((nc$Camels*0.435*(400^0.75)*365) + (nc$Camels*0.435*(400^0.75)*180)*0.1548*0.4) / nc$cells)

lvDonkey <- rasterize(nc, r, field = "donkey_cell")
MERcamel <- rasterize(nc, r, field = "camelME_cell")

##Add production system and livelihood layers
livelihood <- rast('SpatialData/inputs/Elevation/elev1500.tif')
livelihood <- terra::crop(livelihood, aoi1, mask = T)
livelihood <- terra::resample(livelihood, lv, method = 'near')
systemMixedCrop <- rast("SpatialData/inputs/Lvst_system/w001001.adf") #1-4 = rangelands; 5-8 = mixed systems; 9-14 = irrigated
systemMixedCrop <- terra::resample(systemMixedCrop, lv, method = 'near')
systemMixedCrop <- terra::crop(systemMixedCrop, livelihood, mask = T)
periurban <- systemMixedCrop == 13
lv <- c(lv, livelihood)
lv <- c(lv, periurban)

lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = 'SpatialData/inputs/GLW4/TLU.tif', overwrite = T)

#Add season data
wetSSN <- rast('cropParams/croppingDays.tif')
wetSSN <- terra::crop(wetSSN, aoi1, mask = T)
wetSSN <- terra::resample(wetSSN, lv, method = 'near')
wetSSN[is.na(wetSSN)] <- global(wetSSN, 'mean', na.rm = T) #Fill NAs and 0s with global mean
wetSSN[wetSSN ==0] <- global(wetSSN, 'mean', na.rm = T)
drySSN <- 365 - wetSSN
lv <- c(lv, wetSSN)
lv <- c(lv, drySSN)


##Intermediate MER calculations
#################
#Sahelian zone
#MERm Sah cattle 
MERm_WS_Lowland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

MERm_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])+0.5)))

#dry season
MERm_DS_Lowland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                          param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                          ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                              exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                             ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))

MERm_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Lowland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] * 0.0026)

#Dry season
MERt_DS_Lowland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)

MERt_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Lowland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                          ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_WS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_WS_Lowland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"])
   +0.04)

#Dry season
MERl_DS_Lowland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                          ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_DS_Lowland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
   +0.04)

MERl_DS_Lowland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"])
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


MERg_WS_Lowland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / (0.8))

MERg_WS_Lowland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / (0.8))

MERg_WS_Lowland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Lowland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 0.8)

MERg_WS_Lowland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_WS_Lowland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Lowland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Lowland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_WS_Lowland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"] >=0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

#Dry season
MERg_DS_Lowland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / (0.8))

MERg_DS_Lowland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / (0.8))

MERg_DS_Lowland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Lowland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                    0.92*EC) / 0.8)

MERg_DS_Lowland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_DS_Lowland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Lowland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Lowland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >= 0,
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 
                                (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                              (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                 0.92*EC) / 0.8)

MERg_DS_Lowland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"] >=0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Lowland"]*
                                  0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Lowland_cow <- (MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERl_WS_Lowland_cow + MERt_WS_Lowland_cow) * 110
#MERtotal_DS_Lowland_cow <- (MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERl_DS_Lowland_cow + MERt_DS_Lowland_cow) * 228
MERtotal_WS_Lowland_bull <- (MERm_WS_Lowland_bull + MERg_WS_Lowland_bull + MERt_WS_Lowland_bull)
MERtotal_WS_Lowland_steer <- (MERm_WS_Lowland_steer + MERg_WS_Lowland_steer + MERt_WS_Lowland_steer)
MERtotal_WS_Lowland_calf <- (MERm_WS_Lowland_calf + MERg_WS_Lowland_calf + MERt_WS_Lowland_calf)
MERtotal_WS_Lowland_heifer <- (MERm_WS_Lowland_heifer + MERg_WS_Lowland_heifer + MERt_WS_Lowland_heifer)
#MERtotal_WS_Lowland_cow <- (MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERl_WS_Lowland_cow + MERt_WS_Lowland_cow)
MERtotal_WS_Lowland_lamb <- (MERm_WS_Lowland_lamb + MERg_WS_Lowland_lamb + MERt_WS_Lowland_lamb)
#MERtotal_WS_Lowland_sheep <- (MERm_WS_Lowland_sheep + MERg_WS_Lowland_sheep + MERl_WS_Lowland_sheep + MERt_WS_Lowland_sheep)
MERtotal_WS_Lowland_kid <- (MERm_WS_Lowland_kid + MERg_WS_Lowland_kid + MERt_WS_Lowland_kid)
#MERtotal_WS_Lowland_goat <- (MERm_WS_Lowland_goat + MERg_WS_Lowland_goat + MERl_WS_Lowland_goat + MERt_WS_Lowland_goat)

MERtotal_DS_Lowland_bull <- (MERm_DS_Lowland_bull + MERg_DS_Lowland_bull + MERt_DS_Lowland_bull)
MERtotal_DS_Lowland_steer <- (MERm_DS_Lowland_steer + MERg_DS_Lowland_steer + MERt_DS_Lowland_steer)
MERtotal_DS_Lowland_calf <- (MERm_DS_Lowland_calf + MERg_DS_Lowland_calf + MERt_DS_Lowland_calf)
MERtotal_DS_Lowland_heifer <- (MERm_DS_Lowland_heifer + MERg_DS_Lowland_heifer + MERt_DS_Lowland_heifer)
#MERtotal_DS_Lowland_cow <- (MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERl_DS_Lowland_cow + MERt_DS_Lowland_cow)
MERtotal_DS_Lowland_lamb <- (MERm_DS_Lowland_lamb + MERg_DS_Lowland_lamb + MERt_DS_Lowland_lamb)
#MERtotal_DS_Lowland_sheep <- (MERm_DS_Lowland_sheep + MERg_DS_Lowland_sheep + MERl_DS_Lowland_sheep + MERt_DS_Lowland_sheep)
MERtotal_DS_Lowland_kid <- (MERm_DS_Lowland_kid + MERg_DS_Lowland_kid + MERt_DS_Lowland_kid)
#MERtotal_DS_Lowland_goat <- (MERm_DS_Lowland_goat + MERg_DS_Lowland_goat + MERl_DS_Lowland_goat + MERt_DS_Lowland_goat)

##Total pop requirement calcs
MERtotalYr_WS_Lowland_cattle <- lapp(lv[[c(1,4,6)]], fun = function(cattle, livelihood, wetSSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_WS_Lowland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Lowland"]*
            wetSSN) +
           (MERtotal_WS_Lowland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERtotal_WS_Lowland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERtotal_WS_Lowland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Lowland"]*
              wetSSN) +
           ((MERm_WS_Lowland_cow + MERg_WS_Lowland_cow + MERt_WS_Lowland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Lowland"
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Lowland_shoats <- lapp(lv[[c(2,3,4,6)]], fun = function(sheep, goats, livelihood, wetSSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_WS_Lowland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Lowland"]*
            wetSSN) +
           ((MERm_WS_Lowland_sheep + MERg_WS_Lowland_sheep + MERt_WS_Lowland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Lowland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Lowland"]*
              wetSSN) +
           ((MERm_WS_Lowland_goat + MERg_WS_Lowland_goat + MERt_WS_Lowland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              wetSSN) +
           (MERl_WS_Lowland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Lowland_cattle <- lapp(lv[[c(1,4,7)]], fun = function(cattle, livelihood, drySSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_DS_Lowland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Lowland"]*
            drySSN) +
           (MERtotal_DS_Lowland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERtotal_DS_Lowland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERtotal_DS_Lowland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Lowland"]*
              drySSN) +
           ((MERm_DS_Lowland_cow + MERg_DS_Lowland_cow + MERt_DS_Lowland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Lowland_shoats <- lapp(lv[[c(2,3,4,7)]], fun = function(sheep, goats, livelihood, drySSN) { 
  ifelse(livelihood ==0, #0 is the lowland
         (MERtotal_DS_Lowland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Lowland"]*
            drySSN) +
           ((MERm_DS_Lowland_sheep + MERg_DS_Lowland_sheep + MERt_DS_Lowland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Lowland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Lowland"]*
              drySSN) +
           ((MERm_DS_Lowland_goat + MERg_DS_Lowland_goat + MERt_DS_Lowland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              drySSN) +
           (MERl_DS_Lowland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Lowland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})


#######
#Highland zone  
#MERm Sud cattle

MERm_WS_Highland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

MERm_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"])+0.5)))

#dry season
MERm_DS_Highland_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                           param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                           ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                               exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                              ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))

MERm_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Highland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

MERt_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"] * 0.0026)

#Dry season
MERt_DS_Highland_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)

MERt_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Highland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

MERl_WS_Highland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

MERl_WS_Highland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"])
   +0.04)

#Dry season
MERl_DS_Highland_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                           ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

MERl_DS_Highland_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

MERl_DS_Highland_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"])
   +0.04)

#MERg #!x10 greater than Rahimi et al.
#Wet season
#MERg=(DWG*0.92*EC)/(0.043*M.D)
#Negative
#MERg_dry=(DWG*0.92*EC)/(0.8)


MERg_WS_Highland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / (0.8))

MERg_WS_Highland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / (0.8))

MERg_WS_Highland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Highland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 0.8)

MERg_WS_Highland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Highland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_WS_Highland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 0.8)

MERg_WS_Highland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_WS_Highland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

#Dry season
MERg_DS_Highland_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / (0.8))

MERg_DS_Highland_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / (0.8))

MERg_DS_Highland_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Highland_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                     0.92*EC) / 0.8)

MERg_DS_Highland_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Highland_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)

MERg_DS_Highland_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                    0.92*EC) / 0.8)

MERg_DS_Highland_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >= 0,
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 
                                 (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                               (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                  0.92*EC) / 0.8)

MERg_DS_Highland_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"] >=0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Highland"]*
                                   0.92*EC) / 0.8)






##MERtotal by animal class
#Testing total
#MERtotal_WS_Highland_cow <- (MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERl_WS_Highland_cow + MERt_WS_Highland_cow) * 110
#MERtotal_DS_Highland_cow <- (MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERl_DS_Highland_cow + MERt_DS_Highland_cow) * 228
MERtotal_WS_Highland_bull <- (MERm_WS_Highland_bull + MERg_WS_Highland_bull + MERt_WS_Highland_bull)
MERtotal_WS_Highland_steer <- (MERm_WS_Highland_steer + MERg_WS_Highland_steer + MERt_WS_Highland_steer)
MERtotal_WS_Highland_calf <- (MERm_WS_Highland_calf + MERg_WS_Highland_calf + MERt_WS_Highland_calf)
MERtotal_WS_Highland_heifer <- (MERm_WS_Highland_heifer + MERg_WS_Highland_heifer + MERt_WS_Highland_heifer)
#MERtotal_WS_Highland_cow <- (MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERl_WS_Highland_cow + MERt_WS_Highland_cow)
MERtotal_WS_Highland_lamb <- (MERm_WS_Highland_lamb + MERg_WS_Highland_lamb + MERt_WS_Highland_lamb)
#MERtotal_WS_Highland_sheep <- (MERm_WS_Highland_sheep + MERg_WS_Highland_sheep + MERl_WS_Highland_sheep + MERt_WS_Highland_sheep)
MERtotal_WS_Highland_kid <- (MERm_WS_Highland_kid + MERg_WS_Highland_kid + MERt_WS_Highland_kid)
#MERtotal_WS_Highland_goat <- (MERm_WS_Highland_goat + MERg_WS_Highland_goat + MERl_WS_Highland_goat + MERt_WS_Highland_goat)

MERtotal_DS_Highland_bull <- (MERm_DS_Highland_bull + MERg_DS_Highland_bull + MERt_DS_Highland_bull)
MERtotal_DS_Highland_steer <- (MERm_DS_Highland_steer + MERg_DS_Highland_steer + MERt_DS_Highland_steer)
MERtotal_DS_Highland_calf <- (MERm_DS_Highland_calf + MERg_DS_Highland_calf + MERt_DS_Highland_calf)
MERtotal_DS_Highland_heifer <- (MERm_DS_Highland_heifer + MERg_DS_Highland_heifer + MERt_DS_Highland_heifer)
#MERtotal_DS_Highland_cow <- (MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERl_DS_Highland_cow + MERt_DS_Highland_cow)
MERtotal_DS_Highland_lamb <- (MERm_DS_Highland_lamb + MERg_DS_Highland_lamb + MERt_DS_Highland_lamb)
#MERtotal_DS_Highland_sheep <- (MERm_DS_Highland_sheep + MERg_DS_Highland_sheep + MERl_DS_Highland_sheep + MERt_DS_Highland_sheep)
MERtotal_DS_Highland_kid <- (MERm_DS_Highland_kid + MERg_DS_Highland_kid + MERt_DS_Highland_kid)
#MERtotal_DS_Highland_goat <- (MERm_DS_Highland_goat + MERg_DS_Highland_goat + MERl_DS_Highland_goat + MERt_DS_Highland_goat)

##Total pop requirement calcs
MERtotalYr_WS_Highland_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_WS_Highland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
            wetSSN) +
           #Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
           (MERm_WS_Highland_bull * 0.4 * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
           param_ME$value[param_ME$Variable=="Work" & param_ME$name == "Bull" & param_ME$Region == "Highland"] *
           wetSSN) +
           (MERtotal_WS_Highland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERtotal_WS_Highland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERtotal_WS_Highland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Highland"]*
              wetSSN) +
           ((MERm_WS_Highland_cow + MERg_WS_Highland_cow + MERt_WS_Highland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Highland_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_WS_Highland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Highland"]*
            wetSSN) +
           ((MERm_WS_Highland_sheep + MERg_WS_Highland_sheep + MERt_WS_Highland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Highland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Highland"]*
              wetSSN) +
           ((MERm_WS_Highland_goat + MERg_WS_Highland_goat + MERt_WS_Highland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              wetSSN) +
           (MERl_WS_Highland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Highland"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Highland_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_DS_Highland_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Highland"]*
            drySSN) +
           (MERtotal_DS_Highland_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERtotal_DS_Highland_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERtotal_DS_Highland_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Highland"]*
              drySSN) +
           ((MERm_DS_Highland_cow + MERg_DS_Highland_cow + MERt_DS_Highland_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Highland_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==0, #1 is the highlands
         (MERtotal_DS_Highland_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Highland"]*
            drySSN) +
           ((MERm_DS_Highland_sheep + MERg_DS_Highland_sheep + MERt_DS_Highland_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Highland_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Highland"]*
              drySSN) +
           ((MERm_DS_Highland_goat + MERg_DS_Highland_goat + MERt_DS_Highland_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              drySSN) +
           (MERl_DS_Highland_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Highland"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

#######
#Highland zone - periurban
#MERm cattle

MERm_WS_Periurban_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                               param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                               param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                               ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                   exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                  ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

MERm_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])+0.5)))

#dry season
MERm_DS_Periurban_bull <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Bull"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Bull"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Bull"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Bull"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_steer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Steer"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Steer"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Steer"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Steer"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_calf <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Calf"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Calf"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Calf"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Calf"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Heifer"]*
                               param_ME$value[param_ME$Variable == "S" & param_ME$name == "Heifer"]*
                               param_ME$value[param_ME$Variable == "M" & param_ME$name == "Heifer"]*
                               ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                   exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Heifer"]))/
                                  ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Cow"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Cow"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Cow"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Cow"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Lamb"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Lamb"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Lamb"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Lamb"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Sheep"]*
                              param_ME$value[param_ME$Variable == "S" & param_ME$name == "Sheep"]*
                              param_ME$value[param_ME$Variable == "M" & param_ME$name == "Sheep"]*
                              ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                  exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Sheep"]))/
                                 ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_kid <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Kid"]*
                            param_ME$value[param_ME$Variable == "S" & param_ME$name == "Kid"]*
                            param_ME$value[param_ME$Variable == "M" & param_ME$name == "Kid"]*
                            ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Kid"]))/
                               ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))

MERm_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == "Goat"]*
                             param_ME$value[param_ME$Variable == "S" & param_ME$name == "Goat"]*
                             param_ME$value[param_ME$Variable == "M" & param_ME$name == "Goat"]*
                             ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]^0.75)*
                                 exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == "Goat"]))/
                                ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])+0.5)))


#MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
#Wet season
MERt_WS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                               param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] * 0.0026)

#Dry season
MERt_DS_Periurban_bull <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_steer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_calf <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_heifer <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                               param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_lamb <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_kid <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                            param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)

MERt_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             param_ME$value[param_ME$Variable == "MLW" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] * 0.0026)


#MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
#wet season
MERl_WS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_WS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                              ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_WS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"])
   +0.04)

#Dry season
MERl_DS_Periurban_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                            ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_DS_Periurban_sheep <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                              ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)

MERl_DS_Periurban_goat <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                             ECM)/
  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"])
   +0.04)


MERg_WS_Periurban_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / (0.8))

MERg_WS_Periurban_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >=0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / (0.8))

MERg_WS_Periurban_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)

MERg_WS_Periurban_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                   (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                      0.92*EC) / 
                                     (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                   (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                      0.92*EC) / 0.8)

MERg_WS_Periurban_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 0.8)

MERg_WS_Periurban_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)

MERg_WS_Periurban_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 0.8)

MERg_WS_Periurban_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 0.8)

MERg_WS_Periurban_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "WS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)

#Dry season
MERg_DS_Periurban_bull <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Bull" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / (0.8))

MERg_DS_Periurban_steer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >=0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Steer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / (0.8))

MERg_DS_Periurban_calf <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Calf" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)

MERg_DS_Periurban_heifer <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                   (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                      0.92*EC) / 
                                     (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                   (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Heifer" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                      0.92*EC) / 0.8)

MERg_DS_Periurban_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 0.8)

MERg_DS_Periurban_lamb <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Lamb" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)

MERg_DS_Periurban_sheep <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 
                                    (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                  (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Sheep" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                     0.92*EC) / 0.8)

MERg_DS_Periurban_kid <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >= 0,
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 
                                  (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Kid" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                   0.92*EC) / 0.8)

MERg_DS_Periurban_goat <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"] >=0,
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 
                                   (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]),
                                 (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Goat" & param_ME$Season == "DS" & param_ME$Region == "Periurban"]*
                                    0.92*EC) / 0.8)


##MERtotal by animal class
#Testing total
#MERtotal_WS_Periurban_cow <- (MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERl_WS_Periurban_cow + MERt_WS_Periurban_cow) * 110
#MERtotal_DS_Periurban_cow <- (MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERl_DS_Periurban_cow + MERt_DS_Periurban_cow) * 228
MERtotal_WS_Periurban_bull <- (MERm_WS_Periurban_bull + MERg_WS_Periurban_bull + MERt_WS_Periurban_bull)
MERtotal_WS_Periurban_steer <- (MERm_WS_Periurban_steer + MERg_WS_Periurban_steer + MERt_WS_Periurban_steer)
MERtotal_WS_Periurban_calf <- (MERm_WS_Periurban_calf + MERg_WS_Periurban_calf + MERt_WS_Periurban_calf)
MERtotal_WS_Periurban_heifer <- (MERm_WS_Periurban_heifer + MERg_WS_Periurban_heifer + MERt_WS_Periurban_heifer)
#MERtotal_WS_Periurban_cow <- (MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERl_WS_Periurban_cow + MERt_WS_Periurban_cow)
MERtotal_WS_Periurban_lamb <- (MERm_WS_Periurban_lamb + MERg_WS_Periurban_lamb + MERt_WS_Periurban_lamb)
#MERtotal_WS_Periurban_sheep <- (MERm_WS_Periurban_sheep + MERg_WS_Periurban_sheep + MERl_WS_Periurban_sheep + MERt_WS_Periurban_sheep)
MERtotal_WS_Periurban_kid <- (MERm_WS_Periurban_kid + MERg_WS_Periurban_kid + MERt_WS_Periurban_kid)
#MERtotal_WS_Periurban_goat <- (MERm_WS_Periurban_goat + MERg_WS_Periurban_goat + MERl_WS_Periurban_goat + MERt_WS_Periurban_goat)

MERtotal_DS_Periurban_bull <- (MERm_DS_Periurban_bull + MERg_DS_Periurban_bull + MERt_DS_Periurban_bull)
MERtotal_DS_Periurban_steer <- (MERm_DS_Periurban_steer + MERg_DS_Periurban_steer + MERt_DS_Periurban_steer)
MERtotal_DS_Periurban_calf <- (MERm_DS_Periurban_calf + MERg_DS_Periurban_calf + MERt_DS_Periurban_calf)
MERtotal_DS_Periurban_heifer <- (MERm_DS_Periurban_heifer + MERg_DS_Periurban_heifer + MERt_DS_Periurban_heifer)
#MERtotal_DS_Periurban_cow <- (MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERl_DS_Periurban_cow + MERt_DS_Periurban_cow)
MERtotal_DS_Periurban_lamb <- (MERm_DS_Periurban_lamb + MERg_DS_Periurban_lamb + MERt_DS_Periurban_lamb)
#MERtotal_DS_Periurban_sheep <- (MERm_DS_Periurban_sheep + MERg_DS_Periurban_sheep + MERl_DS_Periurban_sheep + MERt_DS_Periurban_sheep)
MERtotal_DS_Periurban_kid <- (MERm_DS_Periurban_kid + MERg_DS_Periurban_kid + MERt_DS_Periurban_kid)
#MERtotal_DS_Periurban_goat <- (MERm_DS_Periurban_goat + MERg_DS_Periurban_goat + MERl_DS_Periurban_goat + MERt_DS_Periurban_goat)

##Total pop requirement calcs
MERtotalYr_WS_Periurban_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, livelihood, periurban, wetSSN) { #!Including pregnancy
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_WS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            wetSSN) +
           (MERtotal_WS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERtotal_WS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Periurban_cow + MERg_WS_Periurban_cow + MERt_WS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
         ,NA)#Close if
})

MERtotalYr_WS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, livelihood, periurban, wetSSN) { #! including pregnancy
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_WS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            wetSSN) +
           ((MERm_WS_Periurban_sheep + MERg_WS_Periurban_sheep + MERt_WS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_WS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              wetSSN) +
           ((MERm_WS_Periurban_goat + MERg_WS_Periurban_goat + MERt_WS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              wetSSN) +
           (MERl_WS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == "Periurban"
           (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
           (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_DS_Periurban_bull * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == "Periurban"]*
            drySSN) +
           (MERtotal_DS_Periurban_steer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_calf * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERtotal_DS_Periurban_heifer * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Periurban_cow + MERg_DS_Periurban_cow + MERt_DS_Periurban_cow) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_cow * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

MERtotalYr_DS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, livelihood, periurban, drySSN) { 
  ifelse(livelihood ==1 & periurban ==1, #1 is the highlands
         (MERtotal_DS_Periurban_lamb * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == "Periurban"]*
            drySSN) +
           ((MERm_DS_Periurban_sheep + MERg_DS_Periurban_sheep + MERt_DS_Periurban_sheep) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_sheep* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
           (MERtotal_DS_Periurban_kid * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == "Periurban"]*
              drySSN) +
           ((MERm_DS_Periurban_goat + MERg_DS_Periurban_goat + MERt_DS_Periurban_goat) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              drySSN) +
           (MERl_DS_Periurban_goat * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == "Periurban"]*
              param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
              param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
         ,NA)#Close if
})

##################
##############
#Total requirements in MJ
#MERall <- sum(eval(parse(text = ls(pattern = "MERtotalYr"))), na.rm = T)
#Horse and donkey liveweights from FAO, 2018 Report on feed inventory and feed balance
#Percentage of animals used for work from CSA, 2022. Assumption that work is an additional 40% of maintenance from FAO, 2018 - Report on feed inventory and feed balance
MERhorse <- (((87/1000) * (250^0.75)) * 8.3 * lvHorse *365) + (0.1466*((87/1000) * (250^0.75)) * 8.3 * lvHorse *180 * 0.4) #8.3 is a conversion from DM to ME #Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false

MERdonkey <- (((87/1000) * (150^0.75)) * 8.3 * lvDonkey * 365) + (0.7609*((87/1000) * (150^0.75)) * 8.3 * lvDonkey * 180 * 0.4) #Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false

MERcattle <- sum(MERtotalYr_DS_Lowland_cattle, MERtotalYr_DS_Highland_cattle, MERtotalYr_WS_Lowland_cattle, MERtotalYr_WS_Highland_cattle, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
terra::writeRaster(MERcattle, 'SpatialData/outputs/cattleMER_MJ_2015_max.tif', overwrite = T)

MERshoats <- sum(MERtotalYr_DS_Lowland_shoats, MERtotalYr_DS_Highland_shoats, MERtotalYr_WS_Lowland_shoats, MERtotalYr_WS_Highland_shoats, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Periurban_shoats, na.rm = T)
terra::writeRaster(MERshoats, 'SpatialData/outputs/shoatsMER_MJ_2015_max.tif', overwrite = T)

MERall <- sum(MERhorse, MERdonkey, MERcamel, MERtotalYr_DS_Lowland_cattle, MERtotalYr_DS_Lowland_shoats, MERtotalYr_DS_Highland_cattle, MERtotalYr_DS_Highland_shoats, MERtotalYr_WS_Lowland_cattle, MERtotalYr_WS_Lowland_shoats, MERtotalYr_WS_Highland_cattle, MERtotalYr_WS_Highland_shoats, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
terra::writeRaster(MERall, 'SpatialData/outputs/livestockMER_MJ_2015_max.tif', overwrite = T)


###
#Weighted mean requirements
#Bull total
weighted.mean(c(MERm_DS_Highland_bull, MERm_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_bull, MERg_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_bull, MERt_WS_Highland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
(MERm_WS_Highland_bull * 0.4)
#total = 107.0

weighted.mean(c(MERm_DS_Lowland_bull, MERm_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_bull, MERg_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_bull, MERt_WS_Lowland_bull), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
#Work/draught power
#Add work only for WS. FAO, 2018 assume work is generally an additional 40% on top of maintenance
(MERm_WS_Lowland_bull* 0.4)
#total = 67.9

#Maintenance
weighted.mean(c(MERm_DS_Highland_cow, MERm_WS_Highland_cow, MERm_DS_Periurban_cow, MERm_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Highland_sheep, MERm_WS_Highland_sheep, MERm_DS_Periurban_sheep, MERm_WS_Periurban_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Highland_goat, MERm_WS_Highland_goat, MERm_DS_Periurban_goat, MERm_WS_Periurban_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))

weighted.mean(c(MERm_DS_Lowland_cow, MERm_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Lowland_sheep, MERm_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERm_DS_Lowland_goat, MERm_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

#Growth - !check sheep and goat inputs
weighted.mean(c(MERg_DS_Highland_cow, MERg_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_sheep, MERg_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Highland_goat, MERg_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERg_DS_Lowland_cow, MERg_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_sheep, MERg_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERg_DS_Lowland_goat, MERg_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

#Lactation
weighted.mean(c(MERl_DS_Highland_cow, MERl_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Highland_sheep, MERl_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Highland_goat, MERl_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERl_DS_Lowland_cow, MERl_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Lowland_sheep, MERl_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERl_DS_Lowland_goat, MERl_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERm_DS_Periurban_cow, MERm_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))*param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]/365

#Locomotion
weighted.mean(c(MERt_DS_Highland_cow, MERt_WS_Highland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_sheep, MERt_WS_Highland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Highland_goat, MERt_WS_Highland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Lowland_cow, MERt_WS_Lowland_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_sheep, MERt_WS_Lowland_sheep), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))
weighted.mean(c(MERt_DS_Lowland_goat, MERt_WS_Lowland_goat), c(as.numeric(global(lv[[7]]*(lv[[4]]==0), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==0), 'mean', na.rm = T))))

weighted.mean(c(MERt_DS_Periurban_cow, MERt_WS_Periurban_cow), c(as.numeric(global(lv[[7]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T)), as.numeric(global(lv[[6]]*(lv[[4]]==1)*(lv[[5]]==1), 'mean', na.rm = T))))

#Gestation
#mean(MERp_cow_fullPreg)
MERp_cow_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/365
MERp_shoat_fullPreg*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/365
