gc()
rm(list=ls())
#.libPaths(c(.libPaths()[2], .libPaths()[3]))
# Prepare livestock requirements - mean
# Author: John Mutua, Simon Fraval
# Last modified by John Mutua on 16/11/2024

# avoid scientific notation
options(scipen = 999)

# # Install required packages
# install.packages("terra")
# install.packages("raster")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("purrr")
# install.packages("tidyterra")

# Load libraries
library(terra)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(tidyterra)
library(readr)
#library(exactextractr) #For zonal statistics

rasterOptions(tmpdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp") # Process needs > 40GB of temporary disk space
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)

terraOptions(tempdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
CropParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams")
LivestockParams_dir <- paste0(root, "/src/3Balance-estimates/", country, "/LivestockParams")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")

param_ME <- read.csv(paste0(LivestockParams_dir, "/Livestock_energy_requirement.csv"), stringsAsFactors = F)
#param_ME <- pivot_longer(select(param_ME, -X), cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat")) %>% 
  filter(Statistic %in% c("Mean", "All"))

##CSIRO, 2007, Nutrient Requirements of Domesticated Ruminants 
#https://vdocuments.net/nutrient-requirements-of-domesticated-ruminants.html?page=1

###Spatial data
#Add level 2 admin boundaries
aoi2 <- st_read(paste0(spatialDir, "/inputs/aoi2.shp"))

ECORegions <- rast(paste0(spatialDir, "/intermediate/regions.tif"))
lvCattle <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2020.D-DA.CTL.tif"))
lvSheep <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2020.D-DA.SHP.tif"))
lvGoat <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2020.D-DA.GTS.tif"))
lvHorse <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2020.D-DA.HRS.tif"))
lvCattle <- terra::crop(lvCattle, aoi2, mask = T)
lvSheep <- terra::crop(lvSheep, aoi2, mask = T)
lvGoat <- terra::crop(lvGoat, aoi2, mask = T)
lvHorse <- terra::crop(lvHorse, aoi2, mask = T)
lvDonkey <- lvHorse*3

lv <- c(lvCattle, lvSheep, lvGoat) #Number per 10km pixel

ECORegions <- terra::resample(ECORegions, lv, method = 'near')
lv <- terra::crop(lv, ECORegions)
lv <- c(lv, ECORegions)

##Add production system layer
glps <- rast(paste0(spatialDir, "/inputs/GLPS/glps.tif"))
glps <- terra::resample(glps, lv, method = 'near')
glps <- terra::crop(glps, ECORegions, mask = T)
periurban <- glps %>% tidyterra::mutate(CLASSNAME = ifelse(as.character(CLASSNAME) == "Urban", 1, 0)) # Convert urban areas to 1, other areas to 0
#periurban <- periurban == 13
lv <- c(lv, periurban)

lvTLU <- lapp(lv[[1:3]], fun = function(cattle, sheep, goat){(cattle*1)+(sheep*0.15)+(goat*0.15)}, filename = paste0(spatialDir, "/inputs/GLW4/TLU.tif"), overwrite = T)

# Produce livestock number by categories
Regions <- c("Dry Savannah", "Forest", "Wet Savannah")
lvPopCategories <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Lamb", "Goat", "Kid")
lvPop <- c(lvCattle, lvSheep, lvGoat, ECORegions)

for(i in Regions){
  
  for(k in lvPopCategories) {
    
    if(k%in%c("Bull", "Cow", "Steer", "Heifer", "Calf")&i=="Dry Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==1, cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Bull", "Cow", "Steer", "Heifer", "Calf")&i=="Forest"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==2, cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Bull", "Cow", "Steer", "Heifer", "Calf")&i=="Wet Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==3, cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Sheep", "Lamb")&i=="Dry Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==1, sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Sheep", "Lamb")&i=="Forest"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==2, sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Sheep", "Lamb")&i=="Wet Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==3, sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Goat", "Kid")&i=="Dry Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==1, goat * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Goat", "Kid")&i=="Forest"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==2, goat * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }else if(k%in%c("Goat", "Kid")&i=="Wet Savannah"){
      LvPopRegion <- lapp(lvPop, fun = function(cattle, sheep, goat, region) {
        ifelse(region==3, goat * param_ME$value[param_ME$Variable=="HS" & param_ME$name == k & param_ME$Region == i], NA)
      })
    }
    terra::writeRaster(LvPopRegion, paste0(spatialDir, "/inputs/GLW4/Dissag_GLW4_2020_", k, "_", i, ".tif"), overwrite=TRUE)
  
  } 
}

# Combine livestock population layers
lvDir <- paste0(root, "/src/3Balance-estimates/Nigeria/SpatialData/inputs/GLW4")
for(k in lvPopCategories){
  lvRas <- sum(rast(paste0(lvDir, "/Dissag_GLW4_2020_", k, "_Dry Savannah.tif")), rast(paste0(lvDir, "/Dissag_GLW4_2020_", k, "_Wet Savannah.tif")), rast(paste0(lvDir, "/Dissag_GLW4_2020_", k, "_Forest.tif")), na.rm=TRUE)
  terra::writeRaster(lvRas, paste0(spatialDir, "/inputs/GLW4/Dissag_GLW4_2020_", k, ".tif"), overwrite=TRUE)
}

# Loop through years
yearList <- c("2020", "2021", "2022", "2023")
for(year in yearList){
  
  #Add season data
  wetSSN <- rast(paste0(CropParams_dir, "/Cropping_days/croppingDays_", year, ".tif"))
  wetSSN <- terra::crop(wetSSN, aoi2, mask = T)
  wetSSN <- terra::resample(wetSSN, lv, method = 'near')
  wetSSN[is.na(wetSSN)] <- global(wetSSN, "mean", na.rm = T) #Fill NAs and 0s with global mean
  wetSSN[wetSSN ==0] <- global(wetSSN, "mean", na.rm = T)
  drySSN <- 365 - wetSSN
  lv <- c(lv, wetSSN)
  lv <- c(lv, drySSN)
  
  lv <- terra::crop(lv, aoi2)
  
  ###Admin level approach - comparing average daily requirements #! not used
  #outIntake_TLU_DM <- lapp(lvTLU, fun = function(TLU){((TLU)*(250*0.03))}, filename = 'SpatialData/inputs/tmp/outIntake.tif', overwrite = T) #, filename = 'D:/outIntake.tif', overwrite = T
  
  ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
  EC <- 20 #energy content of the tissue=MJ/kg
  
  ##Intermediate MER calculations
  #################
  # Loop through regions, seasons and livestock species
  Regions <- c("Dry Savannah", "Forest", "Wet Savannah", "Periurban")
  Seasons <- c("DS","WS")
  LivestockCategories <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Goat", "Lamb", "Kid")
  MERm_list <- list() #Maintenance requirements
  MERt_list <- list() #Locomotion requirements
  MERl_list <- list() #Lactation requirements
  MERg_list <- list() #Growth requirements
  for(i in Regions){
    for(j in Seasons){
      for(k in LivestockCategories){
        
        #MERm <-
        if(i!="Periurban"){
          MERm_list[[i]][[j]][[k]] <- (param_ME$value[param_ME$Variable=="K" & param_ME$name == k]*
                                         param_ME$value[param_ME$Variable == "S" & param_ME$name == k]*
                                         param_ME$value[param_ME$Variable == "M" & param_ME$name == k]*
                                         ((0.26*(param_ME$value[param_ME$Variable =="MLW" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]^0.75)*
                                             exp(-0.008*param_ME$value[param_ME$Variable == "Age" & param_ME$name == k]))/
                                            ((0.02*param_ME$value[param_ME$Variable =="M.D" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i])+0.5)))
        }
        
        #MERt <- WD*MLW*0.0026 #[MJ/KgLW/km]
        if(i=="Periurban"){
          MERt_list[[i]][[j]][[k]] <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]*
                                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == k & param_ME$Season == j & param_ME$Region == "Wet Savannah"] * 0.0026)
          
        }else if(i!="Periurban"){
          MERt_list[[i]][[j]][[k]] <- (param_ME$value[param_ME$Variable=="WD" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]*
                                         param_ME$value[param_ME$Variable == "MLW" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i] * 0.0026)
        }
        
        #MERl <- (DMY*ECM)/((0.02*M.D)+0.04) #! These are a factor of 10 too high.
        if(i=="Periurban"&k %in% c("Cow","Sheep","Goat")){
          MERl_list[[i]][[j]][[k]] <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == k & param_ME$Season == j & param_ME$Region == "Wet Savannah"]*
                                         ECM)/((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == k & param_ME$Season == j & param_ME$Region == "Wet Savannah"])+0.04)
          
        }else if(i!="Periurban"&k %in% c("Cow","Sheep","Goat")){
          MERl_list[[i]][[j]][[k]] <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]*
                                         ECM)/((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i])+0.04)
          
        }

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
        MERp_cow_fullPreg <- 0 #!Pregnancy is included in maintenance for Nigeria
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
        MERp_shoat_fullPreg <- 0 #!Pregnancy is included in maintenance for Nigeria
        #MERp_shoat_fullPreg <- integrate(f,min(t),max(t),y)
        
        #MERg 
        #Wet season
        #MERg=(DWG*0.92*EC)/(0.043*M.D)
        #Negative
        #MERg_dry=(DWG*0.92*EC)/(0.8)
        
        if(i!="Periurban"){
          MERg_list[[i]][[j]][[k]] <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i] >= 0,
                                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]*0.92*EC) / 
                                               (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]),
                                             (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == k & param_ME$Season == j & param_ME$Region == i]*0.92*EC) / 0.8)
        }
      }
    }
  }
  
  
  # List energy requirement objects - see them in the R environment????
  MER_objects <- list(MERm_list = MERm_list,MERt_list = MERt_list,MERl_list = MERl_list, MERg_list = MERg_list)
  
  # List to store the dataframes
  processed_MERs <- list()
  
  # Loop through each object and process
  for (name in names(MER_objects)) {
    processed_MERs[[name]] <- MER_objects[[name]] %>%
      imap_dfr(~ map_dfr(.x, ~ map_dfr(.x, ~ tibble(Value = .x), .id = "Livestock"), .id = "Season") %>%
                 mutate(MERx = substr(name, 1, 4)), .id = "Region")
  }
  
  # Combine all processed dataframes into one
  ##MERtotal by animal class, season and region
  MERsYr <- bind_rows(processed_MERs) %>% dplyr::select(Region, Season, Livestock, MERx, Value)
  
  # Lift MERm  for non-adult females from Wet Savannah
  MERmPeriurbanNonAdultFemale <- MERsYr %>% 
    filter(Region == "Wet Savannah", Livestock %in% c("Bull", "Steer", "Calf", "Heifer", "Lamb", "Kid"),MERx == "MERm") %>% mutate(Region = ifelse(Region == "Wet Savannah", "Periurban", Region))
  
  # Lift MERm for adult females from Wet Savannah
  MERmPeriurbanAdultFemale <- MERsYr %>% 
    filter(Region == "Wet Savannah", Livestock %in% c("Cow", "Sheep", "Goat"),MERx == "MERm") %>% mutate(Region = ifelse(Region == "Wet Savannah", "Periurban", Region))
  
  # Lift MERg for non-adult females from Savannah
  MERgPeriurbanNonAdultFemale <- MERsYr %>% 
    filter(Region == "Wet Savannah", Livestock %in% c("Bull", "Steer", "Calf", "Heifer", "Lamb", "Kid"),MERx == "MERg") %>% mutate(Region = ifelse(Region == "Wet Savannah", "Periurban", Region))
  
  # Lift MERg for adult females from Wet Savannah
  MERgPeriurbanAdultFemale <- MERsYr %>% 
    filter(Region == "Wet Savannah", Livestock %in% c("Cow", "Sheep", "Goat"),MERx == "MERg") %>% mutate(Region = ifelse(Region == "Wet Savannah", "Periurban", Region))
  
  # Add Periurban MERs
  MERsYr <- rbind(MERsYr, MERmPeriurbanNonAdultFemale, MERmPeriurbanAdultFemale, MERgPeriurbanNonAdultFemale, MERgPeriurbanAdultFemale)
  
  # explore energy requirements
  MERsYr_wide <- MERsYr %>% pivot_wider(names_from = Livestock, values_from = Value) %>% select(Region, Season, MERx, Bull, Cow, Steer, Heifer, Calf, Sheep, Goat, Lamb, Kid)
  write_csv(MERsYr_wide, paste0(Results_dir, "/MERs_MJday.csv"), append = FALSE)
  
  # Summarize MERs
  MERtotalYr <- MERsYr %>% group_by(Region, Season, Livestock) %>% summarise(MERtotal = sum(Value, na.rm = TRUE))
  
  # explore energy requirements
  MERtotalYr_wide <- MERtotalYr %>% pivot_wider(names_from = Livestock, values_from = MERtotal) %>% select(Region, Season, Bull, Cow, Steer, Heifer, Calf, Sheep, Goat, Lamb, Kid)
  write_csv(MERtotalYr_wide, paste0(Results_dir, "/MERtotal_MJday.csv"), append = FALSE)
  
  #MER totals
  for(i in Regions){
    for(j in Seasons){
      
      #param_ME <- param_ME %>% filter(Region %in% c(i, "All"), Season %in% c(j, "All"))
      MERsSelected <- MERsYr %>% filter(Region == i, Season == j) # for use later
      MERtotalSelected <- MERtotalYr %>% filter(Region == i, Season == j) # MERs summed up
      
      if(j == "WS" & i == "Dry Savannah"){
        ##Total pop requirement calcs
        MERtotalYr_WS_Dry_Savannah_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, ECORegions, periurban, wetSSN) { 
          ifelse(ECORegions==1 & periurban ==0, #1 is the Dry Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Sah"
                   (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
                 ,NA)#Close if
        })
        
        MERtotalYr_WS_Dry_Savannah_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, ECORegions, periurban, wetSSN) { 
          ifelse(ECORegions ==1 & periurban ==0, #1 is the Dry Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
                   (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
                 ,NA)#Close if
        })
        
      }else if (j == "DS" & i == "Dry Savannah"){
        MERtotalYr_DS_Dry_Savannah_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==1 & periurban ==0, #1 is the Dry Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
        
        MERtotalYr_DS_Dry_Savannah_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==1 & periurban ==0, #1 is the Dry Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                   ,NA)#Close if
          
        })
      }else if(j == "WS" & i == "Wet Savannah"){
        MERtotalYr_WS_Wet_Savannah_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, ECORegions, periurban, wetSSN) { #!Including pregnancy
          ifelse(ECORegions ==3 & periurban == 0, #3 is the Wet Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Sah"
                   (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
                 ,NA)#Close if
        })
        
        MERtotalYr_WS_Wet_Savannah_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, ECORegions, periurban, wetSSN) { #! including pregnancy
          ifelse(ECORegions ==3 & periurban == 0, #3 is the Wet Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
                   (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
                 ,NA)#Close if
        })
        
      }else if(j == "DS" & i == "Wet Savannah"){
        MERtotalYr_DS_Wet_Savannah_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==3 & periurban == 0, #3 is the Wet Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
        
        MERtotalYr_DS_Wet_Savannah_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==3 & periurban == 0, #3 is the Wet Savannah
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
        
        
      }else if(j == "WS" & i == "Forest"){
        MERtotalYr_WS_Forest_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, ECORegions, periurban, wetSSN) { #!Including pregnancy
          ifelse(ECORegions ==2 & periurban == 0, #2 is the Forest
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # & param_ME$Region == "Sah"
                   (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
                 ,NA)#Close if
        })
        
        MERtotalYr_WS_Forest_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, ECORegions, periurban, wetSSN) { #! including pregnancy
          ifelse(ECORegions ==2 & periurban == 0, #2 is the Forest
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERp_cow_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
                   (MERp_cow_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
                 ,NA)#Close if
        })
        
      }else if(j == "DS" & i == "Forest"){
        MERtotalYr_DS_Forest_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==2 & periurban == 0, #2 is the Forest
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] * 
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
        
        MERtotalYr_DS_Forest_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, ECORegions, periurban, drySSN) { 
          ifelse(ECORegions ==2 & periurban == 0, #2 is the Forest
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
 
      }else if(j=="WS"&i=="Periurban"){
        MERtotalYr_WS_Periurban_cattle <- lapp(lv[[c(1,4,5,6)]], fun = function(cattle, ECORegions, periurban, wetSSN) { #!Including pregnancy
          ifelse(periurban == 1, 
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      wetSSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERp_cow_fullPreg * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"])
                 ,NA)#Close if
        })
        
        MERtotalYr_WS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,6)]], fun = function(sheep, goats, ECORegions, periurban, wetSSN) { #! including pregnancy
          ifelse(periurban == 1, 
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                    wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      wetSSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      wetSSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) + # & param_ME$Region == i
                   (MERp_shoat_fullPreg * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]) +
                   (MERp_shoat_fullPreg * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"])
                 ,NA)#Close if
        })
        
      }else if(j=="DS"&i=="Periurban"){
        MERtotalYr_DS_Periurban_cattle <- lapp(lv[[c(1,4,5,7)]], fun = function(cattle, ECORegions, periurban, drySSN) { 
          ifelse(periurban == 1, 
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Bull"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Bull" & param_ME$Region == i]*
                    drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Steer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Steer" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Calf"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Calf" & param_ME$Region == i]*
                      drySSN) +
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Heifer"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Heifer" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Cow"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Cow"]) * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Cow"] * cattle * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Cow" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"] *
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2) # # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
        
        MERtotalYr_DS_Periurban_shoats <- lapp(lv[[c(2,3,4,5,7)]], fun = function(sheep, goats, ECORegions, periurban, drySSN) { 
          ifelse(periurban == 1,
                 (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Lamb"] * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Lamb" & param_ME$Region == i]*
                     drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Sheep"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Sheep"]) * sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Sheep"]* sheep * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Sheep" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2) + # Weighted by fertility rate and assumed to be half in DS and half in WS
                   (MERtotalSelected$MERtotal[MERtotalSelected$Livestock=="Kid"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Kid" & param_ME$Region == i]*
                      drySSN) +
                   ((MERsSelected$Value[MERsSelected$MERx=="MERm"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERg"&MERsSelected$Livestock=="Goat"] + MERsSelected$Value[MERsSelected$MERx=="MERt"&MERsSelected$Livestock=="Goat"]) * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      drySSN) +
                   (MERsSelected$Value[MERsSelected$MERx=="MERl"&MERsSelected$Livestock=="Goat"] * goats * param_ME$value[param_ME$Variable=="HS" & param_ME$name == "Goat" & param_ME$Region == i]*
                      param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
                      param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2) # Weighted by fertility rate and assumed to be half in DS and half in WS
                 ,NA)#Close if
        })
      }
    }
  }
  
  ##############
  #Total requirements in MJ
  #MERall <- sum(eval(parse(text = ls(pattern = "MERtotalYr"))), na.rm = T)
  #8.3 is a conversion from DM to ME
  #365 = annualised. 180 = assuming only working half of the year
  MERhorse <- (((87/1000) * (350^0.75)) * 8.3 * lvHorse * 365) +  (0.9*(((87/1000) * (350^0.75)) * 8.3 * lvHorse * 180))
  
  #Intake calculation from https://books.google.co.uk/books?hl=en&lr=&id=rlBfYgLiqtwC&oi=fnd&pg=PA64&dq=horse+feed+requirements+ME+DM&ots=SjcNMBUJ_o&sig=MABvL3RGWr6J-TZMw8MpCwmwLwU&redir_esc=y#v=onepage&q=horse%20feed%20requirements%20ME%20DM&f=false
  #liveweight of African Donkeys from Nininahazwe et al., 2017 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5682267/
  MERdonkey <- (((87/1000) * (126^0.75)) * 8.3 * lvDonkey * 365) +  (0.9*(((87/1000) * (126^0.75)) * 8.3 * lvDonkey * 180))
  terra::writeRaster(sum(MERhorse, MERdonkey, na.rm = T), paste0(spatialDir, "/outputs/horseDonkeyMER_MJ_", year, ".tif"), overwrite = T)
  
  MERcattle <- sum(MERtotalYr_DS_Dry_Savannah_cattle, MERtotalYr_DS_Wet_Savannah_cattle, MERtotalYr_DS_Forest_cattle, MERtotalYr_DS_Periurban_cattle, MERtotalYr_WS_Dry_Savannah_cattle, MERtotalYr_WS_Wet_Savannah_cattle, MERtotalYr_WS_Forest_cattle, MERtotalYr_WS_Periurban_cattle, na.rm = T)
  terra::writeRaster(MERcattle, paste0(spatialDir, "/outputs/cattleMER_MJ_", year, ".tif"), overwrite = T)
  
  MERshoats <- sum(MERtotalYr_DS_Dry_Savannah_shoats, MERtotalYr_DS_Wet_Savannah_shoats, MERtotalYr_DS_Forest_shoats, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Dry_Savannah_shoats, MERtotalYr_WS_Wet_Savannah_shoats, MERtotalYr_WS_Forest_shoats, MERtotalYr_WS_Periurban_shoats, na.rm = T)
  terra::writeRaster(MERshoats, paste0(spatialDir, "/outputs/shoatsMER_MJ_", year, ".tif"), overwrite = T)
  
  MERall <- sum(MERhorse, MERdonkey, MERtotalYr_DS_Dry_Savannah_cattle, MERtotalYr_DS_Dry_Savannah_shoats, MERtotalYr_DS_Wet_Savannah_cattle, MERtotalYr_DS_Wet_Savannah_shoats, MERtotalYr_DS_Periurban_cattle, MERtotalYr_DS_Periurban_shoats, MERtotalYr_WS_Dry_Savannah_cattle, MERtotalYr_WS_Dry_Savannah_shoats, MERtotalYr_WS_Wet_Savannah_cattle, MERtotalYr_WS_Wet_Savannah_shoats, MERtotalYr_WS_Periurban_cattle, MERtotalYr_WS_Periurban_shoats, na.rm = T)
  terra::writeRaster(MERall, paste0(spatialDir, "/outputs/livestockMER_MJ_", year, ".tif"), overwrite = T)
  
  cat("Completed: ",  "Mean Energy Requirements --", year, "\n")
  
}

