# Extract results at state

library(terra)
#library(stars)
library(sf)
library(exactextractr)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(stars)
library(raster)
library(ggplot2)

# avoid scientific notation
options(scipen = 999)

#rasterOptions(tmpdir = EDDIE_TMP)
rasterOptions(tmpdir="/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
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
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
plotsDir <- paste0(root, "/src/3Balance-estimates/", country, "/Plots")

# Load samples LGAs
sampldLGAs <- vect(paste0(spatialDir, "/inputs/FeedBal_SmplFrmStlmn_241126.gpkg"))

sampldLGAs <- sampldLGAs %>% 
  aggregate(., by = c("statename", "lganame", "state_id", "lga_id")) %>% 
  tidyterra::select(statename, state_id, lganame, lga_id) %>% 
  as.data.frame()

sampldLGAs_List <- unique(sampldLGAs$lganame)

# Loading Nigeria admin boundaries
aoi2 <- read_sf(paste0(spatialDir, "/inputs/aoi2.shp"))
aoi1 <- read_sf(paste0(spatialDir, "/inputs/aoi1.shp"))
aoi0 <- read_sf(paste0(spatialDir, "/inputs/aoi0.shp"))

aoi2 <- aoi2 %>% 
  tidyterra::filter(NAME_2 %in% sampldLGAs_List) %>% 
  left_join(sampldLGAs, by = c("NAME_1" = "statename", "NAME_2" = "lganame")) %>% 
  tidyterra::select(NAME_1, state_id, NAME_2, lga_id) %>% 
  rename(statename=NAME_1, lganame=NAME_2)

aoi2$area_km2 <- expanse(aoi2, unit = "km")

#aoi2 %>% as.data.frame()

# extract DM
feed_list <- c("crop", "grass", "browse", "after")
tFeed <- rast(paste0(spatialDir, "/outputs/Feed_", feed_list, "DM2023.tif"))
feedDM <- exact_extract(tFeed, aoi2, "sum")
feedDM_stats <- cbind(LGA = aoi2$lganame, feedDM)
colnames(feedDM_stats)[2:5] <- c("cropDM_kg", "grassDM_kg", "browseDM_kg", "afterDM_kg")
write.csv(feedDM_stats, paste0(Results_dir, "/FeedDM_LGA.csv"), row.names = FALSE)

# State
feed_list <- c("crop", "grass", "browse", "after")
tFeed <- rast(paste0(spatialDir, "/outputs/Feed_", feed_list, "DM2023.tif"))
feedDM <- exact_extract(tFeed, aoi1, "sum")
feedDM_stats <- cbind(State = aoi1$NAME_1, feedDM)
colnames(feedDM_stats)[2:5] <- c("Crop residue", "Grass", "Browse", "Other biomass")
FeedBasket <- feedDM_stats %>% 
  rowwise() %>% 
  mutate(across(`Crop residue`:`Other biomass`, ~ . / sum(c_across(`Crop residue`:`Other biomass`)) * 100)) %>% 
  mutate_if(is.numeric, round, 1)
write.csv(FeedBasket, paste0(Results_dir, "/FeedBasket_State.csv"), row.names = FALSE)

# National
feedDM <- exact_extract(tFeed, aoi0, "sum")
feedDM_stats <- cbind(LGA = aoi0$COUNTRY, feedDM)
colnames(feedDM_stats)[2:5] <- c("Crop residue", "Grass", "Browse", "Other biomass")
FeedBasket <- feedDM_stats %>% 
  rowwise() %>% 
  mutate(across(`Crop residue`:`Other biomass`, ~ . / sum(c_across(`Crop residue`:`Other biomass`)) * 100)) %>% 
  mutate_if(is.numeric, round, 1)
write.csv(FeedBasket, paste0(Results_dir, "/FeedBasket_Country.csv"), row.names = FALSE)

# # extract diet proportions
# feed_list <- c("crop", "grass", "browse", "after")
# tFeed <- rast(paste0(spatialDir, "/outputs/Feed_", feed_list, "DM2023.tif"))
# feedDM <- exact_extract(tFeed, aoi2, "sum")
# feedDM_stats <- cbind(LGA = aoi2$NAME_2, feedDM)
# colnames(feedDM_stats)[2:4] <- c("cropDM_kg", "grassDM_kg", "browseDM_kg")
# write.csv(feedDM_stats, paste0(Results_dir, "/FeedDM_LGA.csv"), row.names = FALSE)

# extract cropping and non-cropping days
season_list <- c("cropping", "dry")
tSeasons <- rast(paste0(spatialDir, "/outputs/", season_list, "Days_2023.tif"))
seasonLen <- exact_extract(tSeasons, aoi2, "mean")
seasonLen_stats <- cbind(LGA = aoi2$lganame, seasonLen)
colnames(seasonLen_stats)[2:3] <- c("croppingDays", "dryDays")
seasonLen_stats <- seasonLen_stats %>% mutate_if(is.numeric, round, digits = 0)
write.csv(seasonLen_stats, paste0(Results_dir, "/SeasonLength.csv"), row.names = FALSE)

# extract livestock population
lvCategories <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Lamb", "Goat", "Kid")
tLv <- rast(paste0(spatialDir, "/inputs/GLW4/Dissag_GLW4_2020_",lvCategories,".tif"))
names(tLv) <- c("Bull", "Cow", "Steer", "Heifer", "Calf", "Sheep", "Lamb", "Goat", "Kid")
tSumLv <- exact_extract(tLv, aoi2, "sum")
tSumLv <- cbind(LGA = aoi2$NAME_2, tSumLv)
colnames(tSumLv)[2:10] <- c("Bulls", "Cows", "Steers", "Heifers", "Calves", "Sheep", "Lambs", "Goats", "Kids")
tSumLv <- tSumLv %>% mutate_if(is.numeric, round, digits = 0)
# tSumLv$Cattle <- tSumLv$Bulls+tSumLv$Cows+tSumLv$Steers+tSumLv$Heifers+tSumLv$Calves
# sum(tSumLv$Cattle)
write.csv(tSumLv, paste0(Results_dir, "/LivestockPopulation.csv"), row.names = FALSE)

# extract land use types and proportions
luClasses <- c("grass", "shrub", "tree", "crops")
tLU <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Nigeria/Feed_DrySeason/LandUse/LU",luClasses,"300.tif"))
#tLUcrop <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Nigeria/Feed_DrySeason/LandUse/LUcrops300DEA.tif"))
#tLU <- c(tLU, tLUcrop)
tMeanLU <- exact_extract(tLU, aoi2, "mean")
tMeanLU <- cbind(LGA = aoi2$NAME_2, tMeanLU)
colnames(tMeanLU)[2:5] <- c("grass", "shrub", "tree", "crop")
tMeanLU <- tMeanLU %>% mutate(across(where(is.numeric), ~ . * 100))
write.csv(tMeanLU, paste0(Results_dir, "/LandUseProportion.csv"), row.names = FALSE)

# extract burning incidences
luClasses <- c("Crop", "Grass")
tBurn <- rast(paste0(root, "/src/3Balance-estimates/Nigeria/SpatialData/inputs/Burned/2023/burn",luClasses,"Months.tif"))
tBurn <- exact_extract(tBurn, aoi2, "frac")
tBurnEvents <- cbind(LGA = aoi2$NAME_2, tBurn)
tBurnEvents <- tBurnEvents %>%
  mutate(burnEventsCrop = case_when(
    frac_0.burnCropMonths >= 1 ~ 0,
    frac_1.burnCropMonths > 0 | 
      frac_2.burnCropMonths > 0 | 
      frac_3.burnCropMonths > 0 | 
      frac_4.burnCropMonths > 0 ~ 1,
    TRUE ~ NA_real_),
  burnEventsGrass = case_when(
    frac_0.burnGrassMonths >= 1 ~ 0,
    frac_1.burnGrassMonths > 0 | 
      frac_2.burnGrassMonths > 0 | 
      frac_3.burnGrassMonths > 0 | 
      frac_4.burnGrassMonths > 0 ~ 1,
    TRUE ~ NA_real_)) %>% 
  dplyr::select(LGA, burnEventsCrop, burnEventsGrass)
write.csv(tBurnEvents, paste0(Results_dir, "/BurnEventsBoolean.csv"), row.names = FALSE)


pathDMP <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Nigeria/Feed_DrySeason/DMP")
filesDMP <- list.files(path = pathDMP, pattern = paste0("RT6_", "2023", ".*\\.tif$"), full.names = TRUE)
stDMP <- rast(filesDMP)

feedDMP <- exact_extract(stDMP, aoi1, "sum")
feedDMP_stats <- cbind(State = aoi1$NAME_1, feedDMP)
feedDMP_stats <- feedDMP_stats %>% 
  transmute(
    Jan = feedDMP_stats[[2]] + feedDMP_stats[[3]] + feedDMP_stats[[4]],
    Feb = feedDMP_stats[[5]] + feedDMP_stats[[6]] + feedDMP_stats[[7]],
    Mar = feedDMP_stats[[8]] + feedDMP_stats[[9]] + feedDMP_stats[[10]],
    Apr = feedDMP_stats[[11]] + feedDMP_stats[[12]] + feedDMP_stats[[13]],
    May = feedDMP_stats[[14]] + feedDMP_stats[[15]] + feedDMP_stats[[16]],
    Jun = feedDMP_stats[[17]] + feedDMP_stats[[18]] + feedDMP_stats[[19]],
    Jul = feedDMP_stats[[20]] + feedDMP_stats[[21]] + feedDMP_stats[[22]],
    Aug = feedDMP_stats[[23]] + feedDMP_stats[[24]] + feedDMP_stats[[25]],
    Sep = feedDMP_stats[[26]] + feedDMP_stats[[27]] + feedDMP_stats[[28]],
    Oct = feedDMP_stats[[29]] + feedDMP_stats[[30]] + feedDMP_stats[[31]],
    Nov = feedDMP_stats[[32]] + feedDMP_stats[[33]] + feedDMP_stats[[34]],
    Dec = feedDMP_stats[[35]] + feedDMP_stats[[36]] + feedDMP_stats[[37]]) %>% 
  slice(-1) %>% 
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

feedDMP_stats_values <- as.numeric(feedDMP_stats[1, ])

# Scale to 1–10
scaled_values <- ((feedDMP_stats_values - min(feedDMP_stats_values, na.rm = TRUE)) /
                    (max(feedDMP_stats_values, na.rm = TRUE) - min(feedDMP_stats_values, na.rm = TRUE))) * 9 + 1

# Create a new dataframe with month names
feedAvailability <- data.frame(
  Month = names(feedDMP_stats),
  Scaled_Value = round(scaled_values, 2))


write.csv(feedAvailability, paste0(Results_dir, "/feedAvailability.csv"), row.names = FALSE)

feedAvailability$Month <- factor(feedAvailability$Month, levels = month.abb)

feedAvailability_plot  <- ggplot(feedAvailability, aes(x = Month, y = Scaled_Value, group = 1)) +
  geom_line(color = "steelblue", size = 1) +    
  geom_point(color = "darkred", size = 2) +     
  labs(title = "",
       x = "Month", 
       y = "Feed Availability") +
  scale_y_continuous(breaks = seq(1, 10, 2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 12, face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        #panel.grid.minor = element_blank(), 
        #panel.grid.major = element_blank(),
        #legend.key.height = unit(2, "cm"), 
        #legend.key.width = unit(0.5, "cm"),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedAvailability_plot.tiff"), feedAvailability_plot, device = "tiff", dpi = 1000, width=90 * (14/5), height=20 * (14/5), units = "mm")

lvCategories <- c("CTL", "GTS", "SHP")
lvCountLs <- list()
for(lvCategory in lvCategories){
  
  # extract livestock population
  lvCount <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2023.D-DA.",lvCategory,".tif"))
  lvCountSum <- exact_extract(lvCount, aoi1, "sum") %>% as.data.frame()
  lvCountSum <- cbind(State = aoi1$NAME_1, lvCountSum) %>% 
    mutate_if(is.numeric, round, digits = 0)
  colnames(lvCountSum) <- c("STATE", paste0(lvCategory))

  lvCountLs[[lvCategory]] <- lvCountSum
  
  #write.csv(lvCountSum, paste0(Results_dir, "/", lvCategory, "_Population.csv"), row.names = FALSE)
  
}

lvCountSum <- Reduce(function(x, y) left_join(x, y, by = "STATE"), lvCountLs)
write.csv(lvCountSum, paste0(Results_dir, "/lv2023Population.csv"), row.names = FALSE)


lvCategories <- c("CTL", "GTS", "SHP")
lvCountLs <- list()
for(lvCategory in lvCategories){
  
  # extract livestock population
  lvCount <- rast(paste0(spatialDir, "/inputs/GLW4/GLW4-2020.D-DA.",lvCategory,".tif"))
  lvCountSum <- exact_extract(lvCount, aoi1, "sum") %>% as.data.frame()
  lvCountSum <- cbind(State = aoi1$NAME_1, lvCountSum) %>% 
    mutate_if(is.numeric, round, digits = 0)
  colnames(lvCountSum) <- c("STATE", paste0(lvCategory))
  
  #lvCountLs[[lvCategory]] <- lvCountSum
  
  write.csv(lvCountSum, paste0(Results_dir, "/", lvCategory, "_2020Population.csv"), row.names = FALSE)
  
}



