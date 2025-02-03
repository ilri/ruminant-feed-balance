gc()
rm(list=ls())
# Map adequacy
# Author: Simon Fraval
# Last modified by John Mutua on 20/11/2024

# avoid scientific notation
options(scipen = 999)

# # Install required packages
# install.packages("raster")
# install.packages("sf")
# install.packages("exactextractr")
# install.packages("rnaturalearth")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("ggtext")
# install.packages("ggsci")
# install.packages("extrafont")

# Load libraries
library(raster)
#library(stars)
library(sf)
library(exactextractr)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggtext)
library(ggsci)
library(extrafont)
library(stars)
loadfonts(device = "all")
#options(scipen=0, digits=6)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
plotsDir <- paste0(root, "/src/3Balance-estimates/", country, "/Plots"); dir.create(plotsDir, F, T)

##Set plot theme
themeLabs <- theme(text=element_text(family="serif"),
  plot.title.position = "plot",
  plot.title = element_textbox_simple(
    size = 8,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0),
    #fill = "cornsilk"
  ),
  plot.subtitle = element_textbox_simple(
    size = 8,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0),
    #fill = "cornsilk"
  ),
  plot.caption = element_textbox_simple(
    size = 8,
    width = NULL,
    padding = margin(0, 0, 0, 0),
    margin = margin(0, 0, 0, 0),
    #linetype = 1,
    #r = grid::unit(8, "pt"),
    #fill = "azure1"
  ),
  legend.title = element_textbox_simple(
    size = 8,
    width = NULL,
    padding = margin(0, 0, 0, 0),
    margin = margin(0, 0, 0, 0),
  ),
  axis.text = element_blank(),
  strip.background = element_blank(),
  panel.grid.major = element_line(colour = "transparent") #remove map grid lines#, 

)

######
##Feed timeseries breakdown 
tsSum <- read.csv(paste0(Results_dir, "/disaggregated_timeseries.csv"), stringsAsFactors = F)

#@ There is an overestimation of post-harvest growth in the Sahel - due to misclassification of river sides (etc) as cropping - with short growing period and long dry season.
#Correction by reallocating post-harvest growth to grass
tmp <- tsSum[tsSum$zone == "(Agro)pastoral sahel",]
tmp$grassME_mean <- tmp$grassME_mean + (tmp$afterME_mean * 0.94)
tmp$grassME_min <- tmp$grassME_min + (tmp$afterME_min * 0.94)
tmp$grassME_max <- tmp$grassME_max + (tmp$afterME_max * 0.94)
tmp$afterME_mean <- (tmp$afterME_mean * 0.06)
tmp$afterME_min <- (tmp$afterME_min * 0.06)
tmp$afterME_max <- (tmp$afterME_max * 0.06)

tsSum <- rbind(tsSum[!tsSum$zone == "(Agro)pastoral sahel",], tmp)

tsSum_plot <- pivot_longer(dplyr::select(tsSum, zone, year, cropME_mean, grassME_mean, browseME_mean, afterME_mean), cols = -c(zone, year))
lower <- pivot_longer(dplyr::select(tsSum, zone, year, cropME_min, grassME_min, browseME_min, afterME_min), cols = -c(zone, year), values_to = "lower")
upper <- pivot_longer(dplyr::select(tsSum, zone, year, cropME_max, grassME_max, browseME_max, afterME_max), cols = -c(zone, year), values_to = "upper")
tsSum_plot <- cbind(tsSum_plot, dplyr::select(lower, lower))
tsSum_plot <- cbind(tsSum_plot, dplyr::select(upper, upper))

tsSum_plot$value <- tsSum_plot$value/1000000
tsSum_plot$lower <- tsSum_plot$lower/1000000
tsSum_plot$upper <- tsSum_plot$upper/1000000

tsSum_plot <- transform(tsSum_plot, zone=factor(zone,levels=c("(Agro)pastoral sahel", "Central mixed", "Forest mixed", "Northern mixed", "Southern mixed")))
tsSum_plot <- transform(tsSum_plot, name=factor(name,levels= c("grassME_mean", "cropME_mean", "browseME_mean", "afterME_mean")))
Fig1 <- ggplot(tsSum_plot[tsSum_plot$year == 2023,], aes(name, value, fill = name)) + geom_col(position = "identity") + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") +  scale_fill_manual(name = "", labels = c("Grass", "Crop residue", "Browse", "Other biomass"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif", size = 12), axis.text.x=element_blank (), strip.background = element_blank()) + facet_wrap(~zone, ncol = 5) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave(paste0(plotsDir, "/NGAFig1_1000.tiff"), Fig1, device = "tiff", dpi = 1000, width=90 * (14/5), height=20 * (14/5), units = "mm")

tsSum <- tsSum %>% rowwise() %>% mutate(totalME_mean = sum(cropME_mean, grassME_mean, browseME_mean, afterME_mean))
tsSum <- tsSum %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean)

tsSum_plot2 <- pivot_longer(dplyr::select(tsSum, zone, year, cropMEprop_mean, grassMEprop_mean, browseMEprop_mean, afterMEprop_mean), cols = -c(zone, year))
tsSum_plot2 <- transform(tsSum_plot2, zone=factor(zone,levels=c("(Agro)pastoral sahel", "Central mixed", "Forest mixed", "Northern mixed", "Southern mixed")))
tsSum_plot2 <- transform(tsSum_plot2, name=factor(name,levels=c("grassMEprop_mean", "cropMEprop_mean", "browseMEprop_mean", "afterMEprop_mean")))
SI1 <- ggplot(tsSum_plot2, aes(year, value, colour = name, fill = name)) + geom_line() + scale_x_continuous(breaks=c(2020, 2021, 2022, 2023)) + scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8)) + ylab("Proportion of energy") + xlab("Year") +  scale_colour_manual(name = "", labels = c("Grass", "Crop residue", "Browse", "Other biomass"), values = c("cropMEprop_mean" = "#F8CA02", "grassMEprop_mean" = "#008D1F", "afterMEprop_mean" = "#000000", "browseMEprop_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif", size = 12), strip.background = element_blank(), panel.spacing = unit(6, "mm")) + facet_wrap(~zone, nrow = 1) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave(paste0(plotsDir, "/NGASI1.tiff"), SI1, device = "tiff", dpi = 300, width=90 * (14/5), height=20 * (14/5), units = "mm")

#Table 2
zone_area <- readr::read_csv(paste0(Results_dir, "/area_zones.csv")) %>% rename(zone=ECOZone) %>% dplyr::select(zone, area_hectares) %>% 
  mutate(zone = case_when(zone %in% c("(Agro)pastoral sahel", "Northern mixed") ~ "dry_sav", 
                          zone == "Forest mixed" ~ "for", 
                          zone %in% c("Central mixed", "Southern mixed") ~ "wet_sav", TRUE ~ zone)) %>% 
  group_by(zone) %>% summarise(area_hectares=sum(area_hectares, na.rm=TRUE))

feedDM <- read.csv(paste0(Results_dir, "/cropME_region.csv"), stringsAsFactors = F) %>% dplyr::select(region, cropDM, grassDM, browseDM, afterDM) %>% 
  rowwise() %>% mutate(FeedDM = sum(cropDM, grassDM, browseDM, afterDM)) %>% 
  mutate(zone = case_when(region == "Dry Savannah" ~ "dry_sav", 
                          region == "Forest" ~ "for", 
                          region == "Wet Savannah" ~ "wet_sav", TRUE ~ region)) %>% dplyr::select(zone, FeedDM) %>% 
  left_join(zone_area, by="zone") %>% 
  mutate(yieldHa = (FeedDM/area_hectares)/1000, #convert t/ha 1000
         FeedDM = FeedDM/1000000000) %>% #convert to million tonnes /1000000000
  dplyr::select(-area_hectares)
  
x <- dplyr::select(tsSum[tsSum$year == 2023,], c(zone, totalME_mean, cropME_mean, grassME_mean, browseME_mean, afterME_mean))
x$eco <- c("wet_sav", "for", "dry_sav", "wet_sav", "dry_sav")
x <- group_by(x, eco)
x <- summarise_all(dplyr::select(x, -zone), sum)
x<-rename(x, zone=eco)
x <- x %>% left_join(feedDM, by="zone")
x <- x %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean)
x <- dplyr::select(x, c(zone, FeedDM, yieldHa, totalME_mean, grassMEprop_mean, cropMEprop_mean, browseMEprop_mean, afterMEprop_mean)) %>% 
  mutate(totalME_mean=totalME_mean/1000000000000, across(5:ncol(.), ~ . * 100),
         zone=case_when(zone=="wet_sav"~"Wet Savannah", zone == "for" ~ "Forest", zone == "dry_sav" ~ "Dry Savannah", TRUE ~ zone),
         across(where(is.numeric), ~ round(., 2))) %>% 
  rename(Region=zone,`DM (Mt)`=FeedDM, `Yield (t/ha)`=yieldHa, `Energy supply (ME EJ*)`=totalME_mean, `Grass (%ME)`=grassMEprop_mean, `Crop residue (%ME)`=cropMEprop_mean,`Postharvest growth (%ME)`=afterMEprop_mean,`Browse (%ME)`=browseMEprop_mean)
write.csv(x, paste0(Results_dir, "/Annual_feedAvailability.csv"), row.names = FALSE)

######
##Livestock feed adequacy timeseries breakdown
tsSumReg <- read.csv(paste0(Results_dir, "/totals_timeseries_region.csv"))

tsSumReg_mean <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeq_2020, adeq_2021, adeq_2022, adeq_2023), cols = -c(NAME_1))
tsSumReg_mean <- separate(tsSumReg_mean, name, c("name", "year"), "_")

tsSumRegMin <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeqMin_2020, adeqMin_2021, adeqMin_2022, adeqMin_2023), cols = -c(NAME_1))
tsSumRegMax <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeqMax_2020, adeqMax_2021, adeqMax_2022, adeqMax_2023), cols = -c(NAME_1))

tsSumReg_plot <- cbind(tsSumReg_mean, select(tsSumRegMin, lower = value))
tsSumReg_plot <- cbind(tsSumReg_plot, select(tsSumRegMax, upper = value))

tsSumReg_plot <- transform(tsSumReg_plot, NAME_1=factor(NAME_1,levels=c("(Agro)pastoral sahel", "Central mixed", "Forest mixed", "Northern mixed", "Southern mixed")))

Fig2 <- ggplot(tsSumReg_plot, aes(year, value, group = NAME_1)) + geom_hline(yintercept = 1, linetype = 2, colour = "grey") + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available / required") + xlab("Year") + labs(colour = "") + scale_x_discrete(breaks=c("2020", "2021", "2022", "2023")) + scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) + scale_colour_lancet() + theme_classic() +  theme(text=element_text(family="serif", size = 12), strip.background = element_blank()) + facet_wrap(~NAME_1, ncol = 5)
ggsave(paste0(plotsDir, "/NGAFig2_1000.tiff"), Fig2, device = "tiff", dpi = 1000, width=90 * (14/5), height=25 * (14/5), units = "mm")

#plot by aggregation zones
aggregation_zones <- c("region", "state")
for(aggregation_zone in aggregation_zones){
  
  tsSumReg <- read.csv(paste0(Results_dir, "/totals_timeseries_", aggregation_zone,".csv"))
  
  if(aggregation_zone =="region"){
    zones <- st_read(paste0(spatialDir, "/intermediate/zones.gpkg"))
    zones <- zones %>% left_join(tsSumReg, by = c("ECOZone"="NAME_1")) %>% rename(NAME_1=ECOZone)
    tsSumReg_mean <- pivot_longer(dplyr::select(zones, NAME_1, adeq_2020, adeq_2021, adeq_2022, adeq_2023), cols = -c(NAME_1, geom))
  }else if(aggregation_zone =="state"){
    zones <- st_read(paste0(spatialDir, "/inputs/aoi1.shp"))
    zones <- zones %>% left_join(tsSumReg, by = "NAME_1")
    tsSumReg_mean <- pivot_longer(dplyr::select(zones, NAME_1, adeq_2020, adeq_2021, adeq_2022, adeq_2023), cols = -c(NAME_1, geometry))
    }

  tsSumReg_mean <- separate(tsSumReg_mean, name, c("name", "year"), "_") %>% tidyterra::filter(year == 2023)
  if(aggregation_zone=="region"){
    tsSumReg_plot <- transform(tsSumReg_mean, NAME_1=factor(NAME_1,levels=c("(Agro)pastoral sahel", "Northern mixed", "Southern mixed", "Central mixed", "Forest mixed")))
  }else{tsSumReg_plot <- transform(tsSumReg_mean, NAME_1=factor(NAME_1,levels=unique(tsSumReg_mean$NAME_1)))}
  
  
  Fig3 <- ggplot(data = tsSumReg_plot) +
    geom_sf(aes(fill = value)) +
    #facet_wrap(~ year, ncol = 4) +
    scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 1, limits = c(floor(min(tsSumReg_plot$value, na.rm = TRUE)), ceiling(max(tsSumReg_plot$value, na.rm = TRUE))), space = "Lab", name = "", breaks = c(floor(min(tsSumReg_plot$value, na.rm = TRUE)), 1, ceiling(max(tsSumReg_plot$value, na.rm = TRUE))), labels = c("Negative balance", "0", "Positive balance")) +
    labs(fill = "") + 
    coord_sf(xlim = c(2.1, 15.1), ylim = c(3.7, 14.3), expand = FALSE) + 
    theme_bw() +  
    theme(panel.background = element_rect(fill = "white"), text = element_text(family = "arial", size = 12), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white"))
  ggsave(paste0(plotsDir, "/NGAFig3_1000_", aggregation_zone, ".tiff"), Fig3, device = "tiff", dpi = 1000, width=90 * (14/5), height=25 * (14/5), units = "mm")
}

######
##Mapping#
sfZones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate/zones.gpkg"))

feed_grass_MJ <- raster(paste0(spatialDir, "/outputs/Feed_grass_MJ2023.tif"))
feed_crop_MJ <- raster(paste0(spatialDir, "/outputs/Feed_crop_MJ2023.tif"))
feed_browse_MJ <- raster(paste0(spatialDir, "/outputs/Feed_browse_MJ2023.tif"))
feed_after_MJ <- raster(paste0(spatialDir, "/outputs/Feed_after_MJ2023.tif"))
feed_total_MJ <- sum(feed_grass_MJ, feed_crop_MJ, feed_browse_MJ, feed_after_MJ, na.rm =T)
cattleIntake_model_MJ_2020 <- raster(paste0(spatialDir, "/outputs/cattleMER_MJ_2020.tif"))
shoatsIntake_model_MJ_2020 <- raster(paste0(spatialDir, "/outputs/shoatsMER_MJ_2020.tif"))
lvstIntake_MJ <- sum(cattleIntake_model_MJ_2020, shoatsIntake_model_MJ_2020, na.rm = T)

aoi1 <- st_read(paste0(spatialDir, "/inputs/aoi1.shp"))
feed_total_MJ <- mask(feed_total_MJ, aoi1)

lvstIntake_MJ <- mask(lvstIntake_MJ, aoi1)
resLvst <- res(lvstIntake_MJ)[1]
lvstIntake_MJ <- resample(lvstIntake_MJ, feed_total_MJ)/(resLvst/res(feed_total_MJ)[1])^2
lvstIntake_MJ <- reclassify(lvstIntake_MJ, c(-Inf, 0, 0))

countries <- ne_countries(scale = 50, type = "countries", returnclass = "sf")

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_sf(data = sfZones, aes(fill = ECOZone)) + coord_sf(xlim = c(2.1, 15.1), ylim = c(3.7, 14.3), expand = FALSE) + labs(fill = "Region") #+ themeLabs + theme(legend.position="bottom")   

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(feed_total_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("") + labs(fill = expression("Tj"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 1), low = "#CDDF4A", high = "#0BAE1C", na.value = NA) + coord_sf(xlim = c(2.1, 15.1), ylim = c(3.7, 14.3), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")    

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(lvstIntake_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("") + labs(fill = expression("Tj"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 0.5), breaks = c(0, 0.25, 0.5), low = "#FFFFFF", high = "#F9A908", na.value = NA) + coord_sf(xlim = c(2.1, 15.1), ylim = c(3.7, 14.3), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")

