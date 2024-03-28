#.libPaths(c(.libPaths()[2], .libPaths()[3]))
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
loadfonts(device = "win")
#options(scipen=0, digits=6)

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

#######
##Feed timeseries breakdown 
#tsSum <- read.csv('LivestockParams/totals_timeseries.csv')
tsSum <- read.csv('Results/disaggregated_timeseries.csv', stringsAsFactors = F)
tsSum$region <- gsub("Sahel", "(Agro)pastoral Sahel", tsSum$region)
tsSum$region <- gsub("Central", "Central mixed", tsSum$region)
tsSum$region <- gsub("Mixed crop-livestock \\(north)", "North mixed", tsSum$region)
tsSum$region <- gsub("Mixed crop-livestock \\(south)", "South mixed", tsSum$region)

#@ There is an overestimation of post-harvest growth in the Sahel - due to misclassification of river sides (etc) as cropping - with short growing period and long dry season.
#Correction by reallocating post-harvest growth to grass
tmp <- tsSum[tsSum$region == "(Agro)pastoral Sahel",]
tmp$grassME_mean <- tmp$grassME_mean + (tmp$afterME_mean * 0.94)
tmp$grassME_min <- tmp$grassME_min + (tmp$afterME_min * 0.94)
tmp$grassME_max <- tmp$grassME_max + (tmp$afterME_max * 0.94)
tmp$afterME_mean <- (tmp$afterME_mean * 0.06)
tmp$afterME_min <- (tmp$afterME_min * 0.06)
tmp$afterME_max <- (tmp$afterME_max * 0.06)

tsSum <- rbind(tsSum[!tsSum$region == "(Agro)pastoral Sahel",], tmp)

#outCropMEmean <- tsSum$cropME_mean[tsSum$year == 2019] / tsSum$cropDM[tsSum$year == 2019]
#outCropMEmin <- tsSum$cropME_min[tsSum$year == 2019] / tsSum$cropDM[tsSum$year == 2019]
#outCropMEmax <- tsSum$cropME_max[tsSum$year == 2019] / tsSum$cropDM[tsSum$year == 2019]
#outME <- data.frame(region = unique(tsSum$region), outCropMEmean, outCropMEmin, outCropMEmax)
#geomMean <- sqrt(outME$outCropMEmin*outME$outCropMEmax)
#outME$sd <- (outME$outCropMEmean/geomMean)*(sqrt((outME$outCropMEmean^2)-(geomMean^2)))
#outME
tsSum_plot <- pivot_longer(dplyr::select(tsSum, region, year, cropME_mean, grassME_mean, browseME_mean, afterME_mean), cols = -c(region, year))
lower <- pivot_longer(dplyr::select(tsSum, region, year, cropME_min, grassME_min, browseME_min, afterME_min), cols = -c(region, year), values_to = "lower")
upper <- pivot_longer(dplyr::select(tsSum, region, year, cropME_max, grassME_max, browseME_max, afterME_max), cols = -c(region, year), values_to = "upper")
tsSum_plot <- cbind(tsSum_plot, select(lower, lower))
tsSum_plot <- cbind(tsSum_plot, select(upper, upper))
#tsSum <- pivot_longer(dplyr::select(tsSum, region, year, cropME_mean, grassME_mean, browseME_mean, afterME_mean, cropME_min, grassME_min, browseME_min, afterME_min), c(lvstReq, cropME_mean, grassME_mean, browseME_mean, afterME_mean))
#tsSum <- pivot_longer(dplyr::select(tsSum, region, year, cropME_mean, grassME_mean, browseME_mean), c(lvstReq, cropME_mean, grassME_mean, browseME_mean))
tsSum_plot$value <- tsSum_plot$value/1000000
tsSum_plot$lower <- tsSum_plot$lower/1000000
tsSum_plot$upper <- tsSum_plot$upper/1000000
#ggplot(tsSum_plot, aes(year, value, colour = name)) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available (TJ ME)") + xlab("Year") +  scale_colour_manual(name = "", labels = c("Crop residue", "Grass", "Other biomass", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif", size = 14), axis.text.x=element_blank (), strip.background = element_blank()) + facet_wrap(~region, nrow = 1) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +


#ggplot(tsSum_plot[tsSum_plot$year == 2019,], aes(name, value, fill = name)) + geom_col(position = "identity") + coord_flip() + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") +  scale_fill_manual(name = "", labels = c("Crop residue", "Grass", "Other biomass", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif"), axis.text.x=element_blank ()) + facet_wrap(~region) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
tsSum_plot <- transform(tsSum_plot, region=factor(region,levels=c("(Agro)pastoral Sahel", "North mixed","Central mixed", "South mixed", "Cropping")))
#ggplot(tsSum_plot[tsSum_plot$year == 2019,], aes(name, value, fill = name)) + geom_col(position = "identity") + coord_flip() + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") +  scale_fill_manual(name = "", labels = c("Crop residue", "Grass", "Other biomass", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif"), axis.text.y=element_blank ()) + facet_wrap(~region, ncol = 1) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
tsSum_plot <- transform(tsSum_plot, name=factor(name,levels= c("grassME_mean", "cropME_mean", "browseME_mean", "afterME_mean")))
Fig1 <- ggplot(tsSum_plot[tsSum_plot$year == 2019,], aes(name, value, fill = name)) + geom_col(position = "identity") + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") +  scale_fill_manual(name = "", labels = c("Grass", "Crop residue", "Browse", "Other biomass"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif", size = 14), axis.text.x=element_blank (), strip.background = element_blank()) + facet_wrap(~region, ncol = 5) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave("BFFig1_1000.tiff", Fig1, device = "tiff", dpi = 1000, width=85 * (14/5), height=20 * (14/5), units = "mm")


tsSum <- tsSum %>% rowwise() %>% mutate(totalME_mean = sum(cropME_mean, grassME_mean, browseME_mean, afterME_mean))
tsSum <- tsSum %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean)

tsSum_plot2 <- pivot_longer(dplyr::select(tsSum, region, year, cropMEprop_mean, grassMEprop_mean, browseMEprop_mean, afterMEprop_mean), cols = -c(region, year))
tsSum_plot2 <- transform(tsSum_plot2, region=factor(region,levels=c("(Agro)pastoral Sahel", "North mixed","Central mixed", "South mixed", "Cropping")))
tsSum_plot2 <- transform(tsSum_plot2, name=factor(name,levels=c("grassMEprop_mean", "cropMEprop_mean", "browseMEprop_mean", "afterMEprop_mean")))
SI1 <- ggplot(tsSum_plot2, aes(year, value, colour = name, fill = name)) + geom_line() + scale_x_continuous(breaks=c(2015, 2017, 2019)) + scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8)) + ylab("Proportion of energy") + xlab("Year") +  scale_colour_manual(name = "", labels = c("Grass", "Crop residue", "Browse", "Other biomass"), values = c("cropMEprop_mean" = "#F8CA02", "grassMEprop_mean" = "#008D1F", "afterMEprop_mean" = "#000000", "browseMEprop_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif", size = 14), strip.background = element_blank()) + facet_wrap(~region, nrow = 1) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave("BFSI1.tiff", SI1, device = "tiff", dpi = 300, width=85 * (14/5), height=20 * (14/5), units = "mm")


#Table 2
x <- select(tsSum[tsSum$year == 2019,], c(region, totalME_mean, cropME_mean, grassME_mean, browseME_mean, afterME_mean))
x$aez <- c("sud", "sud", "sud", "sud", "sah")
x <- group_by(x, aez)
x <- summarise_all(select(x, -region), sum)
x <- x %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean)
x <- select(x, c(aez, totalME_mean, cropMEprop_mean, grassMEprop_mean, browseMEprop_mean, afterMEprop_mean))


##Livestock feed adequacy timeseries breakdown
tsSumReg <- read.csv('Results/totals_timeseries_region.csv')
#tsSumReg$NAME_1 <- gsub("Sahel", "(Agro)pastoral Sahel", tsSumReg$NAME_1)
#tsSumReg$NAME_1 <- gsub("Central", "Central mixed", tsSumReg$NAME_1)
#tsSumReg$NAME_1 <- gsub("Mixed crop-livestock \\(north)", "North mixed", tsSumReg$NAME_1)
#tsSumReg$NAME_1 <- gsub("Mixed crop-livestock \\(south)", "South mixed", tsSumReg$NAME_1)

tsSumReg_mean <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeq_2014, adeq_2015, adeq_2016, adeq_2017, adeq_2018, adeq_2019), cols = -c(NAME_1))
tsSumReg_mean <- separate(tsSumReg_mean, name, c("name", "year"), "_")
#tsSumReg$value <- tsSumReg$value

tsSumRegMin <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeqMin_2014, adeqMin_2015, adeqMin_2016, adeqMin_2017, adeqMin_2018, adeqMin_2019), cols = -c(NAME_1))
tsSumRegMax <- pivot_longer(dplyr::select(tsSumReg, NAME_1, adeqMax_2014, adeqMax_2015, adeqMax_2016, adeqMax_2017, adeqMax_2018, adeqMax_2019), cols = -c(NAME_1))

#tsSumRegMin <- separate(tsSumRegMin, name, c("name", "year"), "_")
tsSumReg_plot <- cbind(tsSumReg_mean, select(tsSumRegMin, lower = value))
tsSumReg_plot <- cbind(tsSumReg_plot, select(tsSumRegMax, upper = value))

#tsSumReg$lower[tsSumReg$name != "lvstReq"] <- tsSumReg$value[tsSumReg$name != "lvstReq"] - (tsSumReg$value[tsSumReg$name != "lvstReq"]*0.15) #!Temporary for illustration
#tsSumReg$upper[tsSumReg$name != "lvstReq"] <- tsSumReg$value[tsSumReg$name != "lvstReq"] + (tsSumReg$value[tsSumReg$name != "lvstReq"]*0.10)

#ggplot(tsSumReg, aes(year, value, group = groupingBF, colour = groupingBF)) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available / required (Tj ME)") + xlab("Year") + labs(colour = "") + scale_colour_lancet() + theme_classic() +  theme(text=element_text(family="serif")) 
#ggplot(tsSumReg_plot, aes(year, value, group = NAME_1, colour = NAME_1)) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available / required (Tj ME)") + xlab("Year") + labs(colour = "") + scale_y_continuous(limits = c(0,3), breaks = c(0,1,3,5,7,9)) + scale_colour_lancet() + theme_classic() +  theme(text=element_text(family="serif")) 
tsSumReg_plot <- transform(tsSumReg_plot, NAME_1=factor(NAME_1,levels=c("(Agro)pastoral Sahel", "North mixed","Central mixed", "South mixed", "Cropping")))
#ggplot(tsSumReg_plot, aes(year, value, group = NAME_1)) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available / required (Tj ME)") + xlab("Year") + labs(colour = "") + scale_colour_lancet() + theme_classic() +  theme(text=element_text(family="serif", size = 14), strip.background = element_blank()) + facet_wrap(~NAME_1, ncol = 1)

Fig2 <- ggplot(tsSumReg_plot, aes(year, value, group = NAME_1)) + geom_hline(yintercept = 1, linetype = 2, colour = "grey") + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available / required") + xlab("Year") + labs(colour = "") + scale_x_discrete(breaks=c("2015", "2017", "2019")) + scale_y_continuous(limits = c(0,4), breaks = c(0,1,3)) + scale_colour_lancet() + theme_classic() +  theme(text=element_text(family="serif", size = 12), strip.background = element_blank()) + facet_wrap(~NAME_1, ncol = 5)
ggsave("BFFig2_1000.tiff", Fig2, device = "tiff", dpi = 1000, width=85 * (14/5), height=20 * (14/5), units = "mm")


###
##Mapping#
sfZones <- st_read('SpatialData/intermediate/zones.gpkg')

feed_grass_MJ <- raster('SpatialData/outputs/Feed_grass_MJ2019.tif')
feed_crop_MJ <- raster('SpatialData/outputs/Feed_crop_MJ2019.tif')
feed_browse_MJ <- raster('SpatialData/outputs/Feed_browse_MJ2019.tif')
feed_after_MJ <- raster('SpatialData/outputs/Feed_after_MJ2019.tif')
feed_total_MJ <- sum(feed_grass_MJ, feed_crop_MJ, feed_browse_MJ, feed_after_MJ, na.rm =T)
cattleIntake_model_MJ_2015 <- raster('SpatialData/outputs/cattleMER_MJ_2015.tif')
shoatsIntake_model_MJ_2015 <- raster('SpatialData/outputs/shoatsMER_MJ_2015.tif')
lvstIntake_MJ <- sum(cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)

aoi1 <- st_read('SpatialData/inputs/gadm40_BFA_1.shp')
feed_total_MJ <- mask(feed_total_MJ, aoi1)

lvstIntake_MJ <- mask(lvstIntake_MJ, aoi1)
resLvst <- res(lvstIntake_MJ)[1]
lvstIntake_MJ <- resample(lvstIntake_MJ, feed_total_MJ)/(resLvst/res(feed_total_MJ)[1])^2
#lvstIntake_MJ <- disaggregate(lvstIntake_MJ, 28)

countries <- ne_countries(scale = 50, type = "countries", returnclass = "sf")

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_sf(data = sfZones, aes(fill = grouping)) + coord_sf(xlim = c(-6.5, 3.5), ylim = c(9, 15.5), expand = FALSE) + labs(fill = "Region") #+ themeLabs + theme(legend.position="bottom")   

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(feed_total_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("") + labs(fill = expression("Tj"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2), low = "#CDDF4A", high = "#0BAE1C", na.value = NA) + coord_sf(xlim = c(-6.5, 3.5), ylim = c(9, 15.5), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")    
#<b>Ruminant livestock feed availability</b>+ coord_sf(xlim = c(28.8, 48), ylim = c(-11.75, 14.85), expand = FALSE) + scale_fill_gradient(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), low = "#FFFFFF", high = "#F9A908", na.value = NA) 

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(lvstIntake_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("") + labs(fill = expression("Tj"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 0.6), breaks = c(0, 0.2, 0.4, 0.6), low = "#FFFFFF", high = "#F9A908", na.value = NA) + coord_sf(xlim = c(-6.5, 3.5), ylim = c(9, 15.5), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")    
#<b>Ruminant livestock feed requirements</b>+ coord_sf(xlim = c(-6, 3), ylim = c(9, 16), expand = FALSE) + scale_fill_gradient(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), low = "#FFFFFF", high = "#F9A908", na.value = NA) 

##Time series


####Sense checking plots
##Land area
dat <- read.csv('QC_checks/SI_cropping.csv')
dat <- transform(dat, country=factor(country,levels=c("Ethiopia", "Burkina Faso")))

ggplot(dat, aes(dataset, area_km2)) + geom_bar(stat = "identity") + facet_wrap(~ country) + ylab(expression("Area "~km^2)) + xlab("Dataset") + theme_classic() + theme(text=element_text(family="serif", size = 14), axis.text.x = element_text ( angle = 45, vjust = 1, hjust=1), strip.background = element_blank())

##Comparison with other adequacy estimates
aoi1 <- read.csv('Results/totals_compare_2019.csv', stringsAsFactors = F)

datILRI <- read.csv("AltAnalyses/ILRI estimates.csv")
aoi1 <- left_join(aoi1, datILRI)
aoi1$FeedAdeq_govt <- (aoi1$totalBiomass_Ministry*1000*8) / aoi1$lvst # (aoi1$Feed_TjIntake*1000000)

p0 <- ggplot(aoi1, aes(FeedAdeq_ME_common, reorder(NAME_1, FeedAdeq_ME_common))) + geom_point() + geom_errorbar(aes(xmax = FeedAdeq_ME_common_max, xmin = FeedAdeq_ME_common_min), width=0.1) + coord_cartesian(xlim = c(0, 6)) + scale_x_continuous(breaks = c(0,2,4,6,8)) + ylab("") + xlab("") + theme_classic() + theme(text=element_text(family="serif", size = 14)) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")
#p1 <- ggplot(aoi1long, aes(FeedAdeq_ME_common, value, colour = name)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3)) + ylab("Alternative estimates") + xlab("This study") + theme_classic() #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)
p1 <- ggplot(aoi1, aes(FeedAdeq_ME_common, FeedAdeq_ME_Rahimi)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3), xlim = c(0,6)) + ylab("Rahimi et al. estimate") + xlab("This study") + theme_classic() + theme(text=element_text(family="serif", size = 14)) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)
p2 <- ggplot(aoi1, aes(FeedAdeq_ME_common, FeedAdeq_govt)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3), xlim = c(0,6)) + ylab("Government estimate") + xlab("") + theme_classic() + theme(text=element_text(family="serif", size = 14)) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)

grid.arrange(p0, p1, p2, ncol = 3, top = "Feed adequacy (ME avaialbilty / ME requirement per year per admin region)")
#900x350
summary(lm(aoi1$FeedAdeq_ME_common ~ aoi1$FeedAdeq_ME_Rahimi))
summary(lm(aoi1$FeedAdeq_ME_common ~ aoi1$FeedAdeq_govt))
cor(aoi1$FeedAdeq_ME_common[!is.na(aoi1$FeedAdeq_govt)], aoi1$FeedAdeq_ME_Rahimi[!is.na(aoi1$FeedAdeq_govt)], method = "spearman")
cor(aoi1$FeedAdeq_ME_common[!is.na(aoi1$FeedAdeq_govt)], aoi1$FeedAdeq_govt[!is.na(aoi1$FeedAdeq_govt)], method = "spearman")

