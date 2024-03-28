#.libPaths(c(.libPaths()[2], .libPaths()[3]))
library(raster)
library(stars)
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
  panel.grid.major = element_line(colour = "transparent") #remove map grid lines#, 

)
options(scipen = 999)


##Feed timeseries breakdown 
#tsSum <- read.csv('LivestockParams/totals_timeseries.csv')
tsSum <- read.csv('Results/disaggregated_timeseries_concentrates.csv')
tsSum$region <- gsub("agro", "Agro", tsSum$region)

datGovt <- read.csv("AltAnalyses/Ethiopia_govt_estimates2.csv", stringsAsFactors = F)
datGovt <- group_by(datGovt, region)
datGovtCF <- summarise(datGovt, cultivatedME_mean = sum(Cultivated_fodder, na.rm = T))
tsSum <- left_join(tsSum, datGovtCF)
tsSum$cultivatedME_mean <- (tsSum$cultivatedME_mean*1000*8.35)
tsSum$cultivatedME_mean <- ifelse(is.na(tsSum$cultivatedME_mean), 0, tsSum$cultivatedME_mean)
#tsSum$cultivatedME_mean[is.na(tsSum$CultivatedME_mean)] <- 0
tsSum$cropME_mean <- tsSum$cropME_mean - (tsSum$cultivatedME_mean*0.7) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2
tsSum$afterME_mean <- tsSum$afterME_mean - (tsSum$cultivatedME_mean*0.3) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2
tsSum$cropME_min <- tsSum$cropME_min - (tsSum$cultivatedME_mean*0.7) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2
tsSum$afterME_min <- tsSum$afterME_min - (tsSum$cultivatedME_mean*0.3) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2
tsSum$cropME_max <- tsSum$cropME_max - (tsSum$cultivatedME_mean*0.7) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2
tsSum$afterME_max <- tsSum$afterME_max - (tsSum$cultivatedME_mean*0.3) #Grass is only max 55% utilised, cultivated fodder is 100% utilised *2

tsSum_plot <- pivot_longer(dplyr::select(tsSum, region, year, cropME_mean, grassME_mean, browseME_mean, afterME_mean, cultivatedME_mean, concentrateME_mean), cols = -c(region, year))
lower <- pivot_longer(dplyr::select(tsSum, region, year, cropME_min, grassME_min, browseME_min, afterME_min, cultivatedME_mean, concentrateME_min), cols = -c(region, year), values_to = "lower")
upper <- pivot_longer(dplyr::select(tsSum, region, year, cropME_max, grassME_max, browseME_max, afterME_max, cultivatedME_mean, concentrateME_max), cols = -c(region, year), values_to = "upper")
tsSum_plot <- cbind(tsSum_plot, select(lower, lower))
tsSum_plot <- cbind(tsSum_plot, select(upper, upper))

#tsSum <- pivot_longer(dplyr::select(tsSum, region, year, lvstReq, cropME_mean, grassME_mean, browseME_mean, afterME_mean, cropME_min, grassME_min, browseME_min, afterME_min), c(lvstReq, cropME_mean, grassME_mean, browseME_mean, afterME_mean))

tsSum_plot$value <- tsSum_plot$value/1000000 #MJ to TJ
tsSum_plot$lower <- tsSum_plot$lower/1000000
tsSum_plot$upper <- tsSum_plot$upper/1000000

#ggplot(tsSum_plot, aes(year, value, colour = name)) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + geom_line() + ylab("Energy available (Tj ME)") + xlab("Year") +  scale_colour_manual(name = "", labels = c("Crop residue", "Grass", "Other biomass", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif")) + facet_wrap(~region) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +

#ggplot(tsSum_plot[tsSum_plot$year == 2019,], aes(name, value, fill = name)) + geom_col(position = "identity") + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") +  scale_fill_manual(name = "", labels = c("Crop residue", "Grass", "Other biomass", "Browse", "Cultivated grass"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000", "cultivatedME_mean" = "#09FF00")) + theme_classic() + theme(text=element_text(family="serif"), axis.text.x=element_blank ()) + facet_wrap(~region) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
tsSum_plot <- transform(tsSum_plot, region=factor(region,levels=c("(Agro)pastoral", "Lowland mixed", "Midland mixed", "Highland mixed", "Pastoral")))
tsSum_plot <- transform(tsSum_plot, name=factor(name,levels=c("grassME_mean", "cropME_mean", "afterME_mean", "cultivatedME_mean", "concentrateME_mean", "browseME_mean")))
Fig1 <- ggplot(tsSum_plot[tsSum_plot$year == 2019,], aes(name, value, fill = name)) + geom_col(position = "identity") + geom_errorbar(aes(ymin = lower, ymax=upper)) + ylab("Energy available (TJ ME)") + xlab("Feed category") + scale_y_continuous(breaks = c(0,100000,250000,400000,550000)) + scale_fill_manual(name = "", labels = c("Grass", "Crop residue", "Post-harvest growth", "Cultivated grass", "Concentrates", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000", "cultivatedME_mean" = "#09FF00", "concentrateME_mean" = "#4996fc")) + theme_classic() + theme(text=element_text(family="serif", size = 14), axis.title.x=element_blank(), axis.text.x=element_blank (), strip.background = element_blank()) + facet_wrap(~region, ncol = 4) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave("EthFig1_1000con.tiff", Fig1, device = "tiff", dpi = 1000, width=85 * (14/5), height=20 * (14/5), units = "mm")


tsSum <- tsSum %>% rowwise() %>% mutate(totalME_mean = sum(cropME_mean, grassME_mean, browseME_mean, afterME_mean, cultivatedME_mean, concentrateME_mean))
tsSum <- tsSum %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean, cultivatedMEprop_mean = cultivatedME_mean /totalME_mean, concentrateMEprop_mean = concentrateME_mean /totalME_mean)

tsSum_plot2 <- pivot_longer(dplyr::select(tsSum, region, year, cropMEprop_mean, grassMEprop_mean, browseMEprop_mean, afterMEprop_mean, cultivatedMEprop_mean, concentrateMEprop_mean), cols = -c(region, year))
tsSum_plot2 <- transform(tsSum_plot2, name=factor(name,levels=c("grassMEprop_mean", "cropMEprop_mean", "afterMEprop_mean", "cultivatedMEprop_mean", "concentrateMEprop_mean", "browseMEprop_mean")))

SI1 <- ggplot(tsSum_plot2, aes(year, value, colour = name, fill = name)) + geom_line() + ylab("Proportion of energy") + xlab("Year") + scale_x_continuous(breaks=c(2015, 2017, 2019)) +  scale_colour_manual(name = "", labels = c("Grass", "Crop residue", "Post-harvest growth", "Cultivated grass", "Concentrates", "Browse"), values = c("cropMEprop_mean" = "#F8CA02", "grassMEprop_mean" = "#008D1F", "afterMEprop_mean" = "#000000", "browseMEprop_mean" = "#FF0000", "cultivatedMEprop_mean" = "#09FF00", "concentrateMEprop_mean" = "#4996fc")) + theme_classic() + theme(text=element_text(family="serif", size = 14), axis.title.x = element_blank(), strip.background = element_blank()) + facet_wrap(~region, nrow = 1) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +
ggsave("EthSI1_1000.tiff", SI1, device = "tiff", dpi = 300, width=85 * (14/5), height=20 * (14/5), units = "mm")

#Table 2
x <- select(tsSum[tsSum$year == 2019,], c(region, totalME_mean, cropME_mean, grassME_mean, browseME_mean, afterME_mean, cultivatedME_mean, concentrateME_mean))
x$elev <- c("low", "high", "low", "high")
x <- group_by(x, elev)
x <- summarise_all(select(x, -region), sum)
x <- x %>% rowwise() %>% mutate(cropMEprop_mean = cropME_mean/ totalME_mean, grassMEprop_mean = grassME_mean / totalME_mean, browseMEprop_mean = browseME_mean / totalME_mean, afterMEprop_mean = afterME_mean / totalME_mean, cultivatedMEprop_mean = cultivatedME_mean /totalME_mean, concentrateMEprop_mean = concentrateME_mean /totalME_mean)
x <- select(x, c(elev, totalME_mean, cropMEprop_mean, grassMEprop_mean, browseMEprop_mean, afterMEprop_mean, cultivatedMEprop_mean, concentrateMEprop_mean))


#tsSum <- tsSum %>% rowwise() %>% mutate(totalME_min = sum(cropME_min, grassME_min, browseME_min, afterME_min))
#tsSum <- tsSum %>% rowwise() %>% mutate(cropMEprop_min = cropME_min/ totalME_min, grassMEprop_min = grassME_min / totalME_min, browseMEprop_min = browseME_min / totalME_min, afterMEprop_min = afterME_min / totalME_min)
#tsSum <- tsSum %>% rowwise() %>% mutate(totalME_max = sum(cropME_max, grassME_max, browseME_max, afterME_max))
#tsSum <- tsSum %>% rowwise() %>% mutate(cropMEprop_max = cropME_max/ totalME_max, grassMEprop_max = grassME_max / totalME_max, browseMEprop_max = browseME_max / totalME_max, afterMEprop_max = afterME_max / totalME_max)
#lower <- pivot_longer(dplyr::select(tsSum, region, year, cropMEprop_min, grassMEprop_min, browseMEprop_min, afterMEprop_min), cols = -c(region, year), values_to = "lower")
#upper <- pivot_longer(dplyr::select(tsSum, region, year, cropMEprop_max, grassMEprop_max, browseMEprop_max, afterMEprop_max), cols = -c(region, year), values_to = "upper")
#tsSum_plot2 <- cbind(tsSum_plot, select(lower, lower))
#tsSum_plot2 <- cbind(tsSum_plot, select(upper, upper))
#ggplot(tsSum_plot[tsSum_plot2$year == 2019], aes(year, value, colour = name, fill = name)) + geom_boxplot(position = 'fill') + ylab("Energy available (Tj ME)") + xlab("Year") +  scale_colour_manual(name = "", labels = c("Crop", "Grass", "Weeds", "Browse"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "afterME_mean" = "#000000", "browseME_mean" = "#FF0000")) + theme_classic() + theme(text=element_text(family="serif")) + facet_wrap(~region) #scale_colour_manual(name = "", labels = c("Crop", "Grass", "Livestock"), values = c("cropME_mean" = "#F8CA02", "grassME_mean" = "#008D1F", "lvstReq" = "#FF0101")) +


##Livestock feed adequacy timeseries breakdown
tsSumReg <- read.csv('Results/totals_timeseries_region.csv')

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
tsSumReg_plot <- transform(tsSumReg_plot, region=factor(NAME_1,levels=c("(Agro)pastoral", "Lowland mixed", "Midland mixed", "Highland mixed")))
Fig2 <- ggplot(tsSumReg_plot, aes(year, value, group = region)) + geom_hline(yintercept = 1, linetype = 2, colour = "grey") + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", linetype = 0, alpha = 0.3) + ylab("Energy available / required") + xlab("Year") + labs(colour = "") + scale_x_discrete(breaks=c("2015", "2017", "2019")) + scale_y_continuous(limits = c(0,11), breaks = c(0,1,3,5,7,9)) + scale_colour_lancet() + theme_classic() + facet_wrap(~region, ncol = 4)+  theme(text=element_text(family="serif", size = 12), strip.background = element_blank()) # 
ggsave("EthFig2_1000_horses.tiff", Fig2, device = "tiff", dpi = 1000, width=85 * (14/5), height=20 * (14/5), units = "mm")

###
##Mapping#
sfRegion <- st_read('SpatialData/intermediate/zones.gpkg')

feed_grass_MJ <- raster('SpatialData/outputs/Feed_grass_MJ2019.tif')
feed_crop_MJ <- raster('SpatialData/outputs/Feed_crop_MJ2019.tif')
feed_browse_MJ <- raster('SpatialData/outputs/Feed_browse_MJ2019.tif')
feed_after_MJ <- raster('SpatialData/outputs/Feed_after_MJ2019.tif')
feed_total_MJ <- sum(feed_grass_MJ, feed_crop_MJ, feed_browse_MJ, feed_after_MJ, na.rm =T)
cattleIntake_model_MJ_2015 <- raster('SpatialData/outputs/cattleMER_MJ_2015.tif')
shoatsIntake_model_MJ_2015 <- raster('SpatialData/outputs/shoatsMER_MJ_2015.tif')
lvstIntake_MJ <- sum(cattleIntake_model_MJ_2015, shoatsIntake_model_MJ_2015, na.rm = T)

aoi1 <- st_read('SpatialData/inputs/aoi1.gpkg')
feed_total_MJ <- mask(feed_total_MJ, aoi1)

lvstIntake_MJ <- mask(lvstIntake_MJ, aoi1)
resLvst <- res(lvstIntake_MJ)[1]
lvstIntake_MJ <- resample(lvstIntake_MJ, feed_total_MJ)/(resLvst/res(feed_total_MJ)[1])^2
#lvstIntake_MJ <- disaggregate(lvstIntake_MJ, 28)

countries <- ne_countries(scale = 50, type = "countries", returnclass = "sf")

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_sf(data = sfRegion, aes(fill = grouping)) + coord_sf(xlim = c(33, 48), ylim = c(3, 15.5), expand = FALSE) + labs(fill = "Region")  + theme(rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")   

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(feed_total_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("Ruminant livestock feed availability") + labs(fill = expression("TJ"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2), low = "#CDDF4A", high = "#0BAE1C", na.value = NA) + coord_sf(xlim = c(33, 48), ylim = c(3, 15.5), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs + theme(legend.position="bottom")   
#+ coord_sf(xlim = c(28.8, 48), ylim = c(-11.75, 14.85), expand = FALSE) + scale_fill_gradient(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), low = "#FFFFFF", high = "#F9A908", na.value = NA) 

ggplot() + geom_sf(data = countries, colour = "black", show.legend = F) + 
  geom_stars(data = st_as_stars(lvstIntake_MJ)/1000000) + geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + ggtitle("") + labs(fill = expression("TJ"~year^-1)) + xlab("") + ylab("") + scale_fill_gradient(limits = c(0, 0.6), breaks = c(0, 0.2, 0.4, 0.6), low = "#FFFFFF", high = "#F9A908", na.value = NA) + coord_sf(xlim = c(33, 48), ylim = c(3, 15.5), expand = FALSE) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), rect = element_blank(), panel.background = element_rect(fill = "blue3"), panel.grid.major = element_line(color = "blue3")) #+ themeLabs #+ theme(legend.position="bottom") #+ ggtitle("<b>Ruminant livestock feed requirements</b>")
#+ coord_sf(xlim = c(-6, 3), ylim = c(9, 16), expand = FALSE) + scale_fill_gradient(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), low = "#FFFFFF", high = "#F9A908", na.value = NA) 



##Sense checking plots
aoi1 <- read.csv('Results/totals_compare_2019.csv', stringsAsFactors = F)
aoi1$NAME_1[aoi1$NAME_1 == "Southern Nations, Nationalities and Peoples"] <- "SNNPR"

datGovt <- read.csv("AltAnalyses/Ethiopia_govt_estimates2.csv")
aoi1 <- left_join(dplyr::select(aoi1, -X), datGovt)
aoi1$FeedAdeq_govt <- (aoi1$Total*1000*8) / aoi1$lvst # (aoi1$Feed_TjIntake*1000000)

p0 <- ggplot(aoi1, aes(FeedAdeq_ME_common, reorder(NAME_1, FeedAdeq_ME_common))) + geom_point() + geom_errorbar(aes(xmax = FeedAdeq_ME_common_max, xmin = FeedAdeq_ME_common_min), width=0.1) + coord_cartesian(xlim = c(0, 32)) + scale_x_continuous(breaks = c(0,4,8,12,16,20,24,28,32)) + ylab("") + xlab("") + theme_classic() + theme(text=element_text(family="serif", size = 14)) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")
#p1 <- ggplot(aoi1long, aes(FeedAdeq_ME_common, value, colour = name)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3)) + ylab("Alternative estimates") + xlab("This study") + theme_classic() #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)
#p1 <- ggplot(aoi1, aes(FeedAdeq_ME_common, FeedAdeq_ME_Rahimi)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3), xlim = c(0,6)) + ylab("Rahimi et al. estimate") + xlab("This study") + theme_classic() + theme(text=element_text(family="serif")) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)
p2 <- ggplot(aoi1, aes(FeedAdeq_ME_common, FeedAdeq_govt)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 3), xlim = c(0,17)) + scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16)) + ylab("Feyisa et al. estimate") + xlab("") + theme_classic() + theme(text=element_text(family="serif", size =14)) #+ ggtitle("Feed availability (tonnes DM per year per admin region)")  #+ geom_errorbar(aes(ymax = FeedAdeq_ME_common_max, ymin = FeedAdeq_ME_common_min), width=0.1)

grid.arrange(p0, p2, ncol = 2, top = "Feed adequacy (ME avaialbilty / ME requirement per year per admin region)")

summary(lm(aoi1$FeedAdeq_ME_common[aoi1$FeedAdeq_ME_common < 5] ~ aoi1$FeedAdeq_govt[aoi1$FeedAdeq_ME_common < 5]))
