library(raster)
library(stars)
library(sf)
library(dplyr)

feedQuality_SSA <- read.csv('CropParams/feedQuality_SSAdb.csv', stringsAsFactors = F)
feedQuality_SSA <- feedQuality_SSA[!is.na(feedQuality_SSA$IVDMD),]
DMI <- 5 #DMI has minimal effect on ME_DE. Set at a reasonable value
OM <- 0.9
feedQuality_SSA$DE_ILRI <- feedQuality_SSA$ME * (1/0.82)
feedQuality_SSA$DE <- ifelse(is.na(feedQuality_SSA$OM), 0.01*OM*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3, 0.01*(feedQuality_SSA$OM/100)*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD
ggplot(feedQuality_SSA, aes(DE*0.82, ME, colour = OM)) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + coord_cartesian(ylim = c(0, 16), xlim = c(0,16)) + xlab("ME - recreated using NRC, 2001 * 0.82") + ylab("ME - extractred from SSA Feed DB")
feedQuality_SSA$MEdiff <- (feedQuality_SSA$DE*0.82) - feedQuality_SSA$ME
feedQuality_SSA$OM[is.na(feedQuality_SSA$OM)] <- 0
#feedQuality_SSA$Reference <- scale(feedQuality_SSA$Reference)
subfeedQuality_SSA <- feedQuality_SSA[!is.na(feedQuality_SSA$MEdiff) & abs(feedQuality_SSA$MEdiff) >0.25,] #> 0.5
#subfeedQuality_SSA$ID <- scale(subfeedQuality_SSA$Reference)
subfeedQuality_SSA$clust <- cut(subfeedQuality_SSA$Reference, 15, labels = F)
feedQuality_SSA <- left_join(feedQuality_SSA, select(subfeedQuality_SSA, Reference, clust))
feedQuality_SSA$clust <- ifelse(is.na(feedQuality_SSA$clust), 0, feedQuality_SSA$clust)
ggplot(feedQuality_SSA[feedQuality_SSA$Feed_item != "fodder beet" & feedQuality_SSA$clust != 0,], aes(DE*0.82, ME, colour = as.factor(clust))) + geom_point() + geom_abline(intercept = 0, slope = 1, color="grey", linetype="dashed", size=0.5) + scale_color_discrete (name = "ID grouping") + coord_cartesian(ylim = c(5, 10), xlim = c(5,10)) + xlab("ME - recreated using NRC, 2001 * 0.82") + ylab("ME - extractred from SSA Feed DB") +facet_wrap(~Feed_item)
summary(lm(subfeedQuality_SSA$ME ~ subfeedQuality_SSA$NDF + subfeedQuality_SSA$IVDMD + subfeedQuality_SSA$CP + subfeedQuality_SSA$OM))
summary(lm(ME ~ NDF + IVDMD + CP + OM, data = feedQuality_SSA[feedQuality_SSA$clust == 0,]))
feedQuality_SSA$predIVDMD <- ifelse(feedQuality_SSA$clust %in% c(4,6,13), 1, 0)
plot(feedQuality_SSA$MEdiff ~ feedQuality_SSA$predIVDMD)
#subfeedQuality_SSA$clust <- kmeans(cbind(abs(subfeedQuality_SSA$MEdiff), subfeedQuality_SSA$ID), 10)$cluster
summary(lm(feedQuality_SSA$MEdiff ~ feedQuality_SSA$Reference + feedQuality_SSA$OM))
ggplot(feedQuality_SSA, aes(MEdiff, Reference)) + geom_point()
ggplot(feedQuality_SSA, aes(MEdiff, OM)) + geom_point()

feedQuality_SSA$ME_DE <- 0.9410+0.0042*DMI-0.0017*feedQuality_SSA$NDF-0.0022*feedQuality_SSA$CP 
feedQuality_SSA$MEseo <- feedQuality_SSA$DE * feedQuality_SSA$ME_DE
feedQuality_item <- group_by(feedQuality_SSA, codeSPAM)
feedQuality_item <- summarise(feedQuality_item, ME_sd = sd(MEseo, na.rm = T), ME_min = min(MEseo, na.rm = T), ME_max = max(MEseo, na.rm = T), ME = mean(MEseo, na.rm = T), n = n())