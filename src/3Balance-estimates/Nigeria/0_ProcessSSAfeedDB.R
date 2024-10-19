li
brary(readr)
library(dplyr)

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

# paths
feedParOut <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams"); dir.create(feedPar_dest, F, T)

feedQuality <- read_csv(paste0(feedParOut, "/feedQuality_SSAdb.csv"))
crop_classification <- read_csv(paste0(feedParOut, "/Crop classification_feed basket.csv"))

feedQuality <- left_join(feedQuality, dplyr::select(crop_classification, Feed_item = SSAfeedNigeria, codeSPAM, codeBasket_grouped_NGA)) %>% 
  dplyr::select(-1, -2)

feedQuality$codeSPAM[feedQuality$Feed_item == "fodder beet"] <- "beetWhole"
feedQuality$codeSPAM[feedQuality$Feed_item == "Groundnut"] <- "grou"
  
# Write outputs
write.csv(feedQuality, paste0(feedParOut, "/feedQuality_NGA_SSAdb.csv"))

plot(feedQuality$ME ~ feedQuality$IVDMD)
summary(lm(feedQuality$ME ~ feedQuality$IVDMD))

feedQuality <- group_by(feedQuality, Feed_item, codeSPAM)
feedQuality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())

feedQuality <- group_by(feedQuality, codeBasket_grouped_NGA)
ssa_cat_quality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())
