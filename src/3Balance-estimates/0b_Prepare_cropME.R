library(raster)
library(stars)
library(sf)
library(dplyr)
library(exactextractr)

aoi3 <- st_read('SpatialData/inputs/aoi3_Ethiopia.gpkg')

cropHI <- read.csv('CropParams/crop_harvest index.csv', stringsAsFactors = F)
cropHI$utilMax <- ifelse(cropHI$codeSPAM %in% c("swpo", "grou"), 1, ifelse(cropHI$codeSPAM %in% c("bana", "sugc"), 0.3, 0.8))

feedQuality_SSA <- read.csv('CropParams/feedQuality_SSAdb.csv', stringsAsFactors = F)
#If ME is NA then estimate DE and ME from IVDMD & OM #NRC (2001) using IVDMD as a proxy for IVOMD
feedQuality_SSA <- feedQuality_SSA[!is.na(feedQuality_SSA$IVDMD),]
DMI <- 5 #DMI has minimal effect on ME_DE. Set at a reasonable value
OM <- 0.9
feedQuality_SSA$DE_ILRI <- feedQuality_SSA$ME * (1/0.82)
feedQuality_SSA$DE <- ifelse(is.na(feedQuality_SSA$OM), 0.01*OM*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3, 0.01*(feedQuality_SSA$OM/100)*(feedQuality_SSA$IVDMD + 12.9)*4.4 - 0.3)*4.1868 #NRC (2001) using IVDMD as a proxy for IVOMD
feedQuality_SSA$ME_DE <- 0.9410+0.0042*DMI-0.0017*feedQuality_SSA$NDF-0.0022*feedQuality_SSA$CP 
feedQuality_SSA$MEseo <- feedQuality_SSA$DE * feedQuality_SSA$ME_DE # Seo et al., 2021 https://doi.org/10.1093/jas/skab182 
feedQuality_item <- group_by(feedQuality_SSA, codeSPAM)
feedQuality_item <- summarise(feedQuality_item, ME_sd = sd(MEseo, na.rm = T), ME_min = min(MEseo, na.rm = T), ME_max = max(MEseo, na.rm = T), ME = mean(MEseo, na.rm = T), CP = mean(CP, na.rm = T), NDF = mean(NDF, na.rm =T), IVDMD = mean(IVDMD, na.rm = T), n = n())

feedQuality_item$ME[feedQuality_item$codeSPAM == "pmil"] <- feedQuality_item$ME[feedQuality_item$codeSPAM == "smil"]
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "pmil"] <- feedQuality_item$ME_min[feedQuality_item$codeSPAM == "smil"]
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "pmil"] <- feedQuality_item$ME_max[feedQuality_item$codeSPAM == "smil"]
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "rice"] <- 5.91 #From SSA feed DB - excluding outlier Gambia https://feedsdatabase.ilri.org/search/Rice%20straw?title=rice&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=All&combine=
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "rice"] <- 8.42 #From SSA feed DB - excluding outlier Gambia
feedQuality_item$ME[feedQuality_item$codeSPAM == "rice"] <- 6.76 #From SSA feed DB - excluding outlier Gambia
feedQuality_item <- rbind(feedQuality_item, c("cass", NA, 4.18, 4.18, 4.18, NA)) #Kiendrebeogo, et al 2019
feedQuality_item <- rbind(feedQuality_item, c("ocer", 0.79, 6.98-0.79, 6.98+0.79, 6.98, 2041)) #Teff SSA feed DB
feedQuality_item <- rbind(feedQuality_item, c("opul", sd(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "leg"], na.rm = T), min(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "leg"], na.rm = T), max(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "leg"], na.rm = T), mean(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "leg"], na.rm = T)))
feedQuality_item <- rbind(feedQuality_item, c("orts", sd(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), min(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), max(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), mean(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T)))
feedQuality_item <- rbind(feedQuality_item, c("sesa", 0.47, 7.66, 8.13, 8.05)) #EQUIP
feedQuality_item <- rbind(feedQuality_item, c("yams", sd(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), min(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), max(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T), mean(feedQuality_SSA$ME[feedQuality_SSA$codeBasket_grouped == "roots"], na.rm = T)))
feedQuality_item$codeSPAM[feedQuality_item$codeSPAM == "beetWhole"] <- "sugb"
feedQuality_item$ME[feedQuality_item$codeSPAM == "bana"] <- 7.1 #Feyesa et al., 2021
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "bana"] <- 5.9 #Feyesa et al., 2021
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "bana"] <- 8.2 #Feyesa et al., 2021
feedQuality_item$ME[feedQuality_item$codeSPAM == "plnt"] <- 8.5 #Feyesa et al., 2021 - Enset leaf
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "plnt"] <- 7.7 #Feyesa et al., 2021 - Enset leaf
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "plnt"] <- 9.1 #Feyesa et al., 2021 - Enset leaf
feedQuality_item$ME[feedQuality_item$codeSPAM == "natPast"] <- 7.9 #Feyesa et al., 2021
feedQuality_item$ME_min[feedQuality_item$codeSPAM == "natPast"] <- 6.3 #Feyesa et al., 2021
feedQuality_item$ME_max[feedQuality_item$codeSPAM == "natPast"] <- 9.7 #Feyesa et al., 2021 
feedQuality_item$ME <- as.numeric(feedQuality_item$ME)
feedQuality_item$ME_min <- as.numeric(feedQuality_item$ME_min)
feedQuality_item$ME_max <- as.numeric(feedQuality_item$ME_max)
#!'sunf' and other oil crops not included

write.csv(feedQuality_item, 'CropParams/feedQuality_item.csv')

stSPAM <- stack(list.files(path = 'SpatialData/inputs/SPAM2017/', pattern = "_A_clip.tif$", full.names = T) )
iSPAMcropArea <- sum(stSPAM, na.rm = T)

#Crop specific harvest index recipricol, utilisation and ME
crops <- toupper(c('bana','barl','bean','cass','chic','cowp','grou','lent','maiz','ocer','opul','orts','pmil','pige','plnt','pota','rape','rice','sesa','smil','sorg','soyb','sugb','sugc','sunf','swpo','temf','trof','vege','whea','yams'))
tmpCropIndex <- grep(pattern = paste(crops, collapse = "|"), names(stSPAM))
stCropMEmean <- stack()
stCropMEmin <- stack()
stCropMEmax <- stack()
stCropME_HI_utilmean <- stack()
stCropME_HI_utilmin <- stack()
stCropME_HI_utilmax <- stack()
stSPAMcropProp <- stack()
for(i in 1: length(crops)){
  tmpCropIndex <- grep(pattern = paste(crops[i], collapse = "|"), names(stSPAM))
  iSPAMtmpArea <- overlay(stSPAM[[tmpCropIndex]], fun = sum)
  #Create raster of ME values weighted by harvest index and utilisation for each cell where the crop is grown
  icrop <- stSPAM[[tmpCropIndex]]
  icrop[icrop >0] <- 1
  
  icropMean <- icrop * max(0,feedQuality_item$ME[feedQuality_item$codeSPAM == tolower(crops[i])])
  stCropMEmean <- stack(stCropMEmean, icropMean)
  
  icropMin <- icrop * max(0,feedQuality_item$ME_min[feedQuality_item$codeSPAM == tolower(crops[i])]) 
  stCropMEmin <- stack(stCropMEmin, icropMin)
  
  icropMax <- icrop * max(0,feedQuality_item$ME_max[feedQuality_item$codeSPAM == tolower(crops[i])])
  stCropMEmax <- stack(stCropMEmax, icropMax)
  
  icropMean <- icrop * max(0,feedQuality_item$ME[feedQuality_item$codeSPAM == tolower(crops[i])]) *(1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmean <- stack(stCropME_HI_utilmean, icropMean)
  
  icropMin <- icrop * max(0,feedQuality_item$ME_min[feedQuality_item$codeSPAM == tolower(crops[i])]) * (1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmin <- stack(stCropME_HI_utilmin, icropMin)
  
  icropMax <- icrop * max(0,feedQuality_item$ME_max[feedQuality_item$codeSPAM == tolower(crops[i])]) * (1 - cropHI$harvest_index[cropHI$codeSPAM == tolower(crops[i])]) * cropHI$utilMax[cropHI$codeSPAM == tolower(crops[i])]
  stCropME_HI_utilmax <- stack(stCropME_HI_utilmax, icropMax)
  
  stSPAMcropProp <- stack(stSPAMcropProp, iSPAMtmpArea/iSPAMcropArea)
  
  print(paste("Crop species", i))
}
stCropMEmean <- reclassify(stCropMEmean, c(-Inf, 0, NA))
stCropMEmin <- reclassify(stCropMEmin, c(-Inf, 0, NA))
stCropMEmax <- reclassify(stCropMEmax, c(-Inf, 0, NA))
stSPAMcropProp <- reclassify(stSPAMcropProp, c(-Inf, 0, NA))
cropMEmean <- weighted.mean(stCropMEmean, stSPAMcropProp, na.rm = T)
cropMEsd <- weighted.mean( (stCropMEmean-cropMEmean)^2, stSPAMcropProp, na.rm = T)
cropMEmin <- weighted.mean(stCropMEmin, stSPAMcropProp, na.rm = T)
cropMEmax <- weighted.mean(stCropMEmax, stSPAMcropProp, na.rm = T)

#Aggregate at the 3rd admin level and then export raster
aoi3$cropMEmean <- exact_extract(cropMEmean, aoi3, fun = "mean")
aoi3$cropMEmin <- exact_extract(cropMEmin, aoi3, fun = "mean")
aoi3$cropMEmax <- exact_extract(cropMEmax, aoi3, fun = "mean")

feedCropBurn <- stars::read_stars('SpatialData/inputs/Burned/burnCropsDekads.tif')
st_rasterize(sf = aoi3[, "cropMEmean"], template = feedCropBurn, file = "SpatialData/intermediate/cropMEmean.tif")
st_rasterize(sf = aoi3[, "cropMEmin"], template = feedCropBurn, file = "SpatialData/intermediate/cropMEmin.tif")
st_rasterize(sf = aoi3[, "cropMEmax"], template = feedCropBurn, file = "SpatialData/intermediate/cropMEmax.tif")


stCropME_HI_utilmean <- reclassify(stCropME_HI_utilmean, c(-Inf, 0, NA))
stCropME_HI_utilmin <- reclassify(stCropME_HI_utilmin, c(-Inf, 0, NA))
stCropME_HI_utilmax <- reclassify(stCropME_HI_utilmax, c(-Inf, 0, NA))
cropME_HI_utilmean <- weighted.mean(stCropME_HI_utilmean, stSPAMcropProp, na.rm = T)
cropME_HI_utilsd <- weighted.mean( (stCropME_HI_utilmean-cropME_HI_utilmean)^2, stSPAMcropProp, na.rm = T)
cropME_HI_utilmin <- weighted.mean(stCropME_HI_utilmin, stSPAMcropProp, na.rm = T)
cropME_HI_utilmax <- weighted.mean(stCropME_HI_utilmax, stSPAMcropProp, na.rm = T)

#Aggregate at the 3rd admin level and then export raster
aoi3$cropME_HI_utilmean <- exact_extract(cropME_HI_utilmean, aoi3, fun = "mean")
aoi3$cropME_HI_utilmin <- exact_extract(cropME_HI_utilmin, aoi3, fun = "mean")
aoi3$cropME_HI_utilmax <- exact_extract(cropME_HI_utilmax, aoi3, fun = "mean")

st_rasterize(sf = aoi3[, "cropME_HI_utilmean"], template = feedCropBurn, file = "SpatialData/intermediate/cropME_HI_utilmean.tif")
st_rasterize(sf = aoi3[, "cropME_HI_utilmin"], template = feedCropBurn, file = "SpatialData/intermediate/cropME_HI_utilmin.tif")
st_rasterize(sf = aoi3[, "cropME_HI_utilmax"], template = feedCropBurn, file = "SpatialData/intermediate/cropME_HI_utilmax.tif")

