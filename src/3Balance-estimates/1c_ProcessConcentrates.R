library(terra)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

##Calculate total ME for concentrates
#Summarise ME concentration measurements
dat <- read.csv('CropParams/feedQualityConcentrates_SSAdb.csv')
dat <- dat[!is.na(dat$ME),]
dat <- group_by(dat, Feed_item)
concMean <- summarise(dat, MEmean = mean(ME, na.rm = T), CP = mean(CP, na.rm = T), NDF = mean(NDF, na.rm = T), count = n())

concentrateLookup <- data.frame(Feed_item = c("Dairy concentrate", "Dairy meal", "Maize bran", "Maize grain", "Wheat bran", "Wheat germ", "Wheat short", "Rice bran", "Cottonseed cake", "Sunflower cake", "Noug cake", "Linseed cake", "Groundnut cake", "Soya Bean Meal", "Sesame cake", "Brewery by-product (maize)", "Bean hull (unroasted)", "Field pea hull", "Lentil hull", "Barley hull (unroasted)"),
                            name = c("Dairy concentrate", "Dairy meal", "Maize bran", "Maize grain screening", "Wheat bran", "Wheat germ", "Wheat short", "Rice bran", "Cotton seed cake", "Sunflower seed cake", "Noug seed cake", "Linseed cake", "Groundnut cake", "Soybean meal", "Sesame seed cake", "Brewery by-product (maize)", "Bean hull", "Pea hull", "Lentil hull", "Barley hull"))

concMean <- left_join(concMean, concentrateLookup)


#Prepare mass by region
mass <- read.csv('CropParams/Ethiopia_concentrate_masses.csv')#, locale = readr::locale(encoding = 'WINDOWS-1252'))
massLong <- pivot_longer(mass, cols = -Region, values_to = "value")
massLong$name <- gsub("\\.", " ", massLong$name)
massLong$value <- massLong$value * 1000 #tons to kg

massLong <- left_join(massLong, select(concMean, -Feed_item, -count))

#Exclude: "Meat and bone meal" 
massLong <- massLong[!(massLong$name %in% c("Meat and bone meal", "Molasses")),]
#Approximate close matches
massLong$MEmean[massLong$name == "Mixed cake"] <- concMean$MEmean[concMean$Feed_item == "Sunflower cake"]
massLong$MEmean[massLong$name == "Brewery spent grains"] <- concMean$MEmean[concMean$Feed_item == "Brewery by-product (maize)"]
massLong$MEmean[massLong$name == "Bean hull with fine grain_Duka"] <- concMean$MEmean[concMean$name == "Bean hull"]
massLong$MEmean[massLong$name == "Pea hull with fine grain_Duka"] <- concMean$MEmean[concMean$name == "Pea hull"]
massLong$MEmean[massLong$name == "Lentil hull with fine grain_Shana"] <- concMean$MEmean[concMean$name == "Lentil hull"]

massLong$MEmean[massLong$name == "Wheat grain screening"] <- concMean$MEmean[concMean$Feed_item == "Wheat bran"]
massLong$MEmean[massLong$name == "Oat bran"] <- concMean$MEmean[concMean$Feed_item == "Wheat bran"]
massLong$MEmean[massLong$name == "Maize short germ"] <- 9.59 #https://www.ethiofeedsdatabase.org.et/dataset?feed_id=154#details
massLong$MEmean[massLong$name == "Peanut hull"] <- concMean$MEmean[concMean$name == "Bean hull"]
massLong$MEmean[massLong$name == "Bagasse"] <- 6.5 #https://feedsdatabase.ilri.org/search/Sugar%20cane%20waste?title=sugar%20cane&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=All&combine=
massLong$MEmean[massLong$name == "Molasses"] <- 14.5 #Assefa et al.2013 https://d1wqtxts1xzle7.cloudfront.net/48589671/Assefa_et_al__0.5-libre.pdf?1473089637=&response-content-disposition=inline%3B+filename%3DEffects_of_molasses_level_in_a_concentra.pdf&Expires=1709828249&Signature=Hqw7tZ5KdiXB1FcCbREF8iaQmIUIAtsp4DaSaYFcJTcRmgRa~rrTo9P7XzmeKfKMaywapOnjFHlfXZu52jsmZWh5eFA0nSFKcIwvvi-c~ZguV7kehGUYvoARhk-FFQcOvO~SLSq7f~Oiohqt62-Ik4iBT1W8tO5zOCqxpExPUVqNY~9i-tg5CLO2xU33nZ2iOG13NjOdgS0Y5WyElUyfNet1HcQxtM9mrBAqwJ2yD826Spf9vbzyO-TLoN4w-CSChg2CSz8lFewV~Z7CGCuobNFJKfVF3NeW2eGmpgB4BPgxYyG7qoWo6PUeH5UblOU6C5M9ZaYD46h3eH9uZf-9tw__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA

massLong$MEmean[is.na(massLong$MEmean)] <- 10
#Source estimates in order of priority
#"Bagasse"
#"Wheat grain screening"
#"Oat bran"
#"Maize short germ"
#"Malt Germ_Rootlet"
#"Malt Broken_third _thin barley or malt dust"
#"Malt Dust"
#"Malt Floater"
#"Peanut hull"

massLong$MEtotal <- massLong$value * massLong$MEmean

massLong <- group_by(massLong, NAME_1 = Region)
MEregion <- summarise(massLong, ME = sum(MEtotal, na.rm = T))

MEregion$NAME_1[MEregion$NAME_1 == "Addis Ababa"] <- "Addis Abeba"
MEregion$NAME_1[MEregion$NAME_1 == "Benishangul Gumuz"] <- "Benshangul-Gumaz"
MEregion$NAME_1[MEregion$NAME_1 == "Gambella"] <- "Gambela Peoples"
MEregion$NAME_1[MEregion$NAME_1 == "Harari"] <- "Harari People"
MEregion$NAME_1[MEregion$NAME_1 == "Benishangul Gumuz"] <- "Somali"
MEregion$NAME_1[MEregion$NAME_1 == "SNNPR"] <- "Southern Nations, Nationalities and Peoples"

MEregion <- rbind(MEregion, c("Somali", NA))

##Rasterise total ME, area weighted
lvCattle <- rast('SpatialData/inputs/GLW4/Ct_2015_10k.tif')

r <- init(lvCattle, NA)
nc <- vect('SpatialData/inputs/aoi1.gpkg')
nc <- merge(nc, MEregion)
nc$ME[is.na(nc$ME)] <- 0
nc$ME <- as.numeric(nc$ME)
nc$ME[nc$NAME_1 == "Afar"] <- 0 

#Exclude pastoral regions from concentrate ME estimates
livelihood <- vect('SpatialData/inputs/ET_LHZ_2018.shp')
livelihood <- livelihood[livelihood$LZTYPE == "Pastoral",]

nc <- nc-livelihood

#Calculate average ME per cell
r.ext <- terra::extract(r, nc)
r.ext <- group_by(r.ext, ID)
nc$cells <- summarise(r.ext, fun = n())$fun
nc$ME_cell <- nc$ME / nc$cells

#Rasterise ME estimate
concentrateME <- rasterize(nc, r, field = "ME_cell")

##Extract concentrate ME by zone
zones <- st_read('SpatialData/intermediate/zones.gpkg')

concentrateME_zone <- data.frame(region = c(rep("(agro)pastoral", 6), rep("Highland mixed", 6), rep("Lowland mixed", 6), rep("Midland mixed", 6)), year = c(2014:2019, 2014:2019, 2014:2019, 2014:2019), concentrateME = NA) 

MEtmp <- exact_extract(concentrateME, zones, "sum") #!161808672 ME lost in extraction. Check polygon extent
concentrateME_zone$concentrateME <- as.numeric(c(rep(MEtmp[1], 6), rep(MEtmp[2], 6), rep(MEtmp[3], 6), rep(MEtmp[4], 6)))

dissagregatedTimeseries <- read.csv("Results/disaggregated_timeseries.csv")
dissagregatedTimeseries$concentrateME_mean <- concentrateME_zone$concentrateME
dissagregatedTimeseries$concentrateME_min <- concentrateME_zone$concentrateME
dissagregatedTimeseries$concentrateME_max <- concentrateME_zone$concentrateME

write.csv(dissagregatedTimeseries, "Results/disaggregated_timeseries_concentrates.csv")
