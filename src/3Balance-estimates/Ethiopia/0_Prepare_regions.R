library(raster)
library(stars)
library(sf)
library(dplyr)
library(exactextractr)

memory.limit(size = 16000)

aoi3 <- st_read('SpatialData/inputs/aoi3_Ethiopia.gpkg')

elevation1500 <- raster('SpatialData/inputs/Elevation/elev1500.tif')
elevation2300 <- raster('SpatialData/inputs/Elevation/elev2300.tif')
feedCropBurn <- raster('SpatialData/inputs/Burned/burnCropsDekads.tif')

elevation2300 <- resample(elevation2300, elevation1500)
elevation2300 <- crop(elevation2300, extent(elevation1500))

##Raster output
region <- overlay(elevation1500, elevation2300, fun = function(e1500, e2300){ifelse(e2300 == 1, 3, ifelse(e1500 ==0, 1, 2))})
region <- crop(region, extent(feedCropBurn))
region <- raster::resample(region, feedCropBurn, method = "ngb")
writeRaster(region, 'SpatialData/intermediate/regions.tif')

##Elevation region
aoi3$elev1500 <- exact_extract(elevation1500, aoi3, "mean")
aoi3$elev1500 <- ifelse(aoi3$elev1500 > 0.7, 1, 0)
aoi3 <- aoi3 %>%  group_by(elev1500) %>% summarise() #%>% ungroup()
st_write(aoi3, 'SpatialData/intermediate/elevationRegions.gpkg', append = F)

##Vector output
sf_use_s2(FALSE)
aoi1 <-  st_read('SpatialData/inputs/ET_LHZ_2018.shp')
aoi1$elev1500 <- exact_extract(elevation1500, aoi1, "mean")
aoi1$elevBin <- aoi1$elev1500 < 0.7
aoi1$elev2300 <- exact_extract(elevation2300, aoi1, "mean")
elev <- aoi1 %>%  group_by(elevBin) %>% summarise()
aoi1$grouping <- ifelse(grepl("astoral", aoi1$LZTYPE), "(Agro)pastoral", #Lowland
                        ifelse(!grepl("astoral", aoi1$LZTYPE) & aoi1$elev2300 >= 0.5, "Highland mixed", #Highland
                               ifelse(!grepl("astoral", aoi1$LZTYPE) & aoi1$elev1500 >= 0.5 & aoi1$elev2300 < 0.5, "Midland mixed", "Lowland mixed")))

aoi1 <- aoi1 %>%  group_by(grouping) %>% summarise() #%>% ungroup()

st_write(aoi1, 'SpatialData/intermediate/zones.gpkg')
