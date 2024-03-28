library(raster)
library(stars)
library(sf)
library(dplyr)
library(exactextractr)

sf_use_s2(FALSE)
aoi_sudSah <-  st_read('SpatialData/inputs/BF_LHZ_2014.shp')
aoi_sudSah$grouping <- ifelse(grepl("transhumant", aoi_sudSah$LZNAMEEN), "Sahel", "Sudanian")
aoi_sudSah <- aoi_sudSah %>%  group_by(grouping) %>% summarise()
st_write(aoi_sudSah, 'SpatialData/intermediate/regions.gpkg', append = F)

st_rasterize(sf = aoi_sudSah[, "grouping"], file = "SpatialData/intermediate/regions.tif")

sf_use_s2(FALSE)
aoi1 <-  st_read('SpatialData/inputs/BF_LHZ_2014.shp')
aoi1$grouping <- NA
aoi1$grouping[1:2] <- "Central mixed"
aoi1$grouping[c(3)] <- "North mixed"
aoi1$grouping[4] <- "(Agro)pastoral Sahel"
aoi1$grouping[c(5,7,8,9)] <- "Cropping"
aoi1$grouping[c(6)] <- "South mixed"
#aoi1$grouping <- ifelse(grepl("transhumant", aoi1$LZNAMEEN), "Sahel", "Sudanian")
aoi1 <- aoi1 %>%  group_by(grouping) %>% summarise() #%>% ungroup()

st_write(aoi1, 'SpatialData/intermediate/zones.gpkg', append = F)
