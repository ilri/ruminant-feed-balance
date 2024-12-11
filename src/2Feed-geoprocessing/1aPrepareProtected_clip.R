# Prepare protected area layer
# Author: Simon Fraval
# Last modified by John Mutua on 9/12/2024

# # Install required packages
# install.packages("terra")

#Libraries
library(terra)

# study area
country <- "Nigeria"

root <- "."
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/ProtectedAreas")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/ProtectedAreas"); dir.create(outdir, F, T)

# load livelihoods vectors
wdpaNGA <- vect(paste0(indir, "/WDPA_WDOECM_Oct2024_Public_NGA.shp"))

# reference raster?
#r <- rast(ext(wdpaNGA), resolution = 0.00297619, crs = crs(wdpaNGA))
dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

# rasterize the SpatVector
wdpaNGA <- rasterize(wdpaNGA, dmpTemp, field = "STATUS_YR")

# Write output
writeRaster(wdpaNGA, paste0(outdir, "/WDPAGlobal.tif"), overwrite=TRUE)

# # plotting
# library(tidyterra)
# aoi1 <- read_sf(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/AdminBound/aoi1.shp"))
# WDPAGlobal <- rast(paste0(outdir, "/WDPAGlobal.tif"))
# WDPAGlobal[WDPAGlobal>=1]<-1
# WDPAGlobal[WDPAGlobal<=0]<-NA
# WDPAGlobal <- WDPAGlobal %>% crop(., ext(aoi1)) %>% mask(., mask = aoi1)
# ggplot() + geom_sf(data = aoi1, colour = "black", show.legend = F) + 
#   geom_spatraster(data = WDPAGlobal) + 
#   geom_sf(data = aoi1, colour = "black", fill = NA, show.legend = F) + 
#   scale_fill_continuous(na.value = NA, name="Protected areas")
