# Check livestock numbers over years
# Layer provided at 10km spatial resolution, units: animals per km2
# Author: John Mutua
# Last modified on 11/11/2024

# # Install required packages
# install.packages("terra")

library(terra)
library(sf)

country <- "Nigeria"

# root folder
root <- "."
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4")
outdir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/GLW4"); dir.create(outdir, F, T)

aoi2 <- st_read(paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country, "/aoi0.shp"))

# cattle 2020
cattle2020 <- terra::rast(paste0("/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance/src/1Data-download/SpatialData/inputs/GLW4/GLW4-2020.D-DA.CTL.tif"))
cattle2020 <- terra::crop(cattle2020, aoi2)
cattle2020 <- mask(cattle2020, aoi2)
sum_cattle2020 <- global(cattle2020, fun = "sum", na.rm = TRUE)
sum_cattle2020

# cattle - 2015
cattle2015 <- terra::rast("/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance/src/1Data-download/SpatialData/inputs/GLW3/5_Ct_2015_Da.tif")
cattle2015<- terra::crop(cattle2015, aoi2)
cattle2015 <- mask(cattle2015, aoi2)
sum_cattle2015 <- global(cattle2015, fun = "sum", na.rm = TRUE)
sum_cattle2015

plot(aoi2)
