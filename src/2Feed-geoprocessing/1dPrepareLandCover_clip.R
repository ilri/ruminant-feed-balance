# Prepare DEA crop land
# Author: Simon Fraval
# Last modified by John Mutua on 26/11/2024

# # Install required packages
# install.packages("raster")

# Load required packages
library(terra)

terraOptions(tempdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# study area
country <- "Nigeria"

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
proPath <- paste0(root, "/src/1Data-download/SpatialData/inputs/LandCover")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/LandUse"); dir.create(outdir, F, T)

# Fix extent
dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

LUcropsDEA <- rast(paste0(proPath, "/LUcropsDEA100m_NGA.tif"))
LUcropsDEA <- project(LUcropsDEA, dmpTemp)
LUcropsDEA <- crop(LUcropsDEA, ext(dmpTemp))
LUcropsDEA <- resample(LUcropsDEA, dmpTemp, method="near")
LUcropsDEA <- mask(LUcropsDEA, mask = dmpTemp)

writeRaster(LUcropsDEA, paste0(outdir, "/LUcropsDEA300m_NGA.tif"), overwrite=TRUE)