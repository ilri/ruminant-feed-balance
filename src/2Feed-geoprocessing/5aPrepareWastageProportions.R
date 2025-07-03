# Prepare wastage layer
# Last modified by John Mutua on 11/04/2025

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(stars)
library(terra)

root <- "."

country <- "Nigeria"

indir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/", country, "/Wastage"); dir.create(outdir, F, T)

ref <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Nigeria/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202001100000_GLOBE_OLCI_V1.1.2.tif"))

regions <- rast(paste0(indir, "/regions.tif"))

# reclassification matrix
rcl <- matrix(c(
  1, 0.85,
  2, 0.50,
  3, 0.60
), ncol = 2, byrow = TRUE)

# Apply reclassification
feedWastage <- classify(regions, rcl, others = NA)  # unlisted values become NA

feedWastage <- feedWastage %>% 
  crop(., ref) %>% 
  resample(., ref)

writeRaster(feedWastage, paste0(outdir, "/feedWastage.tif"), datatype = "FLT4S", overwrite=TRUE)
