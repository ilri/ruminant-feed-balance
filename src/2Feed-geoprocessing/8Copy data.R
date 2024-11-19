# Copy files to 3Balance-estimates folder
# Author: John Mutua
# Last modified on 12/11/2024

# avoid scientific notation
options(scipen = 999)

# # Install required packages
# install.packages("raster")
# install.packages("stars")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("exactextractr")
# install.packages("purrr")
# install.packages("readr")

# Load libraries

# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

# paths
indir <- paste0(root, "/src"); dir.create(indir, F, T)
spatialDataOut <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs"); dir.create(spatialDataOut, F, T)

# administrative boundaries
admin_src <- paste0(indir, "/1Data-download/SpatialData/inputs/AdminBound/", country)
admin_files <- list.files(path = admin_src, pattern = "aoi|gadm40_NGA", full.names = TRUE)
admin_dest <- paste0(spatialDataOut, "/")
file.copy(admin_files, admin_dest, overwrite = TRUE)

# livelihood zones -- .geojson
lhz_src <- paste0(indir, "/1Data-download/SpatialData/inputs/AggregationZones/FEWS_LZ.geojson")
lhz_dest <- paste0(spatialDataOut, "/FEWS_LZ.geojson")
file.copy(lhz_src, lhz_dest, overwrite = TRUE)

# livelihood zones -- .shp
lhz_src <- paste0(indir, "/1Data-download/SpatialData/inputs/AggregationZones")
lhz_files <- list.files(path = lhz_src, pattern = "NG_LHZ_2018", full.names = TRUE)
lhz_dest <- paste0(spatialDataOut, "/")
file.copy(lhz_files, lhz_dest, overwrite = TRUE)

# livelihood zones -- Provided by the ministry
lhz_src <- paste0(indir, "/1Data-download/SpatialData/inputs/AggregationZones")
lhz_files <- list.files(path = lhz_src, pattern = "Ecological_and_Feed_Distribution", full.names = TRUE)
lhz_dest <- paste0(spatialDataOut, "/")
file.copy(lhz_files, lhz_dest, overwrite = TRUE)

# Feed parameters
feedPar_src <- paste0(indir, "/1Data-download/Tables/inputs/CropParams")
feedPar_files <- list.files(path = feedPar_src, full.names = TRUE)
feedPar_dest <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams"); dir.create(feedPar_dest, F, T)
file.copy(feedPar_files, feedPar_dest, overwrite = TRUE)

# Livestock parameters
livePar_src <- paste0(indir, "/1Data-download/Tables/inputs/LivestockParams")
livePar_files <- list.files(path = livePar_src, full.names = TRUE)
livePar_dest <- paste0(root, "/src/3Balance-estimates/", country, "/LivestockParams"); dir.create(livePar_dest, F, T)
file.copy(livePar_files, livePar_dest, overwrite = TRUE)

# Cropping days
crop_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/", country, "/Cropping_days")
crop_dest <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams/Cropping_days"); dir.create(crop_dest, F, T)
crop_files <- list.files(crop_src, full.names = TRUE, recursive = TRUE)
file.copy(crop_files, crop_dest, recursive = TRUE)

# Feed quantity
feedQu_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/", country, "/Feed_DrySeason/Feed_quantity")
feedQu_dest <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/Feed_quantity"); dir.create(feedQu_dest, F, T)
feedQu_files <- list.files(feedQu_src, pattern = "_(2020|2021|2022|2023)\\.tif$", full.names = TRUE, recursive = TRUE)
file.copy(feedQu_files, feedQu_dest, recursive = TRUE)

# Burned areas
burned_folders <- c("2020", "2021", "2022", "2023")
for (burned_folder in burned_folders) {
  burned_src <- file.path(paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/", country, "/Burned/", burned_folder))
  burned_dest <- file.path(burned_dest <- paste0(root, "/src/3Balance-estimates/", country,"/SpatialData/inputs/Burned/", burned_folder))
  if (!dir.exists(burned_dest)) {dir.create(burned_dest, recursive = TRUE)}
  burned_files <- list.files(burned_src, pattern = "Months\\.tif$", full.names = TRUE, recursive = TRUE)
  file.copy(burned_files, file.path(burned_dest, basename(burned_files)), recursive = TRUE)
}

# SPAM 2020
spam_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/", country, "/SPAM2020")
spam_dest <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/SPAM2020"); dir.create(spam_dest, F, T)
spam_files <- list.files(spam_src, pattern = "_(a|frac)\\.tif$", full.names = TRUE, ignore.case = TRUE)
file.copy(spam_files, spam_dest, recursive = TRUE)