# root folder
root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"

country <- "Nigeria"

# paths
indir <- paste0(root, "/src"); dir.create(indir, F, T)
spatialDataOut <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs"); dir.create(spatialDataOut, F, T)

# administrative boundaries
admin_src <- paste0(indir, "/1Data-download/SpatialData/inputs/AdminBound")
admin_files <- list.files(path = admin_src, pattern = "gadm40_BFA", full.names = TRUE)
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

# Cropping days
crop_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/Cropping_days")
crop_dest <- paste0(root, "/src/3Balance-estimates/", country, "/CropParams/Cropping_days"); dir.create(crop_dest, F, T)
crop_files <- list.files(crop_src, full.names = TRUE, recursive = TRUE)
file.copy(crop_files, crop_dest, recursive = TRUE)

# Feed quantity
feedQu_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/Feed_quantity")
feedQu_dest <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/Feed_quantity"); dir.create(feedQu_dest, F, T)
feedQu_files <- list.files(feedQu_src, pattern = "_(2020|2021|2022|2023)\\.tif$", full.names = TRUE, recursive = TRUE)
file.copy(feedQu_files, feedQu_dest, recursive = TRUE)

# Burned areas
burned_src <- paste0(indir, "/2Feed-geoprocessing/SpatialData/inputs/Burned")
burned_dest <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/Burned"); dir.create(burned_dest,  F, T)
burned_files <- list.files(burned_src, pattern = "Months\\.tif$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
file.copy(burned_files, burned_dest, recursive = TRUE)