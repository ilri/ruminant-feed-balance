library(stars)

livelihood <- st_read('SpatialData/inputs/BF_LHZ_2014.shp')

liveOut <- st_rasterize(sf = livelihood[, "LZCODE"], file = "SpatialData/intermediate/livelihoodZones.tif")
