#Libraries
library(terra)

# study area
region <- "Nigeria"

root <- "/home/s2255815/rdrive/AU_IBAR/ruminant-feed-balance"
indir <- paste0(root, "/src/1Data-download/SpatialData/inputs/ProtectedAreas")
outdir <- paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas"); dir.create(outdir, F, T)

# load livelihoods vectors
wdpaNGA <- vect(paste0(indir, "/WDPA_WDOECM_Oct2024_Public_NGA.shp"))

# reference raster?
#r <- rast(ext(wdpaNGA), resolution = 0.00297619, crs = crs(wdpaNGA))
dmpTemp <- rast(paste0(root, "/src/2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT6_202301100000_GLOBE_OLCI_V1.1.2.tif"))

# rasterize the SpatVector
wdpaNGA <- rasterize(wdpaNGA, dmpTemp, field = "STATUS_YR")

# Write output
writeRaster(wdpaNGA, paste0(outdir, "/WDPAGlobal.tif"), overwrite=TRUE)