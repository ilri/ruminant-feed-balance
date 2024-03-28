##!/bin/sh

mkdir ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas
wget -P ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Feb2021_Public_shp.zip
unzip -j "../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPA_Feb2021_Public_shp.zip" "WDPA_Feb2021_Public_shp_2.zip" -d ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/
find ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/ ! -name WDPA_Feb2021_Public_shp_2.zip -type f -exec rm -f {} + #Remove files
unzip ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPA_Feb2021_Public_shp_2.zip -d ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/

gdal_rasterize -a STATUS_YR -l WDPA_Feb2021_Public_shp-polygons ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPA_Feb2021_Public_shp-polygons.shp ../2Feed-geoprocessing/SpatialData/inputs/ProtectedAreas/WDPAGlobal.tif
