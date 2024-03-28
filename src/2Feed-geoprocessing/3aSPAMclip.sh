#!/bin/sh


gdaltindex clip.shp SpatialData/inputs/Feed_DrySeason/DMP/c_gls_DMP300-RT5-DMP_201901100000_ETHIOPIA_PROBAV_V1.0.1_clip.tif

for file in $(ls SpatialData/inputs/Feed_DrySeason/LandUse/*4326.tif);
do
gdalwarp -cutline clip.shp -crop_to_cutline $file $(echo "$file" | cut -f 1 -d '.')_clip.tif >/dev/null

done
