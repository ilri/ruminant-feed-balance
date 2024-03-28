##!/bin/sh
for file in $(ls ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/*.tif);
do
rm $(echo "$file" | cut -f 1 -d '.').0.1.nc
done

