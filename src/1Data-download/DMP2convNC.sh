##!/bin/sh

for file in $(ls ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/*.nc);
do
gdal_translate NETCDF:$file:GDMP $(echo "$file" | cut -f 1 -d '.').0.1.tif >/dev/null
rm $file
done
