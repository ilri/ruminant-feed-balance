##!/bin/sh

for file in $(ls ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/*.zip);
do
unzip -j $file -d ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/DMP/
rm $file
done
