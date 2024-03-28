##!/bin/sh
#$ -N PhenDL              
#$ -cwd                  
#$ -l h_rt=24:00:00 
#$ -l h_vmem=2G
#$ -M sfraval@ed.ac.uk
for file in $(curl -s https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q2.006/2016.01.01/ |
                  grep href |
                  sed 's/.*href="//' |
                  sed 's/".*//' |
                  grep 'h20v07\|h20v08\|h20v09\|h21v07\|h21v08\|h21v09\|h21v10\|h22v07\|h22v08\|h22v09' |
				  grep '.*\.hdf$'); do

wget -nc --load-cookies .urs_cookies --save-cookies .urs_cookies --keep-session-cookies https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q2.006/2016.01.01/$file -P ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/PhenologyModis
done

