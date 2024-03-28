


### Download land use
(Change year in files below. Defaulting to 2016)
```
mkdir ../2Feed-geoprocessing/SpatialData/inputs/Feed_DrySeason/LandUse
wget -nd -P SpatialData/inputs/Feed/LandUse/ https://zenodo.org/record/3939038/files/PROBAV_LC100_global_v3.0.1_2016-base_Crops-CoverFraction-layer_EPSG-4326.tif
wget -nd -P ../2Feed-geoprocessing/SpatialData/inputs/Feed/LandUse/ https://zenodo.org/record/3939038/files/PROBAV_LC100_global_v3.0.1_2016-base_Forest-Type-layer_EPSG-4326.tif #Not used
wget -nd -P ../2Feed-geoprocessing/SpatialData/inputs/Feed/LandUse/ https://zenodo.org/record/3939038/files/PROBAV_LC100_global_v3.0.1_2016-base_Tree-CoverFraction-layer_EPSG-4326.tif
wget -nd -P ../2Feed-geoprocessing/SpatialData/inputs/Feed/LandUse/ https://zenodo.org/record/3939038/files/PROBAV_LC100_global_v3.0.1_2016-base_Grass-CoverFraction-layer_EPSG-4326.tif
```

### Download tree cover data generated from 3m PlanetScope data, presented as % of 100m cell
Florian Reiner et al., 2023, https://doi.org/10.1038/s41467-023-37880-4

Download the most recent version here:
https://zenodo.org/records/7764460
Version 0.1 was used for the purposes of this publication. Please use the latest version for any adaptations of this code (v1.0 at time of writing).

The expected path is `../2Feed-geoprocessing/SpatialData/inputs/TreeCover/`

After downloading, clip the tiff to your ROI using `gdalwarp`, r `terra`, r `raster`, Python or your favourate desktop GIS package. For `gdalwarp`, you can adapt the shell script named SPAMclip.sh in the feed geoprocessing folder. Note: you will first need to run PrepareDMPclip.R. An untested R script is provided in the workflow for this

Known issues in v 1.0
- underprediction of tree clusters in shrublands
- some (few) false predictions of small trees in dry areas
- occasional inconsistent predictions within mosaics: seamlines along scene edges
- areas of underprediction in dense tropical forest due to lower quality scenes, see DRC
- overprediction (artifacts) in mountains, and occasionally desert dunes
- flowering trees in closed forests are mapped as gaps
- occasional confusion between understory or shrubs and trees in wood- and shrublands
- trees without leaves may not be mapped correctly

### Download SPAM harvested area data
Data for 2017 was used in this study, which was closest to our reference years of 2015-16
See readme here: https://www.dropbox.com/sh/3j7l50i6uue0z1v/AABeqgE2IOv6_VV6sN_zOHAUa?dl=0&e=1&preview=Readme.txt
Direct link for 2017: https://s3.amazonaws.com/mapspam-data/2017/ssa/v2.1/geotiff/spam2017v2r1_ssa_prod.geotiff.zip

The most recent version at time of writing was for 2020 https://www.dropbox.com/sh/3j7l50i6uue0z1v/AABeqgE2IOv6_VV6sN_zOHAUa?dl=0&e=1


### Download dry matter production (DMP) 

1) Log-in to copernicus and request download of DMP for ROI.
2) Create path and download
```
mkdir -p ../2Feed-geoprocessing/SpatialData/inputs/Feed/DMP/
wget -nd -mP ../2Feed-geoprocessing/SpatialData/inputs/Feed/DMP/ ftp://simonfraval:pass@ftp.copernicus.vgt.vito.be/
```

3) Remove files ending in xml and tiff
```
rm ../2Feed-geoprocessing/SpatialData/inputs/Feed/DMP/*.xml
rm ../2Feed-geoprocessing/SpatialData/inputs/Feed/DMP/*.tiff #7km preview
```

### Download burned area
(Same process as DMP)
1) Log-in to Copernicus and request download of burned are for ROI.
2) Create path and download
```
mkdir -p ../2-Feed-geoprocessing/SpatialData/inputs/Burned/
wget -nd -mP ../2Feed-geoprocessing/SpatialData/inputs/Burned/ ftp://simonfraval:pass@ftp.copernicus.vgt.vito.be/
```

3) Remove files ending in xml and tiff
```
rm ../2Feed-geoprocessing/SpatialData/inputs/Burned/*.xml
rm ../2Feed-geoprocessing/SpatialData/inputs/Burned/*.tiff #7km preview
```

### Download GADM shape files or geopackage file

Download: https://gadm.org/data.html
The current expected file path is `../2Feed-geoprocessing/SpatialData/inputs/aoi1.shp`

### Livelihood zones

Download FEWS livelihood zone for ROI using the FEWS data portal: https://fews.net/data/livelihood-zones
The current expected file path is `../3Balance-estimates/SpatialData/inputs
