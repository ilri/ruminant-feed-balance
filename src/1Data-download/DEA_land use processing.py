%matplotlib inline

import datacube
import xarray as xr
import matplotlib.pyplot as plt
import geopandas as gpd

from deafrica_tools.plotting import display_map
from deafrica_tools.bandindices import calculate_indices
from deafrica_tools.datahandling import load_ard
from deafrica_tools.spatial import xr_rasterize
from deafrica_tools.classification import HiddenPrints
from datacube.utils import geometry
from datacube.utils.geometry import assign_crs
from deafrica_tools.plotting import rgb, map_shapefile
from datacube.utils.cog import write_cog

dc = datacube.Datacube(app='cropland_extent')


lat, lon = 12.5422419,-1.99 # Burkina
buffer = 3.5 #Burkina

time_period = ('2019')

resolution=(-20, 20)

#join lat, lon, buffer to get bounding box
lon_range = (lon - buffer, lon + buffer+1) #Burkina
lat_range = (lat + buffer, lat - buffer)

# generate a query object from the analysis parameters
query = {
    'time': time_period,
    'x': lon_range,
    'y': lat_range,
    'resolution':resolution
}


query

# now load the crop-mask using the query
cm = dc.load(product='crop_mask_sahel', measurements=['mask'],
             **query, resampling="average").squeeze()#output_crs="EPSG:4326", 
print(cm)


#mask = xr_rasterize(gdf, cm)
#cm = cm.where(mask)
#cm = cm.coarsen(x=15, boundary = "pad").mean(skipna = False).coarsen(y=15, boundary = "pad").mean(skipna = False)
cm = cm.fillna(0)
cm = cm.coarsen(x=15, y =15, boundary = "pad").mean()
out = cm.to_array()

# Write GeoTIFF to a location
write_cog(geo_im=out,
          fname='LUcropsDEA300_Burkina.tif',
          overwrite=True)
		  
		  
#Ethiopia

lat, lon = 10.5615, 40.691 # Ethiopia

buffer = 8

time_period = ('2019')

resolution=(-20, 20)

#join lat, lon, buffer to get bounding box
lon_range = (lon - buffer, lon + buffer)
lat_range = (lat + buffer-3.5, lat - buffer)

#vector_file = 'aoi0.shp'

# generate a query object from the analysis parameters
query = {
    'time': time_period,
    'x': lon_range,
    'y': lat_range,
    'resolution':resolution
}


query


#geom = geometry.Geometry(geom=gdf.iloc[0].geometry, crs=gdf.crs)

# Update dc query with geometry
#query.update({'geopolygon': geom})

# Load data
cm = dc.load(product='crop_mask',
                     region='eastern',
                     measurements=['mask'], 
             **query, resampling="average").squeeze()
print(cm)

#mask = xr_rasterize(gdf, cm)
#cm = cm.where(mask)
#cm = cm.coarsen(x=15, boundary = "pad").mean(skipna = False).coarsen(y=15, boundary = "pad").mean(skipna = False)
cm = cm.fillna(0)
cm = cm.coarsen(x=15, y =15, boundary = "pad").mean()
out = cm.to_array()

# Write GeoTIFF to a location
write_cog(geo_im=out,
          fname='LUcropsDEA300_Ethiopia.tif',
          overwrite=True)