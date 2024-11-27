var roi = ee.FeatureCollection('FAO/GAUL/2015/level0').filter(ee.Filter.eq('ADM0_NAME', 'Nigeria'));

// Load ESA image
var esa = ee.ImageCollection('ESA/WorldCover/v100').first()
.clip(roi);
var visualization = {
  bands: ['Map'],
};

// Map.centerObject(esa);

// Map.addLayer(esa, visualization, 'Landcover');

Export.image.toDrive({
  image: esa,
  description: 'Nigeria',
  folder: "Landcover_data",
  region: roi,
  scale: 100,
  crs: "EPSG:4326",
  maxPixels: 1e10
});