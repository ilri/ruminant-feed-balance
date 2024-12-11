# Data download
# Author: John Mutua
# Last modified: 11/11/2024

# Learning outcomes
# -Know what data is needed for livestock feed balance modelling
# -Gather spatial data, import them into R and produce maps using scripts

# # Install required packages
# install.packages("geodata")
# install.packages("sf")
# install.packages("RCurl")
# install.packages("httr")
# install.packages("rvest")
# install.packages("googledrive")
# install.packages("tidyverse")
# install.packages("FAOSTAT")

# Load required packages
library(geodata)
library(sf)
library(RCurl)
library(httr)
library(rvest)
library(googledrive)
library(tidyverse)
library(rvest)
library(FAOSTAT)

# Setting the working directory
root <- "."

#### Administrative boundaries----------------------------------------------------------------------------

# study area
country <- "Nigeria"

outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/AdminBound/", country); dir.create(outdir, F, T)

admin_levels <- c("0", "1", "2")
for(admin_level in admin_levels){
  aoi <- geodata::gadm(country = "NGA", level=admin_level, path = paste0(outdir), version="latest") %>% sf::st_as_sf()
  write_sf(aoi, paste0(outdir, "/gadm40_NGA_", admin_level, ".shp"), append = FALSE)
  write_sf(aoi, paste0(outdir, "/aoi", admin_level, ".shp"), append = FALSE)
}

#### Ruminant systems aggregation zones ----------------------------------------------------------------------------
# Available here https://shapefiles.fews.net/LHZ/NG_LHZ_2018.zip
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/AggregationZones"); dir.create(outdir, F, T)

download.file(
  url = paste0("https://shapefiles.fews.net/LHZ/NG_LHZ_2018.zip"),
  destfile = paste0(outdir, "/NG_LHZ_2018.zip"), quiet = TRUE)

unzip(zipfile = paste0(outdir, "/NG_LHZ_2018.zip"), exdir = paste0(outdir, "/"))

# Other zonations maps
library(googledrive)
drive_deauth()
drive_user()
public_file <-  drive_get(as_id("10HsGspftDNgq-fjAjeUwB8QsmHZWUJyT"))
drive_download(public_file, path = paste0(outdir, "/Ecological_and_Feed_Distribution.zip"), overwrite = TRUE)

unzip(zipfile = paste0(outdir, "/Ecological_and_Feed_Distribution.zip"), exdir = paste0(outdir, "/"))

#### Land use----------------------------------------------------------------------------
# Available here <https://zenodo.org/records/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt>
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/LandUse"); dir.create(outdir, F, T)

# Land use classes of interest
land_use_classes <- c("Tree", "Grass", "Crops", "Shrub")

# Download the file
lapply(land_use_classes, function(land_use_class){
  
  if (!file.exists(paste0(outdir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"))){
    
    cat("Downloading: ", land_use_class, "\n")
    
    download.file(
      url = paste0("https://zenodo.org/records/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"),
      destfile = paste0(outdir, "/PROBAV_LC100_global_v3.0.1_2019-nrt_", land_use_class, "-CoverFraction-layer_EPSG-4326.tif"), quiet = TRUE)
    
  }else{cat("File already exists: ", land_use_class, "\n")}
})

# Other land cover datasets - for use in creating cropland mask
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/LandCover"); dir.create(outdir, F, T)

drive_deauth()
drive_user()

#folder link to id
public_folder <- "https://drive.google.com/drive/folders/1SsZVX3LDuvGweJGe8sIeDR0me-WOqH45"
folder_id <- drive_get(as_id(public_folder))

#find files in folder
public_files <- drive_ls(folder_id)

for(i in 1:nrow(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
}

#### Tree cover----------------------------------------------------------------------------
# Available here <https://doi.org/10.1038/s41467-023-37880-4>
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/TreeCover"); dir.create(outdir, F, T)

if (!file.exists(paste0(outdir, "/ps_africa_treecover_2019_100m_v1.0.tif"))){
  cat("Downloading ", "tree cover", "\n")
  download.file(
    url = paste0("https://zenodo.org/records/7764460/files/ps_africa_treecover_2019_100m_v1.0.tif"),
    destfile = paste0(outdir, "/ps_africa_treecover_2019_100m_v1.0.tif"), quiet = TRUE)
}else{cat("File already exists: ", "tree cover", "\n")}

#### Crop type and distribution----------------------------------------------------------------------------
# Available here <https://mapspam.info>, and here <https://www.dropbox.com/sh/3j7l50i6uue0z1v/AABeqgE2IOv6_VV6sN_zOHAUa?dl=0&e=1>
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Feed/CropType"); dir.create(outdir, F, T)

download.file(
  url = paste0("https://www.dropbox.com/scl/fo/808qb807xw4olh8z5pagd/APYE4A4ApAbKhlcfWspRxcg/Global_Geotiff?e=1&file_subpath=%2Fspam2020V0r1_global_harvested_area&preview=spam2020V0r1_global_harvested_area.zip&rlkey=mkmj10j7ub49dzrqo4qec25gp&subfolder_nav_tracking=1&st=34q63fux&dl=1"),
  destfile = paste0(outdir, "/spam2020V1r0_global.zip"), quiet = TRUE)

# Unzip the downloaded file (only the specific zip inside the archive)
unzip(zipfile = paste0(outdir, "/spam2020V1r0_global.zip"),
      files = "spam2020V0r1_global_physical_area.zip",
      exdir = paste0(outdir))

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)
  
# Identify files to remove (all files except the one to keep)
files_to_remove <- all_files[!basename(all_files) %in% "spam2020V0r1_global_physical_area.zip"]
  
# Remove the files
file.remove(files_to_remove)
  
# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

# Unzip the second archive
unzip(zipfile = paste0(outdir, "/spam2020V0r1_global_physical_area.zip"), exdir = paste0(outdir, "/"))

# List all files in the folder
cropPhysicalArea <- list.files(paste0(outdir, "/spam2020V0r1_global_physical_area"), full.names = TRUE)

#### Protected areas----------------------------------------------------------------------------
# Available here https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
options(timeout = 3600)
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/ProtectedAreas"); dir.create(outdir, F, T)

# Download the file
download.file(
  url = "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip",
  destfile = paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip"))

zipped_files <- c("WDPA_WDOECM_Oct2024_Public_NGA_shp_0.zip", "WDPA_WDOECM_Oct2024_Public_NGA_shp_1.zip", "WDPA_WDOECM_Oct2024_Public_NGA_shp_2.zip")

for (zipped_file in zipped_files){
  # Unzip the downloaded file (only the specific zip inside the archive)
  unzip(zipfile = paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA_shp.zip"), files = paste0(zipped_file), exdir = paste0(outdir))
}

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

# Identify files to remove (all files except the one to keep)
files_to_remove <- all_files[!basename(all_files) %in% zipped_files]

# Remove the files
file.remove(files_to_remove)

# List all files in the folder
all_files <- list.files(paste0(outdir), full.names = TRUE)

for(zipped_file in zipped_files){
  
  file_name = basename(paste0(outdir, "/", zipped_file))
  folder_name <- sub("\\.zip$", "", file_name)
  
  # Unzip the second archive
  unzip(zipfile = paste0(outdir, "/", file_name),
        exdir = paste0(outdir, "/", folder_name))
}

# Use list.files() to search for files that end with "NGA_shp-polygons.shp"
shp_files <- list.files(outdir, pattern = "NGA_shp-polygons\\.shp$", recursive = TRUE, full.names = TRUE)

# Read all shapefiles into a list of sf objects
shp_files <- lapply(shp_files, sf::st_read)

# Combine all shapefiles into one
WDPA_WDOECM_Oct2024_Public_NGA <- do.call(rbind, shp_files)

# Write combined file
sf::st_write(WDPA_WDOECM_Oct2024_Public_NGA, paste0(outdir, "/WDPA_WDOECM_Oct2024_Public_NGA.shp"))

#### Above ground dry matter productivity----------------------------------------------------------------------------
# Available here https://land.copernicus.eu/en/products/vegetation/dry-matter-productivity-v1-0-300m#download
options(timeout=3600)
outdir <- paste0("/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/Geodata/DMP"); dir.create(outdir, F, T)

# Download manifest
download.file(
  url <- "https://globalland.vito.be/download/manifest/dmp_300m_v1_10daily_netcdf/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt",
  destfile = paste0(outdir, "/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt"))

# Read in manifest
dmp_manifest <- readLines(paste0(outdir, "/manifest_clms_global_dmp_300m_v1_10daily_netcdf_latest.txt"))

# Select files of interest
#dmp_manifest_list <- grep("RT6_2023", dmp_manifest, fixed=TRUE, value=TRUE) #select a file for each day

# Combine the patterns to search for
patterns <- "RT5_2020|RT6_2020|RT6_2021|RT2_202211100000|RT2_202211200000|RT2_202211300000|RT2_202212100000|RT2_202212200000|RT2_202212310000|RT6_2022|RT6_2023"

# Use grep to search for any of the patterns in dmp_manifest
dmp_manifest_list <- grep(patterns, dmp_manifest, value=TRUE) #select a file for each day

# Define files to exclude
exclude_patterns <- "RT5_202007100000|RT5_202007200000|RT5_202007310000|RT5_202008100000|RT5_202008200000|RT5_202008310000|RT6_202211100000|RT6_202211200000"

# Exclude the specific files from the results
dmp_manifest_list <- grep(exclude_patterns, dmp_manifest_list, invert=TRUE, value=TRUE)

for (i in dmp_manifest_list){
  # Extract the file name
  #file_name <- basename(sub("OLCI_V1.*", "OLCI_V1", i))
  
  file_name_base <- basename(i)
  
  filenamep1 <- substr(file_name_base, 1, 13)
  filenamep2 <- substr(file_name_base, 18, 29)
  
  file_name <- paste0(filenamep1, "RT6_", filenamep2, "_GLOBE_OLCI_V1.1.2.nc")
  
  if(!file.exists(paste0(outdir, "/", file_name))){
    cat("Downloading: ", file_name, "\n")
    download.file(url = i, destfile = paste0(outdir, "/", file_name))
  }else {cat("File already exists: ", file_name, "\n")}
}

#### Phenonology----------------------------------------------------------------------------
# Available here https://www.earthdata.nasa.gov/
yearList <- c("2020", "2021", "2022", "2023")

for(year in yearList){

  outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/PhenologyModis/", year); dir.create(outdir, F, T)
  
  # Modis URL
  url <- paste0("https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q2.061/", year, ".01.01/")
  
  # Get the HTML content of the URL
  page <- read_html(url)

  # Extract href links
  links <- page %>% html_nodes("a") %>% html_attr("href")

  # Filter the links based on the specified patterns
  #pattern <- "h20v07|h20v08|h20v09|h21v07|h21v08|h21v09|h21v10|h22v07|h22v08|h22v09"
  pattern <- "h18v07|h18v08|h19v07|h19v08"
  hdf_links <- links[grep(pattern, links)]
  hdf_links <- hdf_links[grep("\\.hdf$", hdf_links)]

  #paste0(url, hdf_links[1])
  
  for (file in hdf_links){
  if (!file.exists(paste0(outdir, "/", file))) {
    
    # Define your Earthdata credentials
    username <- Sys.getenv("EARTHDATA_USERNAME") # replace with your Earthdata username
    password <- Sys.getenv("EARTHDATA_PASSWORD") # replace with your Earthdata password
    
    cat("Downloading: ", file, "\n")
    download.file(url = paste0("https://", username, ":", password, "@", url, file), 
                  destfile = paste0(outdir, "/", file), quiet = TRUE)
  }else{cat("File already exists: ", file, "\n")}
  }
}

#### Burned areas----------------------------------------------------------------------------
# Available here https://land.copernicus.eu/en/products/vegetation/burnt-area-v3-1-monthly-300m
options(timeout=3600)

# Download manifest
download.file(url <- "https://globalland.vito.be/download/manifest/ba_300m_v3_monthly_netcdf/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt",
              destfile = paste0(root, "/src/1Data-download/SpatialData/inputs/Burned/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt"))

# Read in manifest
dmp_manifest <- readLines(paste0(root, "/src/1Data-download/SpatialData/inputs/Burned/manifest_clms_global_ba_300m_v3_monthly_netcdf_latest.txt"))

yearList <- c("2020", "2021", "2022", "2023")

for(year in yearList){
  
  outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/Burned/", year); dir.create(outdir, F, T)
  
  # Select files of interest
  dmp_manifest_list <- grep(paste0("NTC_", year), dmp_manifest, fixed=TRUE, value=TRUE) #select a file for each day
  for (i in dmp_manifest_list){
    # Extract the file name
    file_name <- basename(i)
    if(!file.exists(paste0(outdir, "/", file_name))){
      download.file(url = i, destfile = paste0(outdir, "/", file_name))
      
    }else{cat("File already exists:", file_name, "\n")}
  }
}

#### Feed parameters----------------------------------------------------------------------------
# Available here https://drive.google.com/drive/folders/1SpB1p9i4MGU1gMahF4M3Uc-HZr8FGoqd
outdir <- paste0(root, "/src/1Data-download/Tables/inputs/", country, "/CropParams"); dir.create(outdir, F, T)

drive_deauth()
drive_user()

#folder link to id
public_folder <- "https://drive.google.com/drive/folders/1SpB1p9i4MGU1gMahF4M3Uc-HZr8FGoqd"
folder_id <- drive_get(as_id(public_folder))

#find files in folder
public_files <- drive_ls(folder_id)

for(i in 1:nrow(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
}

# Or here https://feedsdatabase.ilri.org
outdir <- paste0(root, "/src/1Data-download/Tables/inputs/", country, "/CropParams"); dir.create(outdir, F, T)

# countries <- c("Nigeria", "Cameroon", "Somalia", "Ethiopia")
country_ids = c("All")#, "107", "112", "116", "117")

# Feed items
crop_residue_items <- c("Wheat straw", "Barley straw", "Common bean straw", "Chickpea straw", "Pigeonpea straw", "Lentil straw", "Banana pseudostem", "Soyabean straw", "Sugar cane tops", "fodder beet") 
#pasture_items <- c("Natural pasture")
#cultivated_forages_items <- c("Napier grass", "Rhodes grass")

feed_items <- c(crop_residue_items) #, pasture_items, cultivated_forages_items)

# Loop through countries
country_tables <- lapply(X = country_ids, FUN = function(country){
  
  # Loop through feed items
  feed_item_tables <- lapply(X = feed_items, FUN = function(feed_item){
    
    feed_item <- gsub(" ","%20", feed_item)
    feed_title <- feed_item
    
    paste0("Grabbing: ", feed_item, " ", "page")
    
    #feed_url <- html_session(paste0("https://feedsdatabase.ilri.org/search/", feed_item, "?title=", feed_title, "&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=", country, "&combine="), httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
    
    # feed_url <- session(paste0("https://feedsdatabase.ilri.org/search/", feed_item,
    #                            "?title=", feed_title, 
    #                            "&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=", 
    #                            country, "&combine="))
    
    feed_url <- session(paste0("https://feedsdatabase.ilri.org/search/", feed_item, 
                               "?title=", feed_title, 
                               "&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=", 
                               country, "&combine="), httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
    
    page_results<- feed_url %>% 
      read_html() %>% 
      html_nodes("table") %>% 
      html_table(fill = T) %>% 
      lapply(., function(x) setNames(x, c("Reference", "DM", "ADF", "NDF", "ADL", 
                                          "CP", "OM",	"P", "Ca", "Na", "Fe", "K", 
                                          "Mg", "Cu",	"Mn",	"Zn",	"IVDMD", "ME",	
                                          "NEm",	"NEg",	"NEl",	"Country")))
    
    results_df <- purrr::map_df(page_results, data.frame) %>% 
      mutate(Feed_item = feed_item) %>% 
      mutate(Feed_item = str_replace_all(Feed_item, "%20", " "))
    
  })
  
  # Remove empty dataframes
  feed_item_tables <- feed_item_tables[sapply(feed_item_tables, function(x) dim(x)[1]) > 0]
  
  feed_item_tables <- lapply(X = 1:length(feed_item_tables), FUN = function(i){
    
    df <- feed_item_tables[[i]]
    results_df <- df %>% 
      mutate_at(c(2:21), as.numeric)
    
  })
  
  # Combine all dataframes and remove unnecessary rows
  feed_quality_data <- purrr::map_df(feed_item_tables, data.frame) %>% 
    filter(!grepl("Page COUNT|Page AVG|Page MIN|Page MAX|Total COUNT|Total AVG|Total MIN|Total MAX", Reference))
  
  #return(feed_quality_data)
  
})

feedQuality <- purrr::map_df(country_tables, data.frame)

# Write outputs
write_csv(feedQuality, paste0(outdir, "/feedQuality_SSAdb.csv"))

#### Livestock population----------------------------------------------------------------------------
# Available here https://data.apps.fao.org/catalog//iso/9d1e149b-d63f-4213-978b-317a8eb42d02
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4"); dir.create(outdir, F, T)

speciesCategories <- c("CTL", "GTS", "SHP", "PGS", "HRS") 

for (speciesCategory in speciesCategories){
  
  if(speciesCategory == "HRS"){
    drive_deauth()
    drive_user()
    
    public_file <-  drive_get(as_id("1SE5vHgLVexjv_pdjQjVSoWLaL-v7MIvc"))
    file_name <- public_file$name
    drive_download(public_file, path = paste0(outdir, "/GLW4-2020.D-DA.",speciesCategory,".tif"), overwrite = TRUE)
    
  }else{
    producLink = paste0("https://storage.googleapis.com/fao-gismgr-glw4-2020-data/DATA/GLW4-2020/MAPSET/D-DA/GLW4-2020.D-DA.",speciesCategory,".tif")
    
    file_name <- basename(producLink)
    
    if (!file.exists(paste0(outdir, "/", file_name))){
      download.file(
        url = paste0("https://storage.googleapis.com/fao-gismgr-glw4-2020-data/DATA/GLW4-2020/MAPSET/D-DA/GLW4-2020.D-DA.",speciesCategory,".tif"),
        destfile = paste0(outdir, "/", file_name), quiet = TRUE)
    }else {cat("File already exists:", file_name, "\n")}
  }
}

# Download livestok population from FAOSTATS
outdir <- paste0(root, "/src/1Data-download/Tables/inputs/", country, "/LivestockParams"); dir.create(outdir, F, T)

# Load crop and livestock production data
cl_production <- get_faostat_bulk(code = "QCL", data_folder = outdir)

# Unzip the second archive
file_path <- paste0(paste0(outdir, "/Production_Crops_Livestock_E_All_Data_(Normalized).zip"))
file_name <- sub("\\.zip$", "", basename(file_path))
unzip(zipfile = paste0(file_path), exdir = paste0(outdir, "/", file_name))

FAOLivestock <- read_csv(paste0(outdir, "/", file_name, "/Production_Crops_Livestock_E_All_Data_(Normalized).csv")) %>% 
  filter(Item %in% c("Cattle", "Goats", "Sheep"), Area == "Nigeria", Year %in% c("2019", "2020", "2021", "2022", "2023"))

# Calculate growth rates and estimate 2023 population
FAOLivestock_2023 <- FAOLivestock %>%
  group_by(Item) %>%
  arrange(Year) %>%
  mutate(growth_rate = (Value - lag(Value)) / lag(Value)) %>%
  filter(!is.na(growth_rate)) %>% # Remove NA growth rates
  summarize(Year = 2023, Value = last(Value) * (1 + last(growth_rate)), .groups = "drop") %>% 
  mutate(`Area Code`=159, `Area Code (M49)`="'566", Area="Nigeria", `Item Code` = case_when(Item == "Cattle" ~ 866,
                                                                                            Item == "Sheep" ~ 976,
                                                                                            Item == "Goats" ~ 1016,
                                                                                            TRUE ~ NA),
         `Item Code (CPC)` = case_when(Item == "Cattle" ~ "'02111",
                                 Item == "Sheep" ~ "'02122",
                                 Item == "Goats" ~ "'02123",
                                 TRUE ~ NA),
         `Element Code`=5111, Element = "Stocks", `Year Code`=2023, Unit="An", Flag="B", Note=NA,
         Value = round(Value, 0)) %>% 
  dplyr::select(`Area Code`, `Area Code (M49)`, Area, `Item Code`, `Item Code (CPC)`, Item, `Element Code`, Element, `Year Code`, Year, Unit, Value, Flag, Note)

# Add 2023 data to the original dataframe
FAOLivestock_2023 <- bind_rows(FAOLivestock, FAOLivestock_2023) %>% 
  mutate(Item = case_when(Item == "Cattle" ~ "Cattle", Item == "Sheep" ~ "Shoats", Item == "Goats" ~ "Shoats", TRUE ~ NA)) %>% 
  group_by(Item, Year) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) 

FAOLivestock_2023 <- FAOLivestock_2023 %>% 
  arrange(Item, Year) %>% 
  group_by(Item) %>%
  mutate(PcChange = (Value - lag(Value))/lag(Value)) %>%
  ungroup() 

write_csv(FAOLivestock_2023, paste0(outdir, "/FAOSTAT_livestock_data.csv"), append = FALSE)

# List all files in the current directory
FAO_items <- list.files(paste0(outdir), pattern = "Production_Crops", full.names = TRUE)

# Remove files and folders
sapply(FAO_items, function(FAO_item) {if (file.info(FAO_item)$isdir) {unlink(FAO_item, recursive = TRUE)} else {file.remove(FAO_item)}})

#### Livestock production systems----------------------------------------------------------------------------
# Available here https://drive.google.com/drive/folders/1QW7duDNsccAalfpUaV6VSfjeX_VGzrna?usp=sharing
outdir <- paste0(root, "/src/1Data-download/SpatialData/inputs/GLPS"); dir.create(outdir, F, T)

drive_deauth()
drive_user()

#folder link to id
public_folder = "https://drive.google.com/drive/folders/1QW7duDNsccAalfpUaV6VSfjeX_VGzrna"
folder_id = drive_get(as_id(public_folder))

#find files in folder
public_files = drive_ls(folder_id)

for(i in 1:nrow(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
  
  # Unzip the second archive
  folder_name <- sub("\\.zip$", "", file_name)
  unzip(zipfile = paste0(outdir, "/", file_name), exdir = paste0(outdir, "/", folder_name))
}


#### Livestock parameters----------------------------------------------------------------------------
# Available here https://drive.google.com/drive/folders/1-3N_kmMgcHr_tayylSxlMJAr-2PBGFXd
outdir <- paste0(root, "/src/1Data-download/Tables/inputs/", country, "/LivestockParams"); dir.create(outdir, F, T)

drive_deauth()
drive_user()
  
#folder link to id
public_folder = "https://drive.google.com/drive/folders/1-3N_kmMgcHr_tayylSxlMJAr-2PBGFXd"
folder_id = drive_get(as_id(public_folder))

#find files in folder
public_files = drive_ls(folder_id)

for(i in 1:nrow(public_files)){
  public_file <- public_files[i, ]
  file_name <- public_file$name
  drive_download(public_file, path = paste0(outdir, "/", file_name), overwrite = TRUE)
}
