
# Extract results at state

library(terra)
#library(stars)
library(sf)
library(exactextractr)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(stars)
library(raster)
library(ggplot2)
library(readr)
library(shadowtext)

# avoid scientific notation
options(scipen = 999)

#rasterOptions(tmpdir = EDDIE_TMP)
rasterOptions(tmpdir="/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
rasterOptions(maxmemory = 5e+20) # 6e+10 ~51GB allowed
rasterOptions(todisk = TRUE)

terraOptions(tempdir = "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/AUTemp")
terraOptions(memfrac=0.5)
terraOptions(todisk=TRUE)

# root folder
root <- "."

country <- "Nigeria"

# paths
spatialDir <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData")
Results_dir <- paste0(root, "/src/3Balance-estimates/", country, "/Results")
plotsDir <- paste0(root, "/src/3Balance-estimates/", country, "/Plots")

# nbsData <- readxl::read_xlsx(paste0(root, "/src/1Data-download/SpatialData/inputs/GLW4/NBS_data.xlsx"), sheet = "Sheet1") %>% 
#   dplyr::select(NAME_1, CTL, GTS, SHP) %>% 
#   mutate(nbs_TLU = CTL*1+GTS*0.08+SHP*0.1) %>% 
#   mutate_if(is.numeric, round, digits = 0)

# Loading Nigeria admin boundaries
aoi2 <- read_sf(paste0(spatialDir, "/inputs/aoi2.shp"))
aoi1 <- read_sf(paste0(spatialDir, "/inputs/aoi1.shp")) %>% tidyterra::select(NAME_1)
aoi0 <- read_sf(paste0(spatialDir, "/inputs/aoi0.shp"))

if (st_is_longlat(aoi1)) {
  aoi1 <- st_transform(aoi1, crs = 3857)  # Web Mercator
}

aoi1$area_m2 <- st_area(aoi1) # Calculate area and add it as a new column
aoi1$area_km2 <- as.numeric(aoi1$area_m2) / 1e6 # area in square kilometers:

aoi1 <- st_transform(aoi1, crs = 4326) # transform aoi1 to latlong again

# plot DM supply and demand
dmDemandSupply <- read_csv(paste0(Results_dir, "/stateDMStats.csv"))

lvPopulation <- read_csv(paste0(Results_dir, "/TLU.csv"))

demandSupplyStats <- aoi1 %>% 
  left_join(lvPopulation, by="NAME_1") %>% 
  left_join(dmDemandSupply, by = "NAME_1") 

# %>% 
#   left_join(nbsData %>% dplyr::select(NAME_1, nbs_TLU), by = "NAME_1")

demandSupplyStats <- demandSupplyStats %>%
  mutate(DMReq_tons = DMReq/1000,
         DMReq_class = case_when(
           DMReq_tons <= 80000 ~ "<80,000",
           DMReq_tons > 80000 & DMReq_tons <= 500000 ~ "80,000-500,000",
           DMReq_tons > 500000 & DMReq_tons <= 1000000 ~ "500,000-1,000,000",
           DMReq_tons > 1000000 &  DMReq_tons <= 5000000 ~ "1,000,000-5,000,000",
           DMReq_tons > 5000000 & DMReq_tons <= 10000000 ~ "5,000,000-10,000,000",
           DMReq_tons > 10000000 & DMReq_tons <= 15000000 ~ "10,000,000-15,000,000",
           DMReq_tons > 15000000 ~ ">15,000,000",
           TRUE ~ "NA"),
         DMReq_class = factor(
           DMReq_class,levels = c("<80,000", "80,000-500,000", "500,000-1,000,000", "1,000,000-5,000,000", "5,000,000-10,000,000", "10,000,000-15,000,000", ">15,000,000")),
         DMSupply_tons = DMSupply/1000,
         DMSupply_class = case_when(
           DMSupply_tons <= 500000 ~ "<500,000",
           DMSupply_tons > 500000 & DMSupply_tons <= 1000000 ~ "500,000-1,000,000",
           DMSupply_tons > 1000000 &  DMSupply_tons <= 5000000 ~ "1,000,000-5,000,000",
           DMSupply_tons > 5000000 & DMSupply_tons <= 10000000 ~ "5,000,000-10,000,000",
           DMSupply_tons > 10000000 & DMSupply_tons <= 15000000 ~ "10,000,000-15,000,000",
           DMSupply_tons > 15000000 ~ ">15,000,000",
           TRUE ~ "NA"),
         DMSupply_class = factor(
           DMSupply_class,levels = c("<500,000", "500,000-1,000,000", "1,000,000-5,000,000", "5,000,000-10,000,000", "10,000,000-15,000,000", ">15,000,000")),
         DMadequacy = if_else(adeq_2023 < 1, adeq_2023-1, if_else(adeq_2023 > 1, adeq_2023-1, NA)),
         DMadequacy_class = case_when(
           DMadequacy <=-0.50 ~ "<-50%",
           DMadequacy > -0.50 & DMadequacy <= -0.25 ~ "-50%--25%",
           DMadequacy > -0.25 & DMadequacy <= 0 ~ "-25%-0%",
           DMadequacy > 0 & DMadequacy <= 0.25 ~ "0%-25%",
           DMadequacy > 0.25 & DMadequacy <= 0.50 ~ "25%-50%",
           DMadequacy > 0.50 & DMadequacy <= 1 ~ "50%-100%",
           DMadequacy > 1  ~ ">100%",
           TRUE ~ "NA"),
         DMadequacy_class = factor(
           DMadequacy_class,levels = c("<-50%", "-50%--25%", "-25%-0%", "0%-25%", "25%-50%", "50%-100%", ">100%")),
         TLU_class = case_when(
           TLU <= 100000 ~ "100,000",
           TLU > 100000 & TLU <= 250000 ~ "100,000-250,000",
           TLU > 250000 & TLU <= 500000 ~ "250,000-500,000",
           TLU > 500000 & TLU <= 1000000 ~ "500,000-1,000,000",
           TLU > 1000000 & TLU <= 2000000 ~ "1,000,000-2,000,000",
           TLU > 2000000 & TLU <= 3000000 ~ "2,000,000-3,000,000",
           TLU > 3000000  ~ ">3,000,000",
           TRUE ~ "NA"),
         TLU_class = ifelse(TLU==19434, "<100,000", ifelse(TLU==40604, "<100,000", TLU_class)),
         TLU_class = factor(
           TLU_class,levels = c("<100,000", "100,000-250,000", "250,000-500,000", "500,000-1,000,000", "1,000,000-2,000,000", "2,000,000-3,000,000", ">3,000,000")),
         # nbs_TLU_class = case_when(
         #   nbs_TLU <= 100000 ~ "100,000",
         #   nbs_TLU > 100000 & nbs_TLU <= 250000 ~ "100,000-250,000",
         #   nbs_TLU > 250000 & nbs_TLU <= 500000 ~ "250,000-500,000",
         #   nbs_TLU > 500000 & nbs_TLU <= 1000000 ~ "500,000-1,000,000",
         #   nbs_TLU > 1000000 & nbs_TLU <= 2000000 ~ "1,000,000-2,000,000",
         #   nbs_TLU > 2000000 & nbs_TLU <= 3000000 ~ "2,000,000-3,000,000",
         #   nbs_TLU > 3000000  ~ ">3,000,000",
         #   TRUE ~ "NA"),
         # nbs_TLU_class = ifelse(nbs_TLU==4952, "<100,000", ifelse(nbs_TLU==90553, "<100,000", nbs_TLU_class)),
         # nbs_TLU_class = factor(
         #   nbs_TLU_class,levels = c("<100,000", "100,000-250,000", "250,000-500,000", "500,000-1,000,000", "1,000,000-2,000,000", "2,000,000-3,000,000", ">3,000,000")),
         DMSupply_tha = DMSupply/area_km2/100000,
         DMSupply_tha_class = case_when(
           DMSupply_tha <= 1 ~ "<1",
           DMSupply_tha > 1 & DMSupply_tha <= 1.5 ~ "1-1.5",
           DMSupply_tha > 1.5 & DMSupply_tha <= 2 ~ "1.5-2",
           DMSupply_tha > 2 & DMSupply_tha <= 2.5 ~ "2-2.5",
           DMSupply_tha > 2.5 & DMSupply_tha <= 3 ~ "2.5-3",
           DMSupply_tha > 3  ~ ">3",
           TRUE ~ "NA"),
         DMSupply_tha_class = factor(
           DMSupply_tha_class,levels = c("<1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", ">3")))

# Create a separate dataframe with centroids + coordinates - for use in creating hallo effect for labels
centroids <- st_centroid(demandSupplyStats)
coords <- st_coordinates(centroids)
centroids_df <- cbind(centroids, coords)

# Plot Requirments
feedDemandPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = DMReq_class), color = "black") +
  scale_fill_manual(values = c("darkgreen","green", "lightgreen", "yellow", "orange", "red", "darkred")) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed demand (tonnes DM)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedDemandPlot.tiff"), feedDemandPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot Supply
feedSupplyPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = DMSupply_class), color = "black") +
  scale_fill_manual(values = c("darkred","red", "orange", "yellow", "green", "darkgreen")) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed supply (tonnes DM)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedSupplyPlot.tiff"), feedSupplyPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot Supply - tonnes DM/ha
feedSupplyThaPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = DMSupply_tha_class), color = "black") +
  scale_fill_manual(values = c("darkred","red", "orange", "yellow", "green", "darkgreen")) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed supply (tonnes DM/ha)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedSupplyThaPlot.tiff"), feedSupplyThaPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")


# Plot Deficit/ Surplus
feedAdequacyPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = DMadequacy_class), color = "black") +
  scale_fill_manual(values = c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen")) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed deficit/surplus (%)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedAdequacyPlot.tiff"), feedAdequacyPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot TLU
TLUPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = TLU_class), color = "black") +
  scale_fill_manual(values = c("darkgreen","green", "lightgreen", "yellow", "orange", "red", "darkred")) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Tropical Livestock Units")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/TLUPlot.tiff"), TLUPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot TLU - One color gradient
TLU_gradientPlot <- ggplot(demandSupplyStats) +
  geom_sf(aes(fill = TLU), color = "black") +
  scale_fill_gradientn(
    colors = c("#ffcccc", "#ff6666", "#e60000", "#b30000", "#660000"),
    name = "Tropical Livestock Units"
  ) +
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Tropical Livestock Units")+
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))+
  guides(fill = guide_colorbar(barheight = unit(5, "cm")))

ggsave(paste0(plotsDir, "/TLU_gradientPlot.tiff"), TLU_gradientPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# # Plot nbs TLU - One color gradient
# TLU_gradientPlot <- ggplot(demandSupplyStats) +
#   geom_sf(aes(fill = nbs_TLU), color = "black") +
#   scale_fill_gradientn(
#     colors = c("#ffcccc", "#ff6666", "#e60000", "#b30000", "#660000"),
#     name = "Tropical Livestock Units"
#   ) +
#   geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = NAME_1),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
#   labs(title = "", fill = "Tropical Livestock Units")+
#   theme_bw() +
#   theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
#         text = element_text(size = 12, family = "Arial"),
#         axis.title = element_blank(),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         strip.text = element_text(size = 12),
#         legend.title=element_text(size=12),
#         legend.text=element_text(size = 12))+
#   guides(fill = guide_colorbar(barheight = unit(5, "cm")))
# 
# ggsave(paste0(plotsDir, "/TLU_gradientPlot.tiff"), TLU_gradientPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

demandSupplyStats_CSV <- demandSupplyStats %>% 
  st_drop_geometry()

write.csv(demandSupplyStats_CSV, paste0(Results_dir, "/demandSupplyStats.csv"), row.names = FALSE)

########################################
# Plotting zones
# Loading Nigeria agro-ecological zones
zones <- read_sf(paste0(spatialDir, "/intermediate/zones.gpkg"))

dmDemandSupplyZones <- read_csv(paste0(Results_dir, "/zoneDMStats.csv")) %>% 
  rename(ECOZone=NAME) %>% 
  mutate(DMReq = DMReq / 1000000000, DMSupply = DMSupply / 1000000000) %>%  # make values million tonnes
  mutate(Balance = (adeq_2023-1)*100) %>% 
  mutate_if(is.numeric, round, 1)

dmDemandSupplyZones <- zones %>% 
  left_join(dmDemandSupplyZones, by="ECOZone")

# Create a separate dataframe with centroids + coordinates - for use in creating hallo effect for labels
dmDemandSupplyZones <- st_make_valid(dmDemandSupplyZones)
centroids <- st_centroid(dmDemandSupplyZones)
coords <- st_coordinates(centroids)
centroids_df <- cbind(centroids, coords)

# Plot Supply
feedSupplyZonesPlot <- ggplot(dmDemandSupplyZones) +
  geom_sf(aes(fill = DMSupply), color = "black") +
  scale_fill_gradientn(
    colors = c("#f7fcf5", "#c7e9c0", "#74c476", "#238b45", "#00441b"))+
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = ECOZone),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed supply (Mt DM)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedSupplyZonesPlot.tiff"), feedSupplyZonesPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot Demand
feedDemandZonesPlot <- ggplot(dmDemandSupplyZones) +
  geom_sf(aes(fill = DMReq), color = "black") +
  scale_fill_gradientn(
    colors = c("#fff5eb", "#fdd0a2", "#fdae6b", "#e6550d", "#7f2704"))+
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = ECOZone),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed demand (Mt DM)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedDemandZonesPlot.tiff"), feedDemandZonesPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")

# Plot deficit
feedBalanceZonesPlot <- ggplot(dmDemandSupplyZones) +
  geom_sf(aes(fill = Balance), color = "black") +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#216837", midpoint = 0) + 
  geom_shadowtext(data = centroids_df,aes(x = X, y = Y, label = ECOZone),size = 2.5,color = "black",bg.color = "white", bg.r = 0.15, family = "Arial", fontface = "bold") +
  labs(title = "", fill = "Feed deficit/surplus (%)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Arial", face = "bold"),
        text = element_text(size = 12, family = "Arial"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size = 12))

ggsave(paste0(plotsDir, "/feedBalanceZonesPlot.tiff"), feedBalanceZonesPlot, device = "tiff", dpi = 300, width = 200, height = 120, units = "mm")
