# Prepare agro-ecological zones
# Author: Simon Fraval
# Last modified by John Mutua on 12/11/2024

# # Install required packages
# install.packages("stars")
# install.packages("dplyr")

# Load libraries
library(stars)
library(dplyr)

# root folder
root <- "."
outdirInt <- paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/intermediate"); dir.create(outdirInt, F, T)

country <- "Nigeria"

livelihood_zones <- st_read(paste0(root, "/src/3Balance-estimates/", country, "/SpatialData/inputs/Ecological_and_Feed_Distribution.shp"))

# Introducing codes
livelihood_zones <- livelihood_zones %>%
  mutate(Ecological = case_when(Ecological == "Fresh Water Swanmp Forest" ~ "Fresh Water Swamp Forest",
                                TRUE ~ Ecological),
         ECOZONECode = case_when(Ecological == "Sudan Savannah" ~ "SUSSAV",
                                 Ecological == "Sahel Savannah" ~ "SASSAV",
                                 Ecological == "Southern Guinea Savannah" ~ "SOGSAV",
                                 Ecological == "Lowland Rainfall" ~ "LORAIN",
                                 Ecological == "Fresh Water Swamp Forest" ~ "FRWASF",
                                 Ecological == "Northern Guinea Savannah" ~ "NOGSAV",
                                 Ecological == "Mountain Vegetations" ~ "MOUVEG",
                                 Ecological == "Mangrove" ~ "MANGGR",
                                 TRUE ~ Ecological
    )
  )

# Write outputs
liveOut <- st_rasterize(sf = livelihood_zones[, "ECOZONECode"], file = paste0(outdirInt, "/livelihoodZones.tif"))