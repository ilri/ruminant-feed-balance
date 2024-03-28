# Downloading feed quality data from SSA feed database
# Author: John Mutua
# ----------------------------------------------------------------------------------- #

#gc(reset = T); rm(list = ls()) 
#if (!require("pacman")) install.packages("pacman")
library(tidyverse)
library(rvest)


# Country code
# countries <- c("Ethiopia", "Kenya", "Tanzania", "Uganda", "Rwanda")
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
    
    feed_url <- html_session(paste0("https://feedsdatabase.ilri.org/search/", feed_item, "?title=", feed_title, "&field_scientific_name_value=&field_feed_type_tid=All&field_country_tid=", country, "&combine="), httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
    
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

#feedQuality <- bind_rows(feedQuality, data.frame(ME = 9.49, Feed_item = "Groundnut", codeSPAM = "grou", codeBasketGrouped = "leg"))

crop_classification <- read.csv('CropParams/Crop classification_feed basket.csv', stringsAsFactors = F)

feedQuality <- left_join(feedQuality, select(crop_classification, Feed_item = SSAfeedEthiopia, codeSPAM, codeBasket_grouped))

feedQuality$codeSPAM[feedQuality$Feed_item == "fodder beet"] <- "beetWhole"
feedQuality$codeSPAM[feedQuality$Feed_item == "Groundnut"] <- "grou"
  
# Write outputs
write.csv(feedQuality, "CropParams/feedQuality_SSAdb.csv")

plot(feedQuality$ME ~ feedQuality$IVDMD)
summary(lm(feedQuality$ME ~ feedQuality$IVDMD))

feedQuality <- group_by(feedQuality, Feed_item, codeSPAM)
feedQuality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())

feedQuality <- group_by(feedQuality, codeBasket_grouped)
ssa_cat_quality_sum <- summarise(feedQuality, ME_SD = sd(ME, na.rm = T), ME = mean(ME, na.rm = T), n = n())
