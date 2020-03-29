library(tidyverse)
library(janitor)
library(data.table)

trade_data_raw_path = "/Users/robinlindstrom/Downloads/Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv"

trade_data_raw = as_tibble(fread(trade_data_raw_path)) %>% 
  clean_names() %>% 
  select(area_code, area, item_code, item, element, year, unit, value, flag) %>% 
  filter(flag != "A") # Removes all the aggregates

crop_trade_categories = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_item_categories.csv")) %>% 
  clean_names() %>%
  dplyr::select(source_crop, crop_category, flex_crop, item_code, material, group) 

# FAO data only has a country code, not the iso code. This is used to map with other data using iso3code
FAO_codes = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_codes_raw.csv")) %>% 
  clean_names() %>%
  filter(country_group_new == "Small region") %>% 
  select(country_group, country_code, iso3_code_new) %>% 
  rename(iso3_code = iso3_code_new)

# Get list of the ratio of national sugar beet production vs sugar cane production.
beet_cane_ratio = read_csv("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/beet_cane_ratio.csv") %>% 
  select(iso3_code, sugar_source)

# Get countries that that mostly produce sugar cane
iso3_sugar_cane = beet_cane_ratio %>%
  filter(sugar_source == "Sugar cane") %>% 
  select(iso3_code) %>%
  pull

# Get countries that that mostly produce sugar beet
iso3_sugar_beet = beet_cane_ratio %>%
  filter(sugar_source == "Sugar beet") %>% 
  select(iso3_code) %>%
  pull

# Read the production file. Rename countries with long names to shorter names.
trade_data_processed = trade_data_raw %>% 
  rename("country" = "area", "country_code" = "area_code", "measures" = "element") %>% 
  filter(country != "China") %>%
  
  # Aggregate the data from countries that changed name over the period
  mutate(country = replace(country, country == "Ethiopia PDR", "Ethiopia"), 
         country_code = replace(country_code, country_code == 62, 238)) %>% 
  
  # TODO remove I don't think this is needed anymore since the data is already gathered when normalised
  #group_by(country_code, item_code, item, measures, year, unit, flag) %>% 
  #summarise_all(sum) %>% 
  #ungroup() %>% 
  
  # Add ISO3 code and pre-defined item categories
  left_join(FAO_codes, by = "country_code") %>%
  left_join(crop_trade_categories, by = "item_code") %>% 
  
  # This removes all country groups that aren't countries and all items
  # which has a crop category in the name
  #filter(!is.na(iso3_code)) %>% 
  filter(!is.na(crop_category)) %>% 
  
  # Find the NAs and replace with 0
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  
  # Rename some countries to easier interpretble names
  mutate(country = replace(country, country == "United States of America", "USA")) %>%  
  mutate(country = replace(country, country == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>%  
  mutate(country = replace(country, country == "Syrian Arab Republic", "Syria")) %>% 
  mutate(country = replace(country, country == "Russian Federation", "Russia")) %>% 
  mutate(country = replace(country, country == "Republic of Moldova", "Moldova")) %>% 
  mutate(country = replace(country, country == "Republic of Korea", "South korea")) %>% 
  mutate(country = replace(country, country == "Iran (Islamic Republic of)", "Iran")) %>% 
  mutate(country = replace(country, country == "Lao People's Democratic Republic", "Laos")) %>% 
  mutate(country = replace(country, country == "Democratic Republic of the Congo", "DR Congo")) %>% 
  mutate(country = replace(country, country == "Democratic People's Republic of Korea", "North Korea")) %>% 
  mutate(country = replace(country, country == "China, Taiwan Province of", "Taiwan")) %>% 
  mutate(country = replace(country, country == "China, mainland", "China")) %>% 
  mutate(country = replace(country, country == "Bolivia (Plurinational State of)", "Bolivia")) %>% 
  mutate(country = replace(country, country == "Bosnia and Herzegovina", "Bosnia")) %>%
  mutate(country = replace(country, country == "United Republic of Tanzania", "Tanzania")) %>%
  mutate(country = replace(country, country == "The former Yugoslav Republic of Macedonia", "North Macedonia")) %>%
  mutate(country = replace(country, country == "C\xf4te d'Ivoire", "Cote d'Ivore")) %>%
  mutate(country = replace(country, country == "R\xe9union", "Reunion")) %>% 
  
  # Find all Sugar Raw Centrifugal in countries where sugar cane production is dominant and rename this to
  # Cane Sugar Raw Centrifugal and make it a flex crop as source
  mutate(item = ifelse(item == "Sugar Raw Centrifugal" & iso3_code %in% iso3_sugar_cane, "Cane Sugar Raw Centrifugal", item)) %>% 
  mutate(source_crop = ifelse(item == "Cane Sugar Raw Centrifugal", "Sugar cane", source_crop)) %>% 
  mutate(crop_category = ifelse(item == "Cane Sugar Raw Centrifugal", "Flex crops", crop_category)) %>% 
  
  # Find all Sugar Refined in countries where sugar cane production is dominant and rename this to
  # Cane Sugar Refined and make it a flex crop as source
  mutate(item = ifelse(item == "Sugar refined" & iso3_code %in% iso3_sugar_cane, "Cane Sugar refined", item)) %>% 
  mutate(source_crop = ifelse(item == "Cane Sugar refined", "Sugar cane", source_crop)) %>% 
  mutate(crop_category = ifelse(item == "Cane Sugar refined", "Flex crops", crop_category)) %>%
  
  # Same as above but with sugar beet
  mutate(item = ifelse(item == "Sugar Raw Centrifugal" & iso3_code %in% iso3_sugar_beet, "Beet Sugar Raw Centrifugal", item)) %>% 
  mutate(source_crop = ifelse(item == "Beet Sugar Raw Centrifugal", "Sugar beet", source_crop)) %>% 
  mutate(crop_category = ifelse(item == "Beet Sugar Raw Centrifugal", "Other sugar crops", crop_category)) %>% 
 
   # Same as above but with sugar beet  
  mutate(item = ifelse(item == "Sugar refined" & iso3_code %in% iso3_sugar_beet, "Beet Sugar refined", item)) %>% 
  mutate(source_crop = ifelse(item == "Beet Sugar refined", "Sugar beet", source_crop)) %>%
  mutate(crop_category = ifelse(item == "Beet Sugar refined", "Other sugar crops", crop_category))
  
#trade_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/trade_data_processed.csv"

import_quantity_data = trade_data_processed %>% 
  filter(measures == "Import Quantity")
import_value_data = trade_data_processed %>% 
  filter(measures == "Import Value")
export_quantity_data = trade_data_processed %>% 
  filter(measures == "Export Quantity")
export_value_data = trade_data_processed %>% 
  filter(measures == "Export Value")

import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
import_value_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_value_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"
export_value_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_value_data.csv"

write_csv(import_quantity_data, import_quantity_data_path)
write_csv(import_value_data, import_value_data_path)
write_csv(export_quantity_data, export_quantity_data_path)
write_csv(export_value_data, export_value_data_path)

