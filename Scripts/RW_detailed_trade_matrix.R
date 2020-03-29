library(tidyverse)  # data manipulation
library(data.table)
library(janitor)

## read file ---------
FAO_codes = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_codes_raw.csv")) %>% 
  clean_names() %>%
  filter(country_group_new == "Small region") %>% 
  select(country_code, iso3_code_new) %>% 
  rename(iso3_code = iso3_code_new)

base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"

# Import the normalised trade matrix from FAOSTAT 2020
detailed_trade_data_path = "/Users/robinlindstrom/Downloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized) (1)/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"
detailed_trade_data = as_tibble(fread(detailed_trade_data_path))

#flex_crop_item_codes = c(56, 57, 58, 59,60, 61, 63, 64, 66, 68, 156, 158, 163, 170,
#                         175, 236, 237, 238, 239, 240, 241, 254, 256, 257, 258, 259)

# Read the categories
crop_production_categories = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_item_categories.csv")) %>% 
  clean_names() %>% 
  dplyr::select(source_crop, crop_category, item_code)

# Join the trade matrix with FAO codes and rename some columns, extract only flex crops
detailed_FC_trade_data_processed = detailed_trade_data %>% 
  clean_names() %>% 
  left_join(crop_production_categories, by = "item_code") %>% 
  filter(crop_category == "Flex crops") %>% 
  left_join(FAO_codes %>% 
              rename(reporter_country_iso3 = iso3_code), by = c("reporter_country_code" = "country_code")) %>% 
  left_join(FAO_codes %>% 
              rename(partner_country_iso3 = iso3_code), by = c("partner_country_code" = "country_code")) %>% 
  select(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3, item, source_crop, crop_category, element, unit, year, value, flag)

detailed_flex_trade_quantity = detailed_FC_trade_data_processed %>% 
  filter(element %in% c("Import Quantity", "Export Quantity"))

detailed_flex_trade_value = detailed_FC_trade_data_processed %>% 
  filter(element %in% c("Import Value", "Export Value"))

write_csv(detailed_FC_trade_quantity, paste0(base_path, "detailed_FC_trade_quantity.csv"))
write_csv(detailed_FC_trade_value, paste0(base_path, "detailed_FC_trade_value.csv"))
