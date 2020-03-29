library(tidyverse)

# Open the filtered file -------
detailed_trade_matrix_quantity = read_csv(paste0(base_path, "detailed_FC_trade_quantity.csv"))

# Spread the elements and values so that I have values for import quantity and export quantity
detailed_trade_matrix_quantity_spread = detailed_trade_matrix_quantity %>%
  filter(year == 2016) %>% 
  spread(element, value) %>% 
  clean_names() %>% 
  mutate_all(funs(replace_na(.,0)))

# Get the trade matrix (imports + exports) for each crop
maize_trade_matrix = detailed_trade_matrix_quantity_spread %>% 
  filter(source_crop == "Maize") %>% 
  mutate(trade = import_quantity + export_quantity) %>% 
  group_by(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3) %>% 
  summarise(total_trade = sum(trade)) %>% 
  ungroup() %>% 
  select(reporter_country_iso3, partner_country_iso3, total_trade)

oilpalm_trade_matrix = detailed_trade_matrix_quantity_spread %>% 
  filter(source_crop == "Oil palm fruit") %>% 
  mutate(trade = import_quantity + export_quantity) %>% 
  group_by(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3) %>% 
  summarise(total_trade = sum(trade)) %>% 
  ungroup() %>% 
  select(reporter_country_iso3, partner_country_iso3, total_trade)

soybean_trade_matrix = detailed_trade_matrix_quantity_spread %>% 
  filter(source_crop == "Soybeans") %>% 
  mutate(trade = import_quantity + export_quantity) %>% 
  group_by(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3) %>% 
  summarise(total_trade = sum(trade)) %>% 
  ungroup() %>% 
  select(reporter_country_iso3, partner_country_iso3, total_trade)

# Get the export and import matrix for each crop (TODO: soybean first add other crops later)

soybean_export_matrix = detailed_trade_matrix_quantity_spread %>% 
  filter(source_crop == "Soybeans") %>% 
  select(reporter_country_iso3, partner_country_iso3, export_quantity)

soybean_import_matrix = detailed_trade_matrix_quantity_spread %>% 
  filter(source_crop == "Soybeans") %>% 
  select(reporter_country_iso3, partner_country_iso3, import_quantity)

# TODO: Sugar cane is difficult, I don't know what to do with that for now.

#sugarcane_total_trade = detailed_trade_quantity_spread %>% 
#  filter(source_crop == "Sugar cane") %>% 
#  mutate(trade = import_quantity + export_quantity) %>% 
#  group_by(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3) %>% 
#  summarise(total_trade = sum(trade)) %>% 
#  ungroup() %>% 
#  select(reporter_country_iso3, partner_country_iso3, total_trade)

