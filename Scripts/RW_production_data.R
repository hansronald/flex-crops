
#test_path = "/Users/robinlindstrom/Downloads/Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv"
crop_production_data_raw_path = "/Users/robinlindstrom/Downloads/Production_Crops_E_All_Data_(Normalized)/Production_Crops_E_All_Data_(Normalized).csv"
crop_production_data_raw = as_tibble(fread(crop_production_data_raw_path)) %>% 
  clean_names() %>% 
  select(area_code, area, item_code, item, element, year, value)

# FAO data only has a country code, not the iso code. This is used to map with other data using iso3code
FAO_codes = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_codes_raw.csv")) %>% 
  clean_names() %>%
  filter(country_group_new == "Small region") %>% 
  select(country_group, country_code, iso3_code_new) %>% 
  rename(iso3_code = iso3_code_new)

crop_production_categories = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_item_categories.csv")) %>% 
  clean_names() %>% 
  #  filter(item_group == "Crops Primary") %>% 
  #  filter(category %in% c("Cereals", "Oilseeds", "Roots and tubers", "Sugars", "Vegetable oil", "Fruits")) %>% 
  dplyr::select(source_crop, crop_category, item_code, include, definition)

crop_production_data_processed = crop_production_data_raw %>% 
  rename("country" = "area", "country_code" = "area_code", "measures" = "element") %>% 
  filter(country != "China") %>% 
  mutate(country = replace(country, country == "United States of America", "USA")) %>%  
  mutate(country = replace(country, country == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>%  
  mutate(country = replace(country, country == "Syrian Arab Republic", "Syria")) %>% 
  mutate(country = replace(country, country == "Russian Federation", "Russia")) %>% 
  mutate(country = replace(country, country == "Republic of Moldova", "Moldova")) %>% 
  mutate(country = replace(country, country == "Republic of Korea", "South korea")) %>% 
  mutate(country = replace(country, country == "Iran (Islamic Republic of)", "Iran")) %>% 
  mutate(country = replace(country, country == "Lao People's Democratic Republic", "Laos")) %>% 
  mutate(country = replace(country, country == "Democratic Republic of the Congo", "DR Congo")) %>% 
  mutate(country = replace(country, country == "Democratic People's Republic of Korea", "North korea")) %>% 
  mutate(country = replace(country, country == "China, Taiwan Province of", "Taiwan")) %>% 
  mutate(country = replace(country, country == "China, mainland", "China")) %>% 
  mutate(country = replace(country, country == "Bolivia (Plurinational State of)", "Bolivia")) %>% 
  mutate(country = replace(country, country == "Bosnia and Herzegovina", "Bosnia")) %>%
  mutate(country = replace(country, country == "United Republic of Tanzania", "Tanzania")) %>%
  mutate(country = replace(country, country == "The former Yugoslav Republic of Macedonia", "North macedonia")) %>%
  mutate(country = replace(country, country == "C\xf4te d'Ivoire", "Cote d'Ivore")) %>%
  mutate(country = replace(country, country == "R\xe9union", "Reunion")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  left_join(FAO_codes) %>% 
  left_join(crop_production_categories, by = "item_code") %>% 
  filter(!is.na(iso3_code),
         !is.na(crop_category))


crop_production_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"
write_csv(crop_production_data_processed, crop_production_data_path)

# crop_production_data %>% 
#   filter(source_crop %in% flex_crops,
#          measures %in% measures_selected) %>% 
#   group_by(year, measures, source_crop) %>% 
#   summarise(total_value = sum(value)) %>% 
#   ggplot(aes(x = year, y = total_value, color = source_crop)) +
#   geom_line() +
#   facet_wrap(~source_crop + measures, scales = "free_y", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
#   labs(title = "Total area harvested (ha) and yield (t/ha) for flex crops (1961-2018)", x = "", color = "", y = "")

    