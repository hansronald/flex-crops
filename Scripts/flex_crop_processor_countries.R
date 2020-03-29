#source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")

population_path_name = "/Users/robinlindstrom/Downloads/Population_E_All_Data_(Normalized)/Population_E_All_Data_(Normalized).csv"

population_data = read_csv(population_path_name) %>% 
  clean_names() %>% 
  filter(element == "Total Population - Both sexes") %>% 
  mutate(area_code = as.integer(area_code)) %>% 
  rename("country_code" = "area_code") %>% 
  select(country_code, year, value) %>% 
  mutate(value = value * 1000) %>% 
  
  rename(population = value) %>% 
  filter(year %in% 1961:2016)

FAO_codes_nogroup = FAO_codes %>% 
  select(-country_group)

population_2016 = population_data %>% 
  left_join(FAO_codes_nogroup, by = c("country_code")) %>% 
  select(-country_code) %>% 
  filter(year == "2016") %>% 
  select(-year)

base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# rm(list = ls(all.names = TRUE))   
theme_set(theme_light(base_size = 12))

crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_data_processed.csv"

import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"

crop_production_data = as_tibble(fread(crop_data_path))
import_quantity_data = as_tibble(fread(import_quantity_data_path))
export_quantity_data = as_tibble(fread(export_quantity_data_path))

flex_crop_production_per_capita_2016 = read_csv(paste0(base_path, "flex_crop_production_per_capita_2016.csv"))

# Extract the flex crop data ---
flex_crop_production_data = crop_production_data %>% 
  filter(measures == "Production") %>% 
  filter(crop_category == "Flex crops")

flex_crop_import_quantity = import_quantity_data %>% 
  filter(crop_category == "Flex crops")

flex_crop_export_quantity = export_quantity_data %>% 
  filter(crop_category == "Flex crops")

# Check soybeans for year 2016----

soybean_production_2016 = flex_crop_production_data %>% 
  filter(source_crop == "Soybeans") %>% 
  filter(year == 2016) %>% 
  select(iso3_code, item, c(production = value))

soybean_import_2016 = flex_crop_import_quantity %>% 
  filter(source_crop == "Soybeans") %>% 
  filter(year == 2016) %>% 
  select(iso3_code, item, c(imports = value))

soybean_export_2016 = flex_crop_export_quantity %>% 
  filter(source_crop == "Soybeans") %>% 
  filter(year == 2016) %>% 
  select(iso3_code, item, c(exports = value))

soybean_data_2016 = soybean_import_2016 %>% 
  full_join(soybean_export_2016, by = c("iso3_code", "item")) %>% 
  full_join(soybean_production_2016, by = c("iso3_code", "item")) %>% 
  full_join(flex_crop_production_per_capita_2016, by = c("iso3_code", "item")) %>% 
  mutate_all(funs(replace_na(.,0)))

# How much is produced and traded in 2016 of all the soybean products?
soybean_data_2016 %>% 
  group_by(item) %>% 
  summarise(total_imports = sum(imports),
            total_exports = sum(exports),
            net_exports = total_exports - total_imports,
            total_production = sum(production),
            production_per_capita = min(production_per_capita))

# Soybean paste and soya sauce are such small quantities and is not necessary to look at in this case.

soybean_data_summarised = soybean_data_2016 %>% 
  mutate(material = ifelse(item == "Soybeans", "Raw", "Processed")) %>% 
  group_by(material, iso3_code) %>% 
  summarise(national_production = sum(production),
            national_imports = sum(imports),
            national_exports = sum(exports),
            national_net_exports = national_exports - national_imports,
            production_per_capita = min(production_per_capita))

soybean_processed_data = soybean_data_summarised %>% 
  filter(material == "Processed") %>% 
  ungroup() %>% 
  select(iso3_code, processed_imports = national_imports, processed_exports = national_exports,
         processed_net_exports = national_net_exports)

soybean_raw_data = soybean_data_summarised %>% 
  filter(material == "Raw") %>% 
  ungroup() %>% 
  select(iso3_code, raw_production = national_production, raw_imports = national_imports,
         raw_exports = national_exports, raw_net_exports = national_net_exports,
         raw_production_per_capita = production_per_capita)

# 
soybean_processor_countries_data = soybean_processed_data %>% 
  full_join(soybean_raw_data, by = "iso3_code") %>% 
  mutate_all(funs(replace_na(.,0))) %>% 
  
  # Remove all countries which does not have either raw exports or processed exports (since they are not interesting in this analysis)
  filter(raw_exports != 0 & processed_exports != 0)

# I need some constant which can say something about the loss from raw material becoming processed material
# These constants need to be different between crops of course but maybe also depending on what material it can
# become. Not shure how to distribute that though.
soybean_raw_to_processed_loss = 0.8

soybean_processor_countries_data %>% 
  mutate(raw_available = round((raw_imports + raw_production - raw_exports))) %>% 
  mutate(processed_exports_from_raw = processed_exports - processed_imports + raw_production * soybean_raw_to_processed_loss)


## More data --- 

soybean_processor_countries_manipulated = soybean_processor_countries_data %>% 
  mutate(raw_available = round((raw_imports + raw_production - raw_exports))) %>% 
  mutate(processed_balance = processed_exports - processed_imports) %>%
  mutate(processed_ratio = processed_exports / processed_imports) %>% 
  mutate(raw_to_processed_ratio = raw_available / processed_balance) %>% 
  mutate(raw_producer_indicator = raw_production / raw_available) %>% 
  select(iso3_code, raw_production, raw_available, raw_producer_indicator, processed_balance, processed_ratio, raw_to_processed_ratio)


soybean_processor_countries_manipulated %>% 
  gather(c(raw_available, processed_balance, raw_to_processed_ratio), key = "variable", value = "value") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~variable)
  

ggplot(aes(x = processed_balance, raw_to_processed_ratio)) +
  geom_point() + 
  geom_text(aes(label = iso3_code))

  
## Look at each indivudla processed item (not including soybean paste and soya sauce)
  
soybean_individual_commodities_2016 = soybean_data_2016 %>% 
  filter(!item %in% c("Soya sauce", "Soya paste"))
  

soybean_individual_commodities_summarised = soybean_individual_commodities_2016 %>% 
  group_by(item, iso3_code) %>% 
  summarise(national_production = sum(production),
            national_imports = sum(imports),
            national_exports = sum(exports))

soybean_raw_summarised = soybean_individual_commodities_summarised %>% 
  filter(item == "Soybeans") %>% 
  ungroup() %>% 
  select(-item) %>% 
  rename(soybeans_production = national_production, soybeans_imports = national_imports, soybeans_exports = national_exports)

soybean_cake_summarised = soybean_individual_commodities_summarised %>% 
  filter(item == "Cake, soybeans") %>% 
  ungroup() %>% 
  select(-c(item, national_production)) %>% 
  rename(soybeans_cake_imports = national_imports, soybeans_cake_exports = national_exports)

soybean_oil_summarised = soybean_individual_commodities_summarised %>% 
  filter(item == "Oil, soybean") %>% 
  ungroup() %>% 
  select(-c(item, national_production)) %>% 
  rename(soybeans_oil_imports = national_imports, soybeans_oil_exports = national_exports)

soybean_commodities_summarised = soybean_raw_summarised %>% 
  full_join(soybean_cake_summarised, by = c("iso3_code")) %>% 
  full_join(soybean_oil_summarised, by = c("iso3_code")) %>% 
  mutate_all(funs(replace_na(.,0)))
  



