export_quantity_data = read_csv(paste0(base_path, "export_quantity_data.csv"))
import_quantity_data = read_csv(paste0(base_path, "import_quantity_data.csv"))

FC_national_exports_2016 = export_quantity_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  select(iso3_code, country, item, source_crop, value) %>% 
  group_by(iso3_code, country, source_crop) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(source_crop) %>% 
  arrange(source_crop, desc(value)) %>% 
  mutate(share = value / sum(value)) %>% 
  mutate(cumulative_share = cumsum(value) / sum(value))

FC_national_imports_2016 = import_quantity_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  select(iso3_code, country, item, source_crop, value) %>% 
  group_by(iso3_code, country, source_crop) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(source_crop) %>% 
  arrange(source_crop, desc(value)) %>% 
  mutate(share = value / sum(value)) %>% 
  mutate(cumulative_share = cumsum(value) / sum(value))

write_csv(FC_national_exports_2016, paste0(base_path, "FC_national_exports_2016.csv"))
write_csv(FC_national_imports_2016, paste0(base_path, "FC_national_imports_2016.csv"))
