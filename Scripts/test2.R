import_materials_SB = import_quantity_data %>% 
  filter(crop_category == "Flex crops") %>% 
  filter(source_crop == "Soybeans") %>%
  filter(year == 2016) %>% 
  group_by(country, material) %>% 
  summarise(material_sum = sum(value)) %>% 
  spread(material, material_sum) %>% 
  clean_names() %>% 
  mutate_at(c("raw", "processed"), ~coalesce(., 0))

export_materials_SB = export_quantity_data %>% 
  filter(crop_category == "Flex crops") %>% 
  filter(source_crop == "Soybeans") %>%
  filter(year == 2016) %>% 
  group_by(country, material) %>% 
  summarise(material_sum = sum(value)) %>% 
  spread(material, material_sum) %>% 
  clean_names() %>% 
  mutate_at(c("raw", "processed"), ~coalesce(., 0)) %>% 
  rename(raw_exports = raw, processed_imports = processed)
