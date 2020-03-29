import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"

import_quantity_data = as_tibble(fread(import_quantity_data_path))
export_quantity_data = as_tibble(fread(export_quantity_data_path))

FC_import_quantity_2016 = import_quantity_data %>% 
  filter(year == 2016,
         crop_category == "Flex crops",
         material == "Raw") %>% 
  group_by(country, source_crop) %>% 
  summarise(national_crop_exports_2016 = sum(value))

FC_export_quantity_2016 = export_quantity_data %>% 
  filter(year == 2016,
         crop_category == "Flex crops",
         material == "Raw") %>% 
  group_by(country, source_crop) %>% 
  summarise(national_crop_exports_2016 = sum(value))


export_quantity_data %>%
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(source_crop, item, material) %>% 
  summarise(global_export_weight = sum(value)) %>%
  ungroup() %>% 
  mutate(material = ifelse(item == "Cane Sugar Raw Centrifugal", "Processed", material)) %>% 
  filter(!item %in% c("Sugar non-centrifugal", "Sugar refined")) %>% 
  arrange(source_crop, desc(material)) %>% 
  group_by(source_crop) %>% 
  mutate(total_crop_export_weight = sum(global_export_weight)) %>% 
  group_by(source_crop, material) %>% 
  mutate(material_weight = sum(global_export_weight)) %>% 
  mutate(material_share = round(material_weight/total_crop_export_weight,2 )) %>% 
  select(-c("total_crop_export_weight", "material_weight")) %>% 
  write_csv("test.csv")
  distinct(item)
  distinct(source_crop, material,)
