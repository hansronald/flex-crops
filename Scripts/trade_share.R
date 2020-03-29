

nations_dominate_flex = export_value_data %>% 
  filter(iso3_code != "SDN") %>% 
  filter(source_crop != "") %>% 
  filter(year == 2016) %>% 
  group_by(country, year) %>%
  mutate(yearly_national_export = sum(value)) %>% 
  ungroup() %>%
  group_by(country, year, source_crop) %>%
  summarise(source_crop_export_share = sum(value) / min(yearly_national_export),
            crop_category = min(crop_category),
            source_crop == unique(source_crop)) %>% 
  group_by(year, country) %>%
  top_n(1, source_crop_export_share) %>% 
  ungroup() %>% 
  select(country, year, source_crop_export_share, crop_category)

flex_crop_export_share = export_value_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(country) %>% 
  mutate(total_flex_crop_exports = sum(value)) %>% 
  mutate(flex_crop_export_share = value / total_flex_crop_exports) %>% 
  ungroup() %>% 
  distinct(iso3_code, source_crop, total_flex_crop_exports, flex_crop_export_share) %>% 
  group_by(iso3_code) %>% 
  top_n(1, flex_crop_export_share)


flex_crop_import_share = import_value_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(country) %>% 
  mutate(total_flex_crop_imports = sum(value)) %>% 
  mutate(flex_crop_import_share = value / total_flex_crop_imports) %>% 
  ungroup() %>% 
  distinct(iso3_code, source_crop, total_flex_crop_imports, flex_crop_import_share) %>% 
  group_by(iso3_code) %>% 
  top_n(1, flex_crop_import_share)

write_csv(flex_crop_export_share, paste0(base_path, "flex_crop_export_share.csv"))
write_csv(flex_crop_import_share, paste0(base_path, "flex_crop_import_share.csv"))

