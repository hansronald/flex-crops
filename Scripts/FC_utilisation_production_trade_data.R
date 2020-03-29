
FC_national_production_data_2016 = read_csv(paste0(base_path, "FC_national_production_data_2016.csv")) %>% 
  rename(production = value,
         production_share = share)

FC_utilisation = read_csv(paste0(base_path, "flex_crop_utilisation.csv")) %>% 
  rename(iso3_code = sagecode)

FC_national_exports_2016 = read_csv(paste0(base_path, "FC_national_exports_2016.csv")) %>% 
  select(iso3_code, source_crop, value, share) %>% 
  rename(exports = value,
         item = source_crop,
         export_share = share)

FC_national_imports_2016 = read_csv(paste0(base_path, "FC_national_imports_2016.csv")) %>% 
  select(iso3_code, source_crop, value, share) %>% 
  rename(imports = value,
         item = source_crop,
         import_share = share)


FC_utilisation_production_2018_top10 = FC_national_production_data_2016 %>% 
  left_join(FC_national_exports_2016, by = c("iso3_code", "item")) %>% 
  left_join(FC_national_imports_2016, by = c("iso3_code", "item")) %>% 
  left_join(FC_utilisation, by = c("iso3_code", "item")) %>% 
  group_by(item) %>% 
  arrange(item, desc(production)) %>% 
  top_n(10, production)
  
write_csv(FC_utilisation_production_2018_top10, paste0(base_path, "FC_utilisation_production_2018_top10.csv"))
