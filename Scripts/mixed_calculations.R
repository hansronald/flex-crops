# Sugar beet and sugar cane ratio -----

base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_data_processed.csv"
crop_data = as_tibble(fread(crop_data_path))


beet_cane_ratio = crop_data %>% 
  filter(year == 2016,
         measures == "Production",
         item %in% c("Sugar cane", "Sugar beet")) %>% 
  select(iso3_code, country, item, value) %>% 
  spread(item, value) %>% 
  clean_names() %>% 
  mutate_all(funs(replace_na(.,0))) %>%
  mutate(beet_cane_ratio = round(sugar_beet / sugar_cane,2)) %>% 
  arrange(beet_cane_ratio) %>%
  mutate(sugar_source = ifelse(beet_cane_ratio < 1, "Sugar cane", "Sugar beet"))

write_csv(beet_cane_ratio, paste0(base_path, "beet_cane_ratio.csv"))

### 

import_value_data %>%
  filter(year == 2016) %>% 
  filter(group == "3 - SUGAR CROPS AND SWEETENERS AND DERIVED PRODUCTS") %>% 
  group_by(item) %>% 
  summarise(total_value = sum(value))

import_value_data %>% filter(item == "Beet Sugar Raw Centrifugal")

import_value_data %>% filter(source_crop == "Sugar cane") %>% distinct(item)
