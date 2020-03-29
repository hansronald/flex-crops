install.packages("here")
library(here)
here("Google Drive", "Skola", "SRC", "")


here()crop_production_data_processed = read_csv(paste0(base_path, "crop_production_data_processed.csv")) %>% 
  filter(item %in% c("Soybeans", "Oil palm fruit", "Sugar cane", "Maize"))

FC_national_production_data_2016 = crop_production_data_processed %>% 
  filter(crop_category == "Flex crops",
         year == 2018,
         measures == "Production") %>% 
  select(iso3_code, country, item, value) %>% 
  group_by(item) %>% 
  arrange(item, desc(value)) %>% 
  mutate(share = value / sum(value)) %>% 
  mutate(cumulative_share = cumsum(value) / sum(value))

write_csv(FC_national_production_data_2016, paste0(base_path, "FC_national_production_data_2016.csv"))

