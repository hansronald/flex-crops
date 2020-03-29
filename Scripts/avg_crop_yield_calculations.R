library(tidyverse)
## Data explore ----
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

crop_production_data_processed = read_csv(paste0(base_path, "crop_production_data_processed.csv"))

selected_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize",
                   "Sugar beet", "Cassava", "Coconuts", "Sunflower seed", "Rapeseed", "Rice, paddy", "Wheat")

avg_yield_crops = crop_production_data_processed %>% 
  filter(measures == "Yield",
         year %in% 2010:2018,
         item %in% selected_crops) %>%
  mutate(value = value * 0.0001) %>% 
  group_by(item) %>% 
  summarise(avg_yield = mean(value)) %>%
  ungroup() %>% 
  mutate(item = factor(item, levels = selected_crops)) %>% 
  arrange(item)

write_csv(avg_yield_crops, paste0(base_path, "avg_yield_crops.csv"))
