source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")

crop_production_data_processed_path = here("Processed data", "crop_production_data_processed.csv")
crop_production_data_processed = read_csv(crop_production_data_processed_path)

selected_crops = c("Soybeans", "Maize", "Oil palm fruit", "Sugar cane", "Sugar beet", "Wheat", "Rice, paddy")

#flex_crops = crop_production_data_processed %>% 
#  filter(crop_category == "Flex crops")

# Other crops
selected_crops_data = crop_production_data_processed %>% 
  filter(item %in% selected_crops,
         year %in% c(2010, 2014),
         measures == "Area harvested")

# In thousands of hectare
selected_crops_data %>% 
  group_by(item, year) %>% 
  summarise(total_harvest = sum(value)) %>% 
  arrange(year)
  
  