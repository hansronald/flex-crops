source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")

crop_production_data_processed_path = here("Processed data", "crop_production_data_processed.csv")
crop_production_data_processed = read_csv(crop_production_data_processed_path)

selected_crops = c("Soybeans", "Maize", "Oil palm fruit", "Sugar cane", "Wheat", "Rice, paddy")

flex_crops = crop_production_data_processed %>% 
  filter(crop_category == "Flex crops")

# Just flex crops
flex_crops %>% 
  filter(measures == "Yield") %>% 
  group_by(year, item) %>% 
  summarise(avg_yield = mean(value)) %>% 
  ggplot(aes(x = year, y = avg_yield, color = item)) +
  geom_line() +
  facet_wrap(~item, scales = "free_y")


# Other crops
selected_crops_data = crop_production_data_processed %>% 
  filter(item %in% selected_crops)

selected_crops_data %>% 
  filter(measures == "Yield") %>% 
  group_by(year, item) %>% 
  summarise(avg_yield = mean(value)) %>% 
  ggplot(aes(x = year, y = avg_yield, color = item)) +
  geom_line() +
  #facet_wrap(~item, scales = "free_y") +
  scale_y_continuous(label = scaleFUN)


# Global production (tonnes)
selected_crops_data %>% 
  filter(measures == "Production") %>% 
  group_by(year, item) %>% 
  summarise(global_production = sum(value)) %>% 
  ggplot(aes(x = year, y = global_production, color = item)) +
  geom_line() +
  facet_wrap(~item, scales = "free_y") +
  scale_y_continuous(label = scaleFUN) +
  labs(title = "Global production", y = "Production (tonnes)")

# Global area harvested
selected_crops_data %>% 
  filter(measures == "Area harvested") %>% 
  group_by(year, item) %>% 
  summarise(global_harvested = sum(value)) %>% 
  ggplot(aes(x = year, y = global_harvested, color = item)) +
  geom_line() +
  facet_wrap(~item, scales = "free_y") +
  scale_y_continuous(label = scaleFUN) +
  labs(title = "Global area harvested", y = "Area harvested (hectare)")


