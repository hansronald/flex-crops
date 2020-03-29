library(tidyverse)
library(janitor)
library(ggplot2)
library(skimr)

farm_size_data_path = "/Users/robinlindstrom/Downloads/data/FarmSize_data_fullyProcessed.csv"
farm_size_data = read_csv(farm_size_data_path) %>% 
  clean_names()

flex_crop_farm_size = farm_size_data %>% 
  filter(crop %in% c("Soybeans", "Sugar cane", "Oil, palm fruit", "Maize")) %>% 
  select(c("crop", "name_0", "es1", "fs_class_min", "fs_class_max", "year", "crop_area", "cultivated_area", "harvested_area", "planted_area", "production", "perc_feed", "perc_food", "perc_seed", "perc_waste", "perc_processing", "perc_other", "kcal")) %>% 
  mutate(fs_class = case_when(fs_class_max <= 10 ~ "Small",
                              fs_class_max <= 100 ~ "Medium",
                              TRUE ~ "Large")) %>% 
  rename(country = name_0, iso3_code = es1, item = crop)

# Number of farms per crop class
flex_crop_farm_size %>% 
  filter(!is.na(fs_class_max)) %>% 
  count(crop, fs_class)

flex_crop_farm_size %>% 
  filter(!is.na(cultivated_area)) %>% 
  group_by(item) %>% 
  summarise(avg_cultivated_area = mean(cultivated_area))

flex_crop_farm_size %>%
  filter(!is.na(harvested_area)) %>% 
  group_by(item, iso3_code) %>% 
  summarise(avg_harvested_area = mean(harvested_area)) %>% 
  View()

flex_crop_farm_size %>% 
  filter(!is.na(harvested_area)) %>% 
  ggplot(aes(x = harvested_area, y = perc_food)) +
  geom_point()

flex_crop_farm_size

flex_crop_farm_size %>% 
  count(country) %>% 
  View()

flex_crop_farm_size %>% 
  count(year)

flex_crop_farm_size %>% 
  count(crop)

# Get a glimpse of the data
flex_crop_farm_size %>% skim()
# It seems like the harvested area is the most complete 

flex_crop_farm_size %>% 
  count(crop, fs_class) %>% 
  View()

