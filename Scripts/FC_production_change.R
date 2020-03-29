library(tidyverse)
library(ggplot2)
library(janitor)

## Data explore ----
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# rm(list = ls(all.names = TRUE)) 
theme_set(theme_light(base_size = 12))

crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"
crop_data = read_csv(crop_data_path)

crop_data %>% 
  filter(crop_category == "Flex crops",
         measures == "Area harvested",
         year %in% c(2005, 2016)) %>%
  select(iso3_code, country, year, item, value) %>% 
  spread(year, value) %>% 
  clean_names() %>% 
  mutate(abs_change_harvestarea = x2016 - x2005,
         rel_change_harvestarea = x2016 / x2005) %>% 
#  filter(country == "Paraguay")

  filter(is.finite(rel_change_harvestarea)) %>% 
  group_by(item) %>% 
  arrange(item, desc(abs_change_harvestarea)) %>% 
  top_n(10, abs_change_harvestarea) %>% 
  View()
  #arrange(item, desc(rel_change_harvestarea)) %>% 
  #top_n(5, rel_change_harvestarea)


crop_data %>% 
    filter(country == "Paraguay",
         item == "Soybeans",
         measures == "Area harvested",
         year %in% c(2000,2016)) %>% 
  select(iso3_code, country, year, item, value) %>%
  spread(year, value) %>% 
  clean_names() %>% 
  mutate(change = x2016 - x2000)

