source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")

crop_production_data_processed_path = here("Output processed data", "crop_production_data_processed.csv")
crop_production_data_processed = read_csv(crop_production_data_processed_path) %>% 
  select(country, iso3_code, item, measures, year, value, crop_category)

selected_crops = c("Soybeans", "Maize", "Oil palm fruit", "Sugar cane", "Wheat", "Rice, paddy")

crop_production_data_average = crop_production_data_processed %>% 
  filter(year %in% 2015:2018) %>% 
  group_by(item, measures) %>% 
  summarise(value = mean(value))

crop_production_data_average

crop_production_summarised = crop_production_data_processed %>% 
  filter(year %in% 2015:2018) %>%
  group_by(year, item, measures) %>% 
  mutate(value = ifelse(measures == "Yield", mean(value), sum(value))) %>% 
  distinct(item, measures, value) %>% 
  ungroup() %>% 
  group_by(item, measures) %>% 
  summarise(value = mean(value)) %>% 
  group_by(measures) %>% 
  mutate(total_value = sum(value),
         share = value / total_value) %>% 
  filter(item %in% selected_crops) %>% 
  ungroup()

analysed_crops_summarised = crop_production_summarised %>% 
  filter(measures != "Yield") %>% 
  select(-total_value) %>% 
  
  # Workaround to spread both values and share into for area harvested and production
  gather(temp, value, c(value, share)) %>% 
  unite(temp1, measures, temp, sep = ".") %>% 
  spread(temp1, value) %>% 
  clean_names() %>% 
  select(item, area_harvested_value, area_harvested_share, production_value, production_share) %>% 
  left_join(crop_production_summarised %>% 
              filter(measures == "Yield") %>% 
              select(item, yield = value), by = "item")

analysed_crops_summarised_formatted = analysed_crops_summarised %>% 
  mutate(item = as.factor(item),
         item = fct_reorder(item, c("Soybeans", "Oil palm fruit", "Sugar cane", "Maize", "Rice", "Wheat"))) %>% 
  arrange(item) %>% 
  mutate(area_harvested_value = area_harvested_value / 1e6,
         production_value = production_value / 1e6,
         yield = yield / 10000) %>% 
  rename(Crop = item, `Area harvested` = area_harvested_value, `Area harvested share` = area_harvested_share,
         Production = production_value, `Production share` = production_share, Yield = yield)

write_csv(analysed_crops_summarised_formatted, here("Output data", "analysed_crops_summarised.csv"))

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



