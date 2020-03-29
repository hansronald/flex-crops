
population_path_name = "/Users/robinlindstrom/Downloads/Population_E_All_Data_(Normalized)/Population_E_All_Data_(Normalized).csv"

population_data = read_csv(population_path_name) %>% 
  clean_names() %>% 
  filter(element == "Total Population - Both sexes") %>% 
  mutate(area_code = as.integer(area_code)) %>% 
  rename("country_code" = "area_code") %>% 
  select(country_code, year, value) %>% 
  rename(population = value) %>% 
  filter(year %in% 1961:2016)
  
FAO_codes_nogroup = FAO_codes %>% 
  select(-country_group)

population_2016 = population_data %>% 
  left_join(FAO_codes_nogroup, by = c("country_code")) %>% 
  select(-country_code) %>% 
  filter(year == "2016") %>% 
  select(-year)

crop_share = crop_production_data %>% 
  select(country, iso3_code, measures, year, value, item, crop_category) %>% 
  filter(measures == "Production",
         crop_category == "Flex crops",
         year == 2016) %>% 
  filter(!item %in% c("Oil, palm", "Palm kernels")) %>% 
  group_by(item) %>% 
  mutate(global_flex_crop_production = sum(value)) %>% 
  mutate(global_flex_crop_national_share = value / global_flex_crop_production) %>% 
  top_n(5, global_flex_crop_national_share)

crop_share %>% 
#  mutate(country = fct_reorder(country, global_flex_crop_national_share)) %>% 
  ggplot(aes(y = global_flex_crop_national_share, x = country)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~item, scales = "free_y") +
  labs(title = "Share of total global production of each flex crop", y = "share")

ggsave(paste0(image_path, "global_production_share.png"))  

# Not with itme separate

crop_share = crop_production_data %>% 
  select(country, iso3_code, measures, year, value, item, crop_category) %>% 
  filter(measures == "Production",
         crop_category == "Flex crops",
         year == 2016) %>% 
  filter(!item %in% c("Oil, palm", "Palm kernels")) %>% 
  mutate(global_flex_crop_production = sum(value)) %>% 
  mutate(global_flex_crop_national_share = value / global_flex_crop_production) %>% 
  top_n(5, global_flex_crop_national_share)

crop_share %>% 
  #  mutate(country = fct_reorder(country, global_flex_crop_national_share)) %>% 
  ggplot(aes(y = global_flex_crop_national_share, x = country)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~item, scales = "free_y") +
  labs(title = "Share of total global production of each flex crop", y = "share", x = "")

ggsave(paste0(image_path, "global_share_of_flexcrop_production_2016.png"))

# Crop share per capita
flex_crop_production_per_capita = crop_production_data %>% 
  filter(measures == "Production",
         crop_category == "Flex crops") %>% 
  select(country_code, country, country_group, iso3_code, measures, year, value, item, crop_category) %>% 
  left_join(population_data, by = c("country_code", "year")) %>% 
  mutate(yearly_production = sum(year)) %>% 
  mutate(production_per_capita = value / population) %>% 
  filter(!item %in% c("Oil, palm", "Palm kernels"))

# How has the national production in tonnes per capita changed over time?
flex_crop_production_per_capita %>%
  ggplot(aes(x = year, y = production_per_capita, group = country)) +
  geom_line() +
  facet_wrap(~item, scales = "free_y") +
  labs(title = "Production of different flex crops per capita in all countries", y = "production (tonnes / capita)", x = "")

ggsave(paste0(image_path, "national_tonnes_per_capita_timeseries.png"))

# Which nations have the highest production in tonnes per capita in 2016
flex_crop_production_per_capita %>%
  filter(year == 2016) %>% 
  group_by(item) %>%
  top_n(10, production_per_capita) %>% 
  ggplot(aes(y = production_per_capita, x = country)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~item, scales = "free") +
  labs(title = "National production of  of total global production of each flex crop", y = "tonnes per capita")

ggsave(paste0(image_path, "national_tonnes_per_capita_2016.png"))

flex_crop_production_per_capita_2016 = flex_crop_production_per_capita %>% 
  filter(year == 2016) %>% 
  select(iso3_code, country, item, production_per_capita)

write_csv(flex_crop_production_per_capita_2016, paste0(base_path, "flex_crop_production_per_capita_2016.csv"))

