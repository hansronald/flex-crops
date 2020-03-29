

# General national crop production in 2016
national_crop_harvest_data_2016 = crop_production_data_processed %>% 
  filter(year == 2016,
         measures == "Area harvested") %>% 
  group_by(country) %>% 
  mutate(national_area_harvested = sum(value)) %>% 
  group_by(country, crop_category) %>% 
  mutate(crop_group_national_harvest = sum(value)) %>% 
  mutate(crop_group_harvest_share = crop_group_national_harvest / national_area_harvested) %>%
  distinct(country, iso3_code, crop_category, country_group, national_area_harvested,
           crop_group_national_harvest, crop_group_harvest_share)

national_crop_production_data_2016 = crop_production_data_processed %>% 
  filter(year == 2016,
         measures == "Production") %>% 
  group_by(country) %>% 
  mutate(national_production = sum(value)) %>% 
  group_by(country, crop_category) %>% 
  mutate(crop_group_national_production = sum(value)) %>% 
  mutate(crop_group_production_share = crop_group_national_production / national_production) %>%
  distinct(country, iso3_code, crop_category, country_group, national_production,
           crop_group_national_production, crop_group_production_share)


# Flex crop national production 2016
national_flex_crop_production_data_2016 = crop_production_data_processed %>% 
  filter(year == 2016,
         measures == "Production",
         crop_category == "Flex crops",
         item != "Oil, palm") %>% 
  group_by(country) %>% 
  mutate(national_FC_production = sum(value)) %>% 
  group_by(country, source_crop) %>% 
  summarise(FC_national_production = sum(value))
#  distinct(country, iso3_code, item, country_group, national_production,
#           crop_group_national_production, crop_group_production_share)
  
national_flex_crop_harvest_data_2016 = crop_production_data_processed %>% 
    filter(year == 2016,
           measures == "Area harvested",
           crop_category == "Flex crops") %>% 
    group_by(country) %>% 
    mutate(national_FC_production = sum(value)) %>% 
    group_by(country, source_crop, item) %>% 
    summarise(FC_national_production = sum(value))
#  distinct(country, iso3_code, item, country_group, national_production,
#           crop_group_national_production, crop_group_production_share)

write_csv(national_crop_harvest_data_2016, "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/Output/national_crop_harvest_data_2016.csv")
write_csv(national_crop_production_data_2016, "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/Output/national_crop_production_data_2016.csv")


