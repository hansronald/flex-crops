



# What is it in other crops that has such a great value?
# Check which crop had the most value globally in 2016
other_crop_hectare_value = crop_data %>% 
  filter(crop_category == "Other crops",
         year == 2016) %>% 
  group_by(item) %>% 
  summarise(crop_value = sum(value),
            crop_production = sum(production)) %>% 
  mutate(crop_hectare_value = crop_value / crop_production) %>% 
  arrange(desc(crop_hectare_value))


other_crop_hectare_value %>% 
  ggplot(aes(x = crop_production, y = crop_hectare_value)) +
  geom_point()


# What is it in flex crops that has such a great value?
# Check which crop had the most value globally in 2016
flex_crop_hectare_value = crop_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(item) %>% 
  summarise(crop_value = sum(value),
            crop_production = sum(production)) %>% 
  mutate(crop_hectare_value = crop_value / crop_production) %>% 
  arrange(desc(crop_hectare_value))
