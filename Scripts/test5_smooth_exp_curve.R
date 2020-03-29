crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"
crop_data = read_csv(crop_data_path)
n = 5
crop_data %>% 
  filter(country == "Malaysia",
         item == "Oil palm fruit",
         measures == "Area harvested",
         year %in% 1961:2010) %>% 
  select(year, value) %>%
  group_by(group = gl(length(value)/n, n)) %>%
  summarise(mean_val = mean(value)) %>% 
  ungroup() %>% 
  mutate(group = as.double(group)) %>% 

  #mutate(change = value - lag(value)) %>% 
  #filter(!is.na(change)) %>% 
  ggplot(aes(x = group, y = mean_val)) +
  geom_line()
