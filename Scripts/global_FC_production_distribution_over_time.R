
measures_selected = c("Area harvested", "Production", "Yield")

flex_crop_country_data = crop_data %>% 
  select(country, source_crop, measures, year, value) %>% 
  filter(source_crop %in% flex_crops,
         measures %in% measures_selected) %>% 
  group_by(country, year, measures, source_crop) %>% 
  mutate(total_value = ifelse(measures == "Yield", mean(value), sum(value))) %>% 
  distinct(country, source_crop, measures, year, total_value) %>% 
  ungroup()

lump_data = function(lump_data, threshold = 0.9){
  lump_data = to_be_lumped_data
  last_year = last(lump_data$year)
  plot_order = lump_data %>% 
    filter(year == last_year) %>% 
    arrange(desc(total_value)) %>% 
    mutate(rank = row_number(),
           cum_perc = cumsum(total_value) / sum(total_value))
  
  # Show all crops that together account for at least 90% of the volume, group the rest.
  # threshold = 0.8
  n_source = plot_order %>% filter(cum_perc < threshold) %>% count(n()) %>% pull(n)
  
  lumped_data <- lump_data %>% 
    mutate(country = as.character(country)) %>%
    mutate(plot_label = ifelse(country %in% plot_order$country[1:n_source], country, 'Other')) %>%
    mutate(plot_label = factor(plot_label, levels = c((plot_order$country[1:n_source]), 'Other'))) %>% 
    group_by(plot_label, year) %>%
    summarise(annual_individual_value = sum(total_value)) %>% 
    group_by(year) %>% 
    mutate(percentage = annual_individual_value / sum(annual_individual_value))
  
  return(lumped_data)
}

# global production breakdown plots
theme_set(theme_classic(base_size = 18))
lump_threshold = 0.9
# Soybean
to_be_lumped_data = flex_crop_country_data %>% 
  filter(measures == "Production") %>% 
  group_by(year, source_crop) %>% 
  mutate(total_annual_production = sum(total_value)) %>% 
  filter(source_crop == "Soybeans")

lumped_data = lump_data(to_be_lumped_data, threshold = lump_threshold)

lumped_data %>%
  ggplot(aes(x = year, y = percentage, fill = plot_label)) +
  geom_area() +
  labs(title = "Global soybean production breakdown (1961-2016)", subtitle = paste0("Threshold for lumping is set to ", lump_threshold*100, "%"),
       y = "Production share (of weight)", fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scaleFUN)

ggsave(paste0(image_path, "global_soybean_production_breakdown_share.png"))

# Oil palm
lump_threshold = 0.9
to_be_lumped_data = flex_crop_country_data %>% 
  filter(measures == "Production") %>% 
  group_by(year, source_crop) %>% 
  mutate(total_annual_production = sum(total_value)) %>% 
  filter(source_crop == "Oil palm fruit")

lumped_data = lump_data(to_be_lumped_data, threshold = lump_threshold)

lumped_data %>%
  ggplot(aes(x = year, y = percentage, fill = plot_label)) +
  geom_area() +
  labs(title = "Global oil palm production breakdown (1961-2016)", subtitle = paste0("Threshold for lumping is set to ", lump_threshold*100, "%"),
       y = "Production share (of weight)", fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scaleFUN)

ggsave(paste0(image_path, "global_oilpalm_production_breakdown_share.png"))

# Maize
lump_threshold = 0.7
to_be_lumped_data = flex_crop_country_data %>% 
  filter(measures == "Production") %>% 
  group_by(year, source_crop) %>% 
  mutate(total_annual_production = sum(total_value)) %>% 
  filter(source_crop == "Maize")

lumped_data = lump_data(to_be_lumped_data, threshold = lump_threshold)

lumped_data %>%
  ggplot(aes(x = year, y = percentage, fill = plot_label)) +
  geom_area() +
  labs(title = "Global maize production breakdown (1961-2016)", subtitle = paste0("Threshold for lumping is set to ", lump_threshold*100, "%"),
       y = "Production share (of weight)", fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scaleFUN)

ggsave(paste0(image_path, "global_maize_production_breakdown_share.png"))


# Sugarcane
lump_threshold = 0.8
to_be_lumped_data = flex_crop_country_data %>% 
  filter(measures == "Production") %>% 
  group_by(year, source_crop) %>% 
  mutate(total_annual_production = sum(total_value)) %>% 
  filter(source_crop == "Sugar cane")

lumped_data = lump_data(to_be_lumped_data, threshold = lump_threshold)

lumped_data %>%
  ggplot(aes(x = year, y = percentage, fill = plot_label)) +
  geom_area() +
  labs(title = "Global sugar cane production breakdown (1961-2016)", subtitle = paste0("Threshold for lumping is set to ", lump_threshold*100, "%"),
       y = "Production share (of weight)", fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scaleFUN)

ggsave(paste0(image_path, "global_sugarcane_production_breakdown_share.png"))

## bla

crop_data %>% 
  filter(crop_category == "Flex crops",
         measures == "Production",
         year == 2016) %>% 
  group_by(country) %>% 
  summarise(flex_crop_production = sum(value)/1000) %>% 
  arrange(desc(flex_crop_production)) %>% 
  mutate(cumulative_percentage = cumsum(flex_crop_production) / sum(flex_crop_production))

import_quantity_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(country) %>% 
  summarise(flex_crop_imports = sum(value)/1000) %>% 
  arrange(desc(flex_crop_imports)) %>% 
  mutate(cumulative_percentage = cumsum(flex_crop_imports) / sum(flex_crop_imports))

export_quantity_data %>% 
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(country) %>% 
  summarise(flex_crop_exports = sum(value)/1000) %>% 
  arrange(desc(flex_crop_exports)) %>% 
  mutate(cumulative_percentage = cumsum(flex_crop_exports) / sum(flex_crop_exports))

