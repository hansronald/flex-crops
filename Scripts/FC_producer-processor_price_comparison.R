crop_production_categories = as_tibble(fread("~/Google Drive/Skola/SRC/Thesis/Code/Data/Categories/FAO_item_categories.csv")) %>% 
  clean_names() %>% 
  dplyr::select(source_crop, crop_category, item_code)

# Read the data
prices = read_csv("/Users/robinlindstrom/Downloads/Prices_E_All_Data_(Normalized)/Prices_E_All_Data_(Normalized).csv") %>% 
  clean_names() %>% 
  rename(country = area) %>% 
  left_join(crop_production_categories, by = "item_code") %>% 
  filter(item %in% c("Maize","Oil palm fruit", "Soybeans", "Sugar cane", "Rice", "Rice, paddy", "Wheat"))
#  filter(crop_category == "Flex crops") %>% 
#  filter(!item %in% c("Oil, palm", "Palm kernels", "Popcorn"))

# Get the price time series for all flex crops
prices %>%
  filter(unit == "USD") %>% 
  group_by(year, item) %>% 
  summarise(global_value = sum(value)) %>% 
  ggplot(aes(x = year, y = global_value, color = item)) +
  geom_line() +
  labs(title = "Global producer prices for flex crops", y = "Price (US$/tonne)", color = "", x = "") +
  facet_wrap(~item, scales = "free_y")

ggsave(paste0(img_path, "FC_global_producer_prices.png"))


# Export value

export_value_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_value_data.csv"
export_value_data = read_csv(export_value_data_path)

export_value_data %>%
  filter(crop_category == "Flex crops") %>% 
  filter(year %in% 1991:2016) %>% 
  group_by(year, source_crop) %>% 
  summarise(global_export_value = sum(value)) %>% 
  ggplot(aes(x = year, y = global_export_value, color = source_crop)) +
  geom_line()


