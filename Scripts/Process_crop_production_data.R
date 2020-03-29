source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")

# Get the crop prproduction data from the raw data
data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Data/Crop production/crop_production_data.csv"
crop_production_data_raw = read_production_data(data_path)

# Get the crop value data from the raw data
data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Data/Crop production/crop_value_data.csv"
crop_value_data_raw = read_production_data(data_path) %>% 
  filter(measures == "Gross Production Value (constant 2004-2006 million US$)")

# Get names of different crop groups
all_crops = unique(crop_production_data_raw$item)
#flex_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize", "Cassava")
#emerging_flex_crops = c("Rapeseed", "Sugar beet", "Sunflower seed", "Coconuts")
#other_crops = setdiff(flex_crops, all_crops)
#used_measure = c("Production", "Area harvested", "Yield")

#excluded_crop_categories = c("Nuts", "Mushrooms & berries", "Spices", "Fruits", "Vegetables")

#world = map_data("world") %>%
#  filter(region != "Antarctica")

year = 1961:2016

crop_production_data = get_crop_data(crop_production_data_raw, all_crops, measure = used_measure, year) %>% 
  rename(production = value)
#rm(crop_production_data_raw)

# Get teh value of the crop production
crop_value_data = get_crop_data(crop_value_data_raw, all_crops, measure = used_measure, year) %>%
  select(country, iso3_code, item, year, value) %>% 
  mutate(value = 1000*value)
#rm(crop_production_data_raw)

crop_data = crop_production_data %>% 
  inner_join(crop_value_data, by = c("country" = "country", "iso3_code" = "iso3_code",
                                     "item" = "item", "year" = "year"))

all_crop_categories = crop_data %>% distinct(crop_category) %>% pull()
#used_crop_categories = setdiff(all_crop_categories, excluded_crop_categories)

# crop_data = crop_data %>% filter(crop_category %in% used_crop_categories)

save_file_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_data.csv" 
write_csv(as.data.frame(crop_data), save_file_path)



