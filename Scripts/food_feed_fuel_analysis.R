library(tidyverse)
#library(dplyr)
library(data.table)
library(janitor)
library(ggplot2)
#library(stringr)

path_name = "~/Google Drive/Skola/SRC/Thesis/Data/CropAllocationFoodFeedFuel_Geotiff/CropUse_Kcal_Final_June2018.csv"
crop_util = read_csv(path_name) %>% 
  clean_names()

flex_crops = c("Soyabeans", "Sugar Cane", "Palm Oil", "Maize")

# Which crop is globally most used for other uses?
crop_util %>%
  #filter(item %in% flex_crops) %>% 
  group_by(item) %>% 
  summarise(global_supply = sum(domsupply),
            global_foodsupply = sum(foodsupply),
            global_feedsupply = sum(feedsupply),
            global_otherutil = sum(otherutil)) %>% 
  mutate(foodfraction = round(global_foodsupply / global_supply, 2),
         feedfraction = round(global_feedsupply / global_supply, 2),
         otherutilfraction = round(global_otherutil / global_supply, 2)) %>% 
  arrange(desc(otherutilfraction)) %>% 
  select(-c("global_supply", "global_foodsupply", "global_feedsupply", "global_otherutil")) %>% 
  View()


## Which crops has the highest share of other uses?
crop_util %>%
  #filter(item %in% flex_crops) %>% 
  group_by(item) %>% 
  summarise(global_supply = sum(domsupply),
            global_foodsupply = sum(foodsupply),
            global_feedsupply = sum(feedsupply),
            global_otherutil = sum(otherutil)) %>% 
  mutate(foodfraction = round(global_foodsupply / global_supply, 2),
         feedfraction = round(global_feedsupply / global_supply, 2),
         otherutilfraction = round(global_otherutil / global_supply, 2)) %>% 
  arrange(desc(global_otherutil)) %>%
  filter(item %in% c("Soyabeans", "Palm Oil", "Sugar Cane", "Maize")) %>% 
  View()
  select(item, global_supply, global_otherutil, otherutilfraction)
  select(-c("global_supply", "global_foodsupply", "global_feedsupply", "global_otherutil"))
  View()


  
## Which crops has the highest share of other uses?
flex_crop_utilisation = crop_util %>%
    #filter(item %in% flex_crops) %>% 
  filter(item %in% c("Soyabeans", "Palm Oil", "Sugar Cane", "Maize")) %>% 
  mutate(item = case_when(
    item == "Soyabeans" ~ "Soybeans",
    item == "Palm Oil" ~ "Oil palm fruit",
    item == "Sugar Cane" ~ "Sugar cane",
    item == "Maize" ~ "Maize"
  )) %>% 
  group_by(item) %>% 
  arrange(item, desc(domsupply))

write_csv(flex_crop_utilisation, paste0(base_path, "flex_crop_utilisation.csv"))

test_path = "/Users/robinlindstrom/Downloads/psd_alldata.csv"

test = as_tibble(fread(test_path)) %>% 
  clean_names()

commodities = test %>% 
  distinct(commodity_description)

commodities %>% View()

test %>% 
  filter(str_detect(commodity_description, "bean")) %>% 
  distinct(commodity_description)
  
## What is the global production of soybean meal?
global_soybean_meal_production = test %>% 
  filter(commodity_description == "Meal, Soybean") %>% 
  filter(attribute_description == "Production") %>% 
  group_by(market_year) %>% 
  summarise(global_soybean_meal = sum(value))


## What is the global production of soybean oil?
global_soybean_oil_production = test %>% 
  filter(commodity_description == "Oil, Soybean") %>% 
  filter(attribute_description == "Production") %>% 
  group_by(market_year) %>% 
  summarise(global_soybean_oil = sum(value))

## What is the global production of soybean oilseed?
global_soybean_oilseed_production = test %>% 
  filter(commodity_description == "Oilseed, Soybean") %>% 
  filter(attribute_description == "Production") %>% 
  group_by(market_year) %>% 
  summarise(global_soybean_oilseed = sum(value))

# Plot the soybean oilseed, oil and meal production
global_soybean_meal_production %>%
  left_join(global_soybean_oil_production, by = "market_year") %>% 
  left_join(global_soybean_oilseed_production, by = "market_year") %>% 
  gather("type", "production", -market_year) %>% 
  ggplot(aes(x = market_year, y = production, color = type)) +
  geom_line()


test %>% 
  filter(attribute_description == "Production") %>% 
  filter(str_detect(commodity_description, "Meal")) %>% 
  group_by(commodity_description, market_year) %>% 
  summarise(global_production = sum(value)) %>% 
  select(market_year, commodity_description, global_production) %>% 
  ggplot(aes(x = market_year, y = global_production, color = commodity_description)) +
  geom_line()



## Food balance data FAO
path_name = "/Users/robinlindstrom/Downloads/FoodBalanceSheetsHistoric_E_All_Data/FoodBalanceSheetsHistoric_E_All_Data_NOFLAG.csv"
feed_data = as_tibble(fread(path_name)) %>% 
  clean_names()


# Check first for feed
feed_data = feed_data %>% 
  filter(element == "Feed") %>% 
  select(-c("area_code", "item_code", "element_code", "element", "unit")) %>% 
  gather("year", "production", -c("area", "item")) %>% 
  mutate(year = as.numeric(gsub("y","", year)))

chosen_crops = c("Maize and products", "Wheat and products", "Cassava and products",
                 "Barley and products", "Vegetables", "Vegetables, Other",
                 "Potatoes and products", "Oilcrops", "Sweet potatoes",
                 "Rice (Milled Equivalent)", "Sugar cane", "Sorghum and products",
                 "Cereals, Other", "Soyabeans", "Oats")

test = feed_data %>% 
  group_by(year, item) %>%
  filter(!is.na(production)) %>% 
  summarise(global_feed_production = sum(production)) %>%
  filter(item %in% chosen_crops)

test %>% 
  ggplot(aes(x = year, y = global_feed_production, fill = item)) +
  geom_area()

feed_data %>% 
  distinct(item) %>% 
  View()

## Something else

path_name = "/Users/robinlindstrom/Downloads/FoodBalanceSheetsHistoric_E_All_Data/FoodBalanceSheetsHistoric_E_All_Data_NOFLAG.csv"
food_balance = as_tibble(fread(path_name)) %>% 
  clean_names()

chosen_crop = "Soyabeans"

food_balance_processed = food_balance %>% 
  select(-c("area_code", "item_code", "element_code", "unit")) %>% 
#  filter(area == "Brazil") %>% 
#  filter(item == chosen_crop) %>% 
  filter(element %in% c("Food", "Feed", "Other uses", "Processing", "Seed", "Losses")) %>% 
  gather("year", "production", -c("area", "item", "element")) %>% 
  mutate(year = as.numeric(gsub("y","", year)))

food_balance_processed %>% 
  filter(area == "Brazil") %>% 
  filter(item == chosen_crop) %>%
  ggplot(aes(x = year, y = production, fill = element)) +
  geom_area()


# How has oil palm developed in indonesia over time?
food_balance_processed %>% 
  filter(area == "Indonesia") %>% 
  filter(item %in% c("Palm Oil", "Palm kernels", "Palmkernel Oil")) %>%
  ggplot(aes(x = year, y = production, fill = element)) +
  geom_area() +
  facet_wrap(~item, scales = "free_y")

flex_crops = c("Palm Oil", "Palm kernels", "Palmkernel Oil", "Soyabeans",
               "Soyabean Oil", "Maize and products", "Sugar cane", "Sugar non-centrifugal")

# Plot all the flex crops and what their main uses are
food_balance_processed %>% 
  filter(item %in% flex_crops,
         !is.na(production)) %>% 
  group_by(year, item, element) %>% 
  summarise(global_production = sum(production)) %>% 
  ggplot(aes(x = year, y = global_production, fill = element)) +
  geom_area() +
  facet_wrap(~item, scales = "free_y")





