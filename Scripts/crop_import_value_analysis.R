### Read source and create processed data ----

source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")
# source("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/read_trade_data.R")

# Read the data-----------------
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
# rm(list = ls(all.names = TRUE)) 

#import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
import_value_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_value_data.csv"
#export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"
#export_value_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_value_data.csv"

#import_quantity_data = as_tibble(fread(import_quantity_data_path))
import_value_data = as_tibble(fread(import_value_data_path))

#export_quantity_data = as_tibble(fread(export_quantity_data_path))
#export_value_data = as_tibble(fread(export_value_data_path))

theme_set(theme_light(base_size = 12))


# Analysis ----
# What is the proportion of flex crops imported in each country and which flex crop is the most dominating

national_crop_group_import_value = import_value_data %>% 
  filter(year == 2016) %>% 
  select(country, iso3_code, item, source_crop, crop_category, value) %>% 
  group_by(country, iso3_code, source_crop, crop_category) %>%
  summarise(total_import_value = sum(value)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(national_crop_import_value = sum(total_import_value)) %>% 
  filter(source_crop != "") %>% 
  #ungroup() %>% 
  #group_by(country) %>% 
  mutate(top_crop_group_import = source_crop[total_import_value == max(total_import_value)]) %>% 
  ungroup() %>% 
  group_by(country, crop_category) %>% 
  mutate(national_crop_category_import_value = sum(total_import_value),
         national_crop_import_value_proportion = national_crop_category_import_value/national_crop_import_value)

national_crop_group_import_value_proportion = national_crop_group_import_value %>% 
  filter(crop_category == "Flex crops") %>% 
  ungroup() %>% 
  group_by(country) %>% 
  #filter(national_crop_category_import_value != 0) %>% 
  top_n(1,total_import_value)
#mutate(top_flex_crop_import = source[import_value == max(import_value)]) %>% 
#distinct(iso3_code, national_crop_import_value_proportion, top_flex_crop_import)



# Check how many countries that has a flex crop as the most dominating importing crop --------

countries = sort((unique(import_value_data$country)))
n_countries = length(countries)
years = sort(unique(import_value_data$year))
n_years = length(years)
n_countries_with_flex_crop_dominating_import = rep(0, n_years)
#nations_dominate_flex_list = data.frame(10, 2010, 2011)

#nations_dominate_flex_list <- as.data.frame(matrix(0, ncol = n_years, nrow = n_countries))
#colnames(nations_dominate_flex_list) = paste0("y", years)
#rownames(nations_dominate_flex_list) = countries

nations_dominate_flex_empty_list = as.data.frame(expand.grid(countries, years)) %>% 
  rename(country = Var1, year = Var2)

temp = data.frame()

for(i in 1:n_years){
  nations_dominate_flex = import_value_data %>% 
    filter(source_crop != "") %>% 
    filter(year == years[i]) %>% 
    group_by(country, year) %>%
    mutate(yearly_national_import = sum(value)) %>% 
    ungroup() %>%
    group_by(country, year, source_crop) %>%
    summarise(source_crop_import_share = sum(value) / min(yearly_national_import),
              crop_category = min(crop_category)) %>% 
    group_by(year, country) %>%
    top_n(1, source_crop_import_share) %>% 
    ungroup() %>% 
    select(country, year, source_crop_import_share, crop_category)
  
  temp = rbind(temp, nations_dominate_flex %>% 
                 select(-crop_category))
  
  #merge(nations_dominate_flex_list, nations_dominate_flex, by = c("country", "year"))
  
  current_value = nations_dominate_flex %>%
    filter(crop_category == "Flex crops") %>% 
    select(-crop_category) %>% 
    summarise(n_countries_with_flex_crop_dominating_import = n()) %>% 
    pull()
  
  n_countries_with_flex_crop_dominating_import[i] = current_value
}

nations_dominate_flex_list = nations_dominate_flex_empty_list %>% 
  left_join(temp, by = c("country" = "country", "year" = "year"))

flex_crop_import_share = data.frame(year = 1961:2016, n_countries_with_flex_crop_dominating_import = n_countries_with_flex_crop_dominating_import)

flex_crop_import_share %>% 
  ggplot(aes(year, n_countries_with_flex_crop_dominating_import)) +
  geom_line() +
  labs(title = "Number of countries with flex crops as highest share of import value", x = "", y = "Number fo countries")

write_csv(flex_crop_import_share, paste0(base_path, "import_top_flex_crop.csv"))

## Import share 2016 -----

import_share_invididual_commodity = import_value_data %>% 
  filter(year == 2016) %>% 
  group_by(country) %>% 
  mutate(yearly_national_export = sum(value)) %>%
  ungroup() %>% 
  mutate(individual_crop_proportion_exports = value / yearly_national_export) %>% 
  group_by(year, iso3_code, country, source_crop) %>% 
  summarise(commodity_import = sum(value / yearly_national_export),
            crop_category = min(crop_category))

# Check the import share for each flex crop
individual_flex_crop_import_share = import_share_invididual_commodity %>% 
  filter(crop_category == "Flex crops") %>% 
  ungroup() %>% 
  select(-c(year, crop_category)) %>% 
  group_by(country) %>% 
  top_n(1,commodity_import)

# Spread the data for comparison
individual_flex_crop_import_share_spread =  individual_flex_crop_import_share %>% 
  spread(source_crop, commodity_import) %>% 
  clean_names()

total_flex_crop_import_share = individual_flex_crop_import_share %>% 
  group_by(iso3_code, country) %>% 
  summarise(flex_crop_import_share = sum(commodity_import)) %>%
  arrange(desc(flex_crop_import_share))

# Make histogram of the import share
total_flex_crop_import_share %>% 
  ggplot(aes(x = flex_crop_import_share)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of import share of flex crop", x = "import share of flex crop",
       y = "count")

# Make histogram of each individual flex crop import share
individual_flex_crop_import_share %>% 
  ggplot(aes(x = commodity_import)) +
  geom_histogram(bins = 30) +
  facet_wrap(~source_crop) +
  labs(title = "Distribution of import share of individual flex crop", x = "import share of flex crop",
       y = "count")

# Write a csv file of total flex crop import share
write_csv(total_flex_crop_import_share, paste0(base_path, "total_flex_crop_import_share.csv"))





