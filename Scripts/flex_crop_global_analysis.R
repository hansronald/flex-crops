### Source ----
source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")
# source("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/read_production_data.R")
# -----------------

## Data explore ----
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# rm(list = ls(all.names = TRUE)) 
theme_set(theme_light(base_size = 12))

crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"
crop_data = as_tibble(fread(crop_data_path))

## 
measures_selected = c("Area harvested", "Yield")
flex_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize")

land_area_2016 = read_csv("/Users/robinlindstrom/Downloads/Inputs_LandUse_E_All_Data_(Normalized)/Inputs_LandUse_E_All_Data_(Normalized).csv") %>% 
  clean_names() %>% 
  filter(item == "Country area",
         year == 2016) %>% 
  left_join(get_fao_codes() %>% select(country_code, iso3_code), by = c("area_code" = "country_code")) %>%
  mutate(value = value*1000) %>% 
  select(iso3_code, value) %>% 
  rename(land_area = value)

## Plot the global area harvested and yield for flex crops ---

flex_crop_global_data = crop_data %>% 
  select(country, source_crop, measures, year, value) %>% 
  filter(source_crop %in% flex_crops,
         measures %in% measures_selected) %>% 
  group_by(year, measures, source_crop) %>% 
  mutate(total_value = ifelse(measures == "Yield", mean(value), sum(value))) %>% 
  distinct(source_crop, measures, year, total_value)

#crop_data %>% 
#  filter(year == 1961,
#         source_crop == "Maize",
#         measures == "Yield") %>% 
#  summarise(mean = mean(value))

flex_crop_global_data %>% 
  ggplot(aes(x = year, y = total_value, color = source_crop)) +
  geom_line() +
  facet_wrap(~source_crop + measures, scales = "free_y", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_y_continuous(labels = scaleFUN) +
  labs(title = "Total area harvested (ha) and yield (t/ha) for flex crops (1961-2018)", x = "", color = "", y = "")

ggsave(file = paste0(image_path, "global_flex_harvest_yield.png"))

## hrj ----
                      
crop_category_area_harvested = crop_data %>% 
  filter(measures == "Area harvested") %>% 
  group_by(crop_category, year) %>% 
  summarise(total_harvest = sum(value))

#  ggplot(aes(x = year, y = total_harvest, color = crop_category)) +
#  geom_line() +
#  labs(title = "Total area harvested of different crop categories", x = "",
#       y = "Area harvest", color = "Crop category")

crop_category_average_yield = crop_data %>% 
  filter(measures == "Yield") %>% 
  group_by(crop_category, year) %>% 
  summarise(average_yield = mean(value))

#  ggplot(aes(x = year, y = average_yield, color = crop_category)) +
#  geom_line() +
#  labs(title = "Average yield of different crop categories", x = "", y = "Average yield", color = "Crop category")


crop_category_global_data = crop_category_area_harvested %>% 
    left_join(crop_category_average_yield, by = c("crop_category", "year")) %>% 
  gather(total_harvest, average_yield, key = "measures", value = "value")

n = 5
cols = gg_color_hue(n)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

crop_category_global_data %>% 
  mutate(measures = ifelse(measures == "total_harvest", "Total harvest", "Average yield"),
         measures = as.factor(measures),
         measures = fct_relevel(measures, c("Total harvest", "Average yield"))) %>% 
  ggplot(aes(x = year, y = value, color = crop_category)) +
  geom_line() +
  facet_wrap(~measures, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scaleFUN) +
  labs(title = "Total area harvested and average yield of different crop categories",
       x = "", y = "Yield (tonnes/ha) and area harvested (ha)", color = "") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(legend.position = "bottom") 

ggsave(paste0(image_path, "crop_category_harvest_and_yield.png"), height = 4)

## different solution

theme_set(theme_classic(base_size = 12))

g1 = crop_category_area_harvested = crop_data %>% 
  filter(measures == "Area harvested") %>% 
  group_by(crop_category, year) %>% 
  summarise(total_harvest = sum(value)) %>% 
  ggplot(aes(x = year, y = total_harvest, color = crop_category)) +
  geom_line() +
  labs(title = "Total area harvested of different crop categories", x = "",
       y = "Area harvest", color = "Crop category")

g2 = crop_category_average_yield = crop_data %>% 
  filter(measures == "Yield") %>% 
  group_by(crop_category, year) %>% 
  summarise(average_yield = mean(value)) %>% 
  ggplot(aes(x = year, y = average_yield, color = crop_category)) +
  geom_line() +
  labs(title = "Average yield of different crop categories", x = "", y = "Average yield", color = "Crop category")

ggarrange(g1, g2, common.legend = TRUE, legend = "right", ncol = 1)

ggarrange(g1, g2, common.legend = TRUE, legend = "right", ncol = 1) %>% 
  ggexport(filename = paste0(image_path, "crop_category_harvest_and_yield.png"))



## Where does the increase in sugar crop yield come from? 
crop_data %>% 
  filter(crop_category == "Other sugar crops") %>% 
  filter(measures == "Yield") %>% 
  group_by(year, item) %>% 
  summarise(average_yield = mean(value)) %>% 
  ggplot(aes(x = year, y = average_yield, color = item)) +
  geom_line()

# 

flex_crop_global_area_harvested = crop_data %>% 
  filter(measures == "Area harvested",
         crop_category == "Flex crops") %>% 
  group_by(year, item) %>% 
  summarise(total_area_harvested_per_crop = sum(value)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(annual_flex_crop_harvest = sum(total_area_harvested_per_crop),
         percentage = total_area_harvested_per_crop / annual_flex_crop_harvest)

g1 = flex_crop_global_area_harvested %>%   
  ggplot(aes(x = year, y = total_area_harvested_per_crop, color = item)) +
  geom_line() +
  labs(title = "Total global flex crop harvested area (1961-2018", x ="", y = "Area harvested (ha)")

g2 = flex_crop_global_area_harvested %>%   
  ggplot(aes(x = year, y = percentage, color = item)) +
  geom_line() +
  labs(title = "Global area harvested as a share (1961-2018)", x = "", y = "Percentage")

ggarrange(g1,g2, common.legend = TRUE, legend = "right", ncol = 1) %>% 
  ggexport(filename = paste0(image_path, "flex_individual_crop_category_harvest_and_yield.png"))

## Make individual country plots ----------------

category = "Flex crops"
min_land_area = 500000
min_proportion = 0.25
max_proportion = 1
stacked = TRUE

crop_data_proportion = crop_data %>% 
  filter(measures == "Area harvested") %>% 
  group_by(country, year) %>%
  mutate(crop_harvest_area = sum(value)) %>% 
  filter(crop_category == category) %>%
  
  # First calculate the total area for all crops group
  ungroup() %>% 
  group_by(country, year) %>%
  mutate(flex_crop_area_harvested = sum(value)) %>% 
  mutate(flex_crop_area_harvested_share = flex_crop_area_harvested / crop_harvest_area) %>% 
  ungroup() %>% 
  group_by(country, year, item) %>% 
  mutate(individual_flex_crop_harvest_area = sum(value)) %>% 
  summarise(individual_flex_crop_area_harvested_share = individual_flex_crop_harvest_area / crop_harvest_area,
            flex_crop_area_harvested_share = min(flex_crop_area_harvested_share),
            source_crop = unique(source_crop),
            iso3_code = unique(iso3_code)) %>% 
  ungroup() %>% 
  left_join(land_area_2016, by = "iso3_code")
  
  # Then calculate total area per individual crop
#  group_by(country, year, source_crop) %>%
#  mutate(crop_individual_area = sum(value)) %>%
#  mutate(crop_individual_area_proportion = (crop_individual_area) / crop_land_area) %>% 
#  select(country, year, crop_individual_area_proportion, crop_category_area_proportion, source_crop, crop_land_area, land_area)

# Get the countries which has the largest share of area harvested in 2017
last_year = last(crop_data_proportion$year)
top_countries = crop_data_proportion %>% 
  filter(year == last_year,
         land_area > 1000000,
         !country %in% c("Croatia", "Serbia", "Moldova", "Slovenia", "Bosnia")) %>% # TODO fix this later
  ungroup() %>% 
  group_by(country) %>%
  top_n(1, source_crop) %>%

#  filter(land_area > min_land_area) %>% 
  filter(flex_crop_area_harvested_share > min_proportion && flex_crop_area_harvested_share < max_proportion) %>%
  pull(country)

final_plot = crop_data_proportion %>% 
  filter(country %in% top_countries)

#new_order = national_trade %>% 
#  arrange(desc(trade_balance)) %>% 
#  select(country) %>% 
#  pull()

#final_plot2 = arrange(transform(final_plot, country = factor(country, levels = new_order)), country)
#iris2 <- arrange(transform(iris,
#                           Species=factor(Species,levels=new_order)),Species)
#theme_set(theme_light(base_size = 8))

theme_set(theme_classic(base_size = 9))

# Area plot of proportion of total national crop harvest
final_plot %>%
    ggplot(aes(x=year, y=individual_flex_crop_area_harvested_share, fill = str_wrap(source_crop, width = 15))) + 
    geom_area() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(title = "Flex crops area harvested coverage", fill = "", x = "", y = "proportion of total area harvested") +
    theme(axis.text.x = element_text(angle=60, hjust=1, size = 8)) +
    theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8), legend.position = "bottom") +
    #      theme_set(theme_light(base_size = 8)) +
    facet_wrap(~country) +
    scale_fill_brewer(palette = "Set2")

#theme(axis.text.x=element_text(color = "black", size=x_axis_tick_size, angle=x_axis_tick_angle, vjust=.8, hjust=0.8))

ggsave(file = paste0(image_path, "national_flex_crop_harvest_share_of_total_crop_harvest.png"))

# Line plot
# final_plot %>%
#     ggplot(aes(x=year, y=individual_flex_crop_area_harvested_share, color = str_wrap(source_crop, width = 8))) + 
#     geom_line() +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#     labs(color = "", x = "", y = "proportion of total area harvested") +
#     theme(axis.text.x = element_text(angle=60, hjust=1)) +
#     theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8)) +
#     facet_wrap(~country) +
#     scale_color_brewer(palette = "Set2")
