# library
source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")
#library(ggplot2)

nutrient_input_average_path = here("Data", "Crop data", "nutrient_input_average.csv")
nutrient_input_average = read_csv(nutrient_input_average_path) %>% 
  gather("Nutrient", "value", -Crop) %>% 
  clean_names()

nutrient_input_global_path = here("Data", "Crop data", "nutrient_input_global.csv")
nutrient_input_global = read_csv(nutrient_input_global_path) %>% 
  gather("Nutrient", "value", -Crop) %>% 
  clean_names() %>% 
  mutate(value = value * 1e6)

# # Plot input in kg per hectare
# nutrient_input_global %>%
#   mutate(nutrient = as.factor(nutrient)) %>% 
#   mutate(nutrient = fct_relevel(nutrient, c("N", "P", "K", "NPK"))) %>% 
#   mutate(crop = as.factor(crop)) %>% 
#   mutate(crop = fct_relevel(crop, c("Maize", "Oil Palm", "Soybeans", "Sugar crops", "Rice", "Wheat"))) %>% 
#   group_by(crop) %>% 
#   ggplot(aes(fill=nutrient, y=value, x=crop)) + 
#   geom_bar(position="dodge", stat="identity") +
#   labs(title = "Global total nutrient application per crop", y = "tonnes",x = "", fill = "Nutrient") +
#   scale_y_continuous(labels = scaleFUN)
# 
# ggsave(here("Output images", "nutrient_input_global.png"))
# 
# # Plot input in kg per hectare
# nutrient_input_average %>%
#   mutate(nutrient = as.factor(nutrient)) %>% 
#   mutate(nutrient = fct_relevel(nutrient, c("N", "P", "K", "NPK"))) %>% 
#   mutate(crop = as.factor(crop)) %>% 
#   mutate(crop = fct_relevel(crop, c("Maize", "Oil Palm", "Soybeans", "Sugar crops", "Rice", "Wheat"))) %>% 
#   group_by(crop) %>% 
#   ggplot(aes(fill=nutrient, y=value, x=crop)) + 
#   geom_bar(position="dodge", stat="identity") +
#   labs(title = "Average nutrient application per crop", y = "Kilograms per hectare",x = "")
# 
# ggsave(here("Output images", "nutrient_input_average.png"))


# Plot input in kg per hectare

# Find crop plot order based on total NPK input
crop_order = nutrient_input_global %>% 
  filter(nutrient == "NPK") %>% 
  arrange(desc(value)) %>% 
  pull(crop)

nutrient_input_global %>%
  filter(nutrient != "NPK") %>% 
  mutate(nutrient = as.factor(nutrient)) %>% 
  mutate(nutrient = fct_relevel(nutrient, c("N", "P", "K"))) %>% 
  mutate(crop = as.factor(crop)) %>% 
  mutate(crop = fct_relevel(crop, rev(crop_order))) %>% 
  group_by(crop) %>% 
  ggplot(aes(fill=nutrient, y=value, x=crop)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Global total nutrient application per crop", y = "tonnes",x = "", fill = "Nutrient", tag = "a)") +
  coord_flip() +
  scale_y_continuous(labels = scaleFUN) +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette="Paired")

ggsave(here("Output images", "nutrient_input_global.png"), height = 2, width = 3, dpi = 200)

# Plot input in kg per hectare

# Find the order based on total nutrient input
crop_order = nutrient_input_average %>% 
  filter(nutrient == "NPK") %>% 
  arrange(desc(value)) %>% 
  pull(crop)

nutrient_input_average %>%
  filter(nutrient != "NPK") %>% 
  mutate(nutrient = as.factor(nutrient)) %>% 
  mutate(nutrient = fct_relevel(nutrient, c("N", "P", "K"))) %>% 
  mutate(crop = as.factor(crop)) %>% 
  mutate(crop = fct_relevel(crop, rev(crop_order))) %>% 
  group_by(crop) %>% 
  ggplot(aes(fill=nutrient, y=value, x=crop)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Average nutrient application per crop", y = "Kilograms per hectare",x = "", tag = "b)") +
  coord_flip() +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette="Paired")

ggsave(here("Output images", "nutrient_input_average.png"), height = 2, width = 3, dpi = 300)

global_fertiliser_per_category = read_csv(here("Data", "Input", "Global fertilisers use aggregated by category.csv")) %>% 
  clean_names()

global_fertiliser_per_category %>% 
  select(crop_category, starts_with("share")) %>% 
  filter(crop_category != "Fruits/Vegetables") %>%
  gather(year, percent, -crop_category) %>% 
  ggplot(aes(x = crop_category, y = percent, fill = year)) +
  geom_bar(position="dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))


# Total global NPK per crop
global_NPK_use_per_category = read_csv(here("Data", "Input", "Global NPK use aggregated by category.csv")) %>% 
  clean_names()

global_NPK_use_per_category_processed = global_NPK_use_per_category %>% 
  select(crop_category, fertiliser_type, quantity_mt_2014_2015) %>% 
  group_by(crop_category) %>% 
  mutate(total_fertiliser = sum(quantity_mt_2014_2015)) %>% 
  arrange(desc(total_fertiliser)) %>% 
  ungroup()

global_NPK_use_per_category$crop_category



global_NPK_use_per_category_processed %>% 
  mutate(fertiliser_type = as.factor(fertiliser_type)) %>% 
  mutate(fertiliser_type = fct_relevel(fertiliser_type, c("N", "P", "K"))) %>% 
  mutate(crop_category = as.factor(crop_category)) %>%
  mutate(crop_category = fct_relevel(crop_category,
                                     c("Wheat", "Rice", "Maize", "Soybean", "Oil Palm" ,"Sugar Crops",
                                       "Other Oilseeds", "Fibre Crops", "Other Cereals",  "Roots & Tubers", 
                                       "Fruits", "Vegetables", "Grassland","Residual"))) %>% 
  ggplot(aes(x = crop_category, y = quantity_mt_2014_2015, fill = fertiliser_type)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Total global NPK use per crop (Mt nutrients)", y = "quantity (Mt)", x = "", fill = "Fertiliser type") +
  theme_classic(base_size = 6) +
  theme(legend.key.size = unit(0.5,"line"),
        legend.position="top")

ggsave(here("Output images","global_NPK_use_per_category.png"), height = 2, width = 3, dpi = 300)

