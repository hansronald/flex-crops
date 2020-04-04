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
  theme_classic(base_size = 7)

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
  theme_classic(base_size = 7)

ggsave(here("Output images", "nutrient_input_average.png"), height = 2, width = 3, dpi = 200)

