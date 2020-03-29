### Source ----
source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")
library(scales)
# source("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/read_production_data.R")
# -----------------

## Data explore ----
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# rm(list = ls(all.names = TRUE)) 
theme_set(theme_classic(base_size = 12))

# Read the data
import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"
crop_production_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"

import_quantity_data = as_tibble(fread(import_quantity_data_path))
export_quantity_data = as_tibble(fread(export_quantity_data_path))
crop_production_data = read_csv(crop_production_data_path)

FC_import_quantity_data = import_quantity_data %>%
  filter(crop_category == "Flex crops")

FC_export_quantity_data = export_quantity_data %>%
  filter(crop_category == "Flex crops")

FC_crop_production_data = crop_production_data %>% 
  filter(crop_category == "Flex crops",
         measures == "Production")

# FC_import_quantity_data %>% 
#   filter(year == 2016) %>% 
#   group_by(iso3_code, country, source_crop) %>% 
#   summarise(FC_imports = sum(value)) %>% 
#   ungroup() %>% 
#   group_by(source_crop) %>% 
#   mutate(global_FC_imports = sum(FC_imports)) %>% 
#   mutate(global_FC_import_share = FC_imports / global_FC_imports) %>% 
#   ungroup() %>% 
#   group_by(source_crop) %>% 
#   top_n(5, global_FC_import_share) %>% 
#   arrange(global_FC_import_share)

# Imports
top_FC_importers = FC_import_quantity_data %>% 
  filter(year == 2016) %>% 
  group_by(iso3_code, country, source_crop) %>% 
  summarise(FC_imports = sum(value)) %>% 
  ungroup() %>% 
  group_by(source_crop) %>% 
  arrange(source_crop, desc(FC_imports)) %>% 
  mutate(national_FC_share = round(FC_imports / sum(FC_imports), 2)) %>% 
  mutate(cumulative_percentage = round(cumsum(FC_imports) / sum(FC_imports), 2)*100) %>% 
  mutate(lab = paste0(source_crop, "\n(Cumulative % = ", cumulative_percentage[5], ")")) %>% 
  top_n(5, FC_imports) %>% 
  ungroup()

top_FC_importers %>%  
  ungroup() %>% 
  mutate(country = tidytext::reorder_within(country, national_FC_share, source_crop)) %>% 
  ggplot(aes(x = country, y = national_FC_share, fill = source_crop)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~lab, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = percent) +
  labs(title = "Global import share (in weight)", y = "share", x = "")

ggsave(paste0(image_path, "top_FC_importers.png"))  

# Exports
top_flex_crop_exporters = FC_export_quantity_data %>% 
  filter(year == 2016) %>% 
  group_by(iso3_code, country, source_crop) %>% 
  summarise(FC_exports = sum(value)) %>% 
  ungroup() %>% 
  group_by(source_crop) %>% 
  arrange(source_crop, desc(FC_exports)) %>% 
  mutate(national_FC_share = round(FC_exports / sum(FC_exports), 2)) %>% 
  mutate(cumulative_percentage = round(cumsum(FC_exports) / sum(FC_exports), 2)*100) %>% 
  mutate(lab = paste0(source_crop, "\n(Cumulative % = ", cumulative_percentage[5], ")")) %>% 
  top_n(5, FC_exports) %>% 
  ungroup()

top_flex_crop_exporters %>%  
  ungroup() %>% 
  mutate(country = tidytext::reorder_within(country, national_FC_share, source_crop)) %>% 
  ggplot(aes(x = country, y = national_FC_share, fill = source_crop)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~lab, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = percent) +
  labs(title = "Global export share (in weight)", y = "share", x = "")

ggsave(paste0(image_path, "top_FC_exporters.png"))  




# Production
top_FC_producers = FC_crop_production_data %>% 
  filter(year == 2016) %>% 
  group_by(iso3_code, country, source_crop) %>% 
  summarise(FC_production = sum(value)) %>% 
  ungroup() %>% 
  group_by(source_crop) %>% 
  arrange(source_crop, desc(FC_production)) %>% 
  mutate(national_FC_share = round(FC_production / sum(FC_production), 2)*100) %>% 
  mutate(cumulative_percentage = round(cumsum(FC_production) / sum(FC_production), 2)) %>% 
  mutate(lab = paste0(source_crop, "\n(Cumulative % = ", cumulative_percentage[5], ")")) %>% 
  top_n(5, FC_production) %>% 
  ungroup()

top_FC_producers %>%  
  ungroup() %>% 
  mutate(country = tidytext::reorder_within(country, national_FC_share, source_crop)) %>% 
  ggplot(aes(x = country, y = national_FC_share, fill = source_crop)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~lab, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = percent) +
  labs(title = "Global production share (in weight)", y = "share", x = "")

ggsave(paste0(image_path, "top_FC_producers.png"))  



