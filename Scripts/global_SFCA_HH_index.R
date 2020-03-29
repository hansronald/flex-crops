### Source ----
source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/flex_crops_functions.R")
# source("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/read_production_data.R")
# -----------------

## Data explore ----
base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# rm(list = ls(all.names = TRUE)) 
theme_set(theme_light(base_size = 12))

# Read the data
import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"
import_quantity_data = as_tibble(fread(import_quantity_data_path))
export_quantity_data = as_tibble(fread(export_quantity_data_path))

flex_crop_data = crop_data %>%
  filter(crop_category == "Flex crops")

#rm(crop_data)

# Import quantity share
individual_flex_crop_import_share = import_quantity_data %>% 
  filter(crop_category == "Flex crops") %>% 
  group_by(year, source_crop) %>%
  mutate(total_individual_flex_crop_import = sum(value)) %>% 
  mutate(individual_flex_crop_import_share = value / total_individual_flex_crop_import) %>% 
  distinct(iso3_code, country, source_crop, individual_flex_crop_import_share) %>% 
  ungroup()

HH_index_individual_flex_crops_imports = individual_flex_crop_import_share %>% 
  group_by(year, source_crop) %>% 
  summarise(HH_import_index = sum(individual_flex_crop_import_share^2))


# export quantity share
individual_flex_crop_export_share = export_quantity_data %>% 
  filter(crop_category == "Flex crops") %>% 
  group_by(year, source_crop) %>%
  mutate(total_individual_flex_crop_export = sum(value)) %>% 
  mutate(individual_flex_crop_export_share = value / total_individual_flex_crop_export) %>% 
  distinct(iso3_code, country, source_crop, individual_flex_crop_export_share) %>% 
  ungroup()

HH_index_individual_flex_crops_exports = individual_flex_crop_export_share %>% 
  group_by(year, source_crop) %>% 
  summarise(HH_export_index = sum(individual_flex_crop_export_share^2))

individual_flex_crop_production_share = flex_crop_data %>% 
  filter(year %in% 1961:2016) %>% 
  filter(measures == "Production") %>% 
  group_by(year, source_crop) %>%
  mutate(total_individual_flex_crop_production = sum(value)) %>% 
  mutate(individual_flex_crop_share = value / total_individual_flex_crop_production) %>% 
  distinct(iso3_code, country, source_crop, individual_flex_crop_share) %>% 
  ungroup()
  
HH_index_individual_flex_crops_production = individual_flex_crop_production_share %>% 
  group_by(year, source_crop) %>% 
  summarise(HH_production_index = sum(individual_flex_crop_share^2))
  
#HH_index_individual_flex_crops_production %>% 
#  ggplot(aes(x = year, y = HH_production_index, color = source_crop)) +
#  geom_line() +
#  labs(title = "Herfindal herchman index of flex crop production")

HH_index_individual_flex_crops = HH_index_individual_flex_crops_production %>% 
  full_join(HH_index_individual_flex_crops_exports, by = c("year", "source_crop")) %>% 
  full_join(HH_index_individual_flex_crops_imports, by = c("year", "source_crop")) %>% 
  gather(-c(year, source_crop), key = "measures", value = "value")

HH_index_individual_flex_crops %>% 
  ggplot(aes(x = year, y = value, color = measures)) +
  geom_line() +
  facet_wrap(~source_crop) +
  labs(title = "Herfindahl–Hirschman Index (HHI) of global flex crop activity", x = "", y = "HHI") +
  scale_color_discrete(name = "Measure", labels = c("Production", "Exports", "Imports"))

ggsave(paste0(image_path, "Global_IFCA_HH_index.png"))


