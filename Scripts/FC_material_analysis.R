image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

theme_set(theme_classic(base_size = 12))
library(ggrepel)

#import_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/import_quantity_data.csv"
export_quantity_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/export_quantity_data.csv"

#import_quantity_data = as_tibble(fread(import_quantity_data_path))
export_quantity_data = as_tibble(fread(export_quantity_data_path))

crop_data_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/crop_production_data_processed.csv"
crop_data = read_csv(crop_data_path)


  

#FC_import_quantity_2016 = import_quantity_data %>% 
#  filter(year == 2016,
#         crop_category == "Flex crops",
#         material == "Raw") %>% 
#  group_by(country, source_crop) %>% 
#  summarise(national_crop_exports_2016 = sum(value))

FC_export_quantity_2016 = export_quantity_data %>% 
  filter(year == 2016,
         crop_category == "Flex crops",
         material == "Raw") %>% 
  group_by(country, source_crop) %>% 
  summarise(national_crop_exports_2016 = sum(value))

# Raw, processed distribution for all flex crops
export_quantity_data %>%
  filter(crop_category == "Flex crops",
         year == 2016) %>% 
  group_by(source_crop, item, material) %>% 
  summarise(global_export_weight = sum(value)) %>%
  ungroup() %>% 
  mutate(material = ifelse(item == "Cane Sugar Raw Centrifugal", "Processed", material)) %>% 
  filter(!item %in% c("Sugar non-centrifugal", "Sugar refined")) %>% 
  arrange(source_crop, desc(material)) %>% 
  group_by(source_crop) %>% 
  mutate(total_crop_export_weight = sum(global_export_weight)) %>% 
  group_by(source_crop, material) %>% 
  mutate(material_weight = sum(global_export_weight)) %>% 
  mutate(material_share = round(material_weight/total_crop_export_weight,2 )) %>% 
  select(-c("total_crop_export_weight", "material_weight"))

# Soybeans

FC_production_quantity_2016 = crop_data %>% 
  filter(year == 2016,
         item == "Soybeans") %>% 
  filter(measures == "Production") %>% 
  select(iso3_code, value) %>% 
  rename(production = value)

export_quantity_data %>%
  filter(source_crop == "Soybeans") %>% 
  group_by(year, material) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = material)) +
  geom_area()


export_quantity_data %>%
  filter(source_crop == "Soybeans") %>% 
  group_by(year, item) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = item)) +
  geom_area()

soybean_national_summary_2016 = export_quantity_data %>%
  filter(source_crop == "Soybeans",
         year == 2016) %>% 
  group_by(item, iso3_code, country) %>% 
  summarise(value = sum(value)) %>% 
  spread(item, value) %>% 
  clean_names() %>% 
  mutate_at(c("cake_soybeans", "oil_soybean", "soybeans"), ~coalesce(., 0)) %>% 
  mutate(total_exports = cake_soybeans + oil_soybean + soybeans,
         raw_share = soybeans / total_exports) %>% 
  left_join(FC_production_quantity_2016, by = "iso3_code") %>% 
  arrange(desc(production)) %>% 
  mutate(export_share = total_exports / production) %>% 
  ungroup()
  
soybean_national_summary_2016 %>% 
  top_n(10, production) %>% 
  ggplot(aes(x = raw_share, y = export_share, label = iso3_code)) +
  geom_point() +
  geom_text_repel(aes(label = iso3_code)) +
  labs(x = "Raw soybean share of exports", y = "Export share of total production",
       title = "Top 10 soybean producers comparison")

ggsave(paste0(image_path, "soybean_national_summary_2016.png"))
  









