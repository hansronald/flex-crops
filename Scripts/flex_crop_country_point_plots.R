library(ggrepel)

base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"
total_flex_crop_import_share = read_csv(paste0(base_path, "total_flex_crop_import_share.csv"))
total_flex_crop_export_share = read_csv(paste0(base_path, "total_flex_crop_export_share.csv"))

total_flex_crop_trade_share = total_flex_crop_import_share %>% 
  left_join(total_flex_crop_export_share %>% 
              select(-country), by = "iso3_code")

# Find which countries to label, above a certain import level and above a certain export level
points_to_label = total_flex_crop_trade_share %>% 
  filter(flex_crop_import_share > 0.25 | flex_crop_export_share > 0.5) %>% 
  pull(iso3_code)

# Plot the import share vs the export share
total_flex_crop_trade_share %>%
  filter(iso3_code != "SDN") %>% 
  mutate(alpha = ifelse(iso3_code %in% points_to_label, 1, 0.2)) %>% 
  ggplot(aes(x = flex_crop_import_share, y = flex_crop_export_share)) +
  geom_point(aes(alpha = alpha), show.legend = FALSE) +
  geom_text_repel(aes(label = iso3_code), data = total_flex_crop_trade_share %>% 
              filter(iso3_code %in% points_to_label)) +
  labs(title = "Flex crops and commodities share of agricultural trade (2016)",
       x = "Share of total agricultural imports", y = "Share of total agricultural exports")

ggsave(file = paste0(image_path, "total_flex_crop_trade_share.png"))


total_flex_crop_trade_share %>% filter(iso3_code == "MUS")
