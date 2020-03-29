

base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

theme_set(theme_classic())
write_csv(flex_crop_export_share, paste0(base_path, "export_top_flex_crop.csv"))


total_flex_crop_export_share = read_csv(paste0(base_path, "total_flex_crop_export_share.csv"))

export_top_flex_crop = read_csv(paste0(base_path, "export_top_flex_crop.csv")) %>% 
  rename("Dominating export" = n_countries_with_flex_crop_dominating_export)
import_top_flex_crop = read_csv(paste0(base_path, "import_top_flex_crop.csv")) %>% 
  rename("Dominating import" = n_countries_with_flex_crop_dominating_import)

trade_top_flex_crop = export_top_flex_crop %>% 
  left_join(import_top_flex_crop, by = c("year")) %>% 
  gather(-year, key = "type", value = "value")


trade_top_flex_crop %>% 
  ggplot(aes(x = year, y = value, color = type)) +
  geom_line() +
  labs(title = "Number of countries with a flex as highest share of trade value", x = "", y = "No. of countries")

ggsave(file = paste0(image_path, "no_countries_with_flex_crop_highest_trade_value.png"))
