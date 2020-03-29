library(ggalluvial)

img_path = "/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"
EU28 = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
theme_set(theme_classic())

detailed_trade_matrix_quantity_EU_grouped = detailed_trade_matrix_quantity_spread %>% 
  
  # Group all EU countries
  mutate(reporter_countries = ifelse(reporter_country_iso3 %in% EU28, "EU28", reporter_countries)) %>%
  mutate(reporter_country_iso3 = ifelse(reporter_country_iso3 %in% EU28, "EU28", reporter_country_iso3)) %>%
  
  mutate(partner_countries = ifelse(partner_country_iso3 %in% EU28, "EU28", partner_countries)) %>%
  mutate(partner_country_iso3 = ifelse(partner_country_iso3 %in% EU28, "EU28", partner_country_iso3)) %>%
  
  group_by(reporter_countries, reporter_country_iso3, partner_countries, partner_country_iso3, item,
           source_crop) %>% 
  summarise(export_quantity = sum(export_quantity),
            import_quantity = sum(import_quantity))

# Soybean traders
soybean_trade_matrix = detailed_trade_matrix_quantity_EU_grouped %>% 
  filter(source_crop == "Soybeans")

# A higher number would yield more exporters
global_export_share_threshold = 0.85

# A higher number would yield more importers per country
national_import_share_threshold = 0.65

# Get the top exporting countries ISO3 code
top_soybean_exporters_iso3 = soybean_trade_matrix %>% 
  group_by(reporter_countries, reporter_country_iso3) %>% 
  summarise(total_export = sum(export_quantity)) %>% 
  arrange(desc(total_export)) %>% 
  ungroup() %>% 
  mutate(cumul_perc = cumsum(total_export) / sum(total_export)) %>% 
  filter(cumul_perc <= global_export_share_threshold) %>% 
  pull(reporter_country_iso3)

# Get the top exporters main importing countries
soybean_top_exporters_top_importers = soybean_trade_matrix %>% 
  filter(reporter_country_iso3 %in% top_soybean_exporters_iso3) %>% 
  group_by(reporter_country_iso3, partner_country_iso3) %>% 
  summarise(total_exports = sum(export_quantity)) %>% 
  arrange(reporter_country_iso3, desc(total_exports)) %>% 
  group_by(reporter_country_iso3) %>% 
  mutate(cum_perc = cumsum(total_exports) / sum(total_exports)) %>% 
  filter(cum_perc < national_import_share_threshold) %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3) %>% 
  select(-cum_perc) 

### Alluvial

# Plot the alluvial of soybean exports and imports
subtitle_text = paste0("Export share min ", global_export_share_threshold*100,
                       "%, national import share min ", national_import_share_threshold*100, "%")

soybean_top_exporters_top_importers %>% 
  ggplot(aes(y = total_exports, axis1 = source, axis2 = destination)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_text(stat = "stratum", infer.label = TRUE, size = 4) +
  scale_x_discrete(limits = c("Exports", "Imports"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Global soybean exports and imports", subtitle = subtitle_text) +
  theme(legend.position = "none")

ggsave(paste0(img_path, "soybean_top_exporters_top_importers_plot.png"))


# Maize traders
maize_trade_matrix = detailed_trade_matrix_quantity_EU_grouped %>% 
  filter(source_crop == "Maize")

# A higher number would yield more exporters
global_export_share_threshold = 0.85

# A higher number would yield more importers per country
national_import_share_threshold = 0.6

# Get the top exporting countries ISO3 code
top_maize_exporters_iso3 = maize_trade_matrix %>% 
  group_by(reporter_countries, reporter_country_iso3) %>% 
  summarise(total_export = sum(export_quantity)) %>% 
  arrange(desc(total_export)) %>% 
  ungroup() %>% 
  mutate(cumul_perc = cumsum(total_export) / sum(total_export)) %>% 
  filter(cumul_perc <= global_export_share_threshold) %>% 
  pull(reporter_country_iso3)

# Get the top exporters main importing countries
maize_top_exporters_top_importers = maize_trade_matrix %>% 
  filter(reporter_country_iso3 %in% top_maize_exporters_iso3) %>% 
  group_by(reporter_country_iso3, partner_country_iso3) %>% 
  summarise(total_exports = sum(export_quantity)) %>% 
  arrange(reporter_country_iso3, desc(total_exports)) %>% 
  group_by(reporter_country_iso3) %>% 
  mutate(cum_perc = cumsum(total_exports) / sum(total_exports)) %>% 
  filter(cum_perc < national_import_share_threshold) %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3) %>% 
  select(-cum_perc) 

### Alluvial

# Plot the alluvial of maize exports and imports
subtitle_text = paste0("Export share min ", global_export_share_threshold*100,
                       "%, national import share min ", national_import_share_threshold*100, "%")

maize_top_exporters_top_importers %>% 
  ggplot(aes(y = total_exports, axis1 = source, axis2 = destination)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_text(stat = "stratum", infer.label = TRUE, size = 4) +
  scale_x_discrete(limits = c("Exports", "Imports"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Global maize exports and imports", subtitle = subtitle_text) +
  theme(legend.position = "none")

ggsave(paste0(img_path, "maize_top_exporters_top_importers_plot.png"))


# Oil palm traders
OP_trade_matrix = detailed_trade_matrix_quantity_EU_grouped %>% 
  filter(source_crop == "Oil palm fruit")

# A higher number would yield more exporters
global_export_share_threshold = 0.90

# A higher number would yield more importers per country
national_import_share_threshold = 0.7

# Get the top exporting countries ISO3 code
top_OP_exporters_iso3 = OP_trade_matrix %>% 
  group_by(reporter_countries, reporter_country_iso3) %>% 
  summarise(total_export = sum(export_quantity)) %>% 
  arrange(desc(total_export)) %>% 
  ungroup() %>% 
  mutate(cumul_perc = cumsum(total_export) / sum(total_export)) %>% 
  filter(cumul_perc <= global_export_share_threshold) %>% 
  pull(reporter_country_iso3)

# Get the top exporters main importing countries
OP_top_exporters_top_importers = OP_trade_matrix %>% 
  filter(reporter_country_iso3 %in% top_OP_exporters_iso3) %>% 
  group_by(reporter_country_iso3, partner_country_iso3) %>% 
  summarise(total_exports = sum(export_quantity)) %>% 
  arrange(reporter_country_iso3, desc(total_exports)) %>% 
  group_by(reporter_country_iso3) %>% 
  mutate(cum_perc = cumsum(total_exports) / sum(total_exports)) %>% 
  filter(cum_perc < national_import_share_threshold) %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3) %>% 
  select(-cum_perc) 

### Alluvial

# Plot the alluvial of Oil palm exports and imports
subtitle_text = paste0("Export share min ", global_export_share_threshold*100,
                       "%, national import share min ", national_import_share_threshold*100, "%")

OP_top_exporters_top_importers %>% 
  ggplot(aes(y = total_exports, axis1 = source, axis2 = destination)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_text(stat = "stratum", infer.label = TRUE, size = 4) +
  scale_x_discrete(limits = c("Exports", "Imports"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Global oil palm exports and imports", subtitle = subtitle_text) +
  theme(legend.position = "none")

ggsave(paste0(img_path, "OP_top_exporters_top_importers_plot.png"))

