library(treemap)

img_path = "/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"
EU28 = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")

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

# Plot and save the treemap of soybean imports
png(filename=paste0(img_path, "detailed_trade_matrix_quantity_EU_grouped.png"))
treemap_plot = detailed_trade_matrix_quantity_EU_grouped %>%
  filter(reporter_country_iso3 == "USA") %>% 
  group_by(partner_country_iso3) %>% 
  summarise(soybean_exports_quantity = sum(export_quantity)) %>%
  mutate(export_share = round(soybean_exports_quantity / sum(soybean_exports_quantity),2),
         partner_country_iso3 = ifelse(export_share >= 0.05, paste0(partner_country_iso3, " (", round(export_share,2)*100, "%)"), partner_country_iso3)) %>% 
  arrange(desc(export_share)) %>% 
  treemap(index = c("partner_country_iso3"),
          vSize = "soybean_exports_quantity",
          title = "Global soybean import share")
dev.off()

# Soybean traders
soybean_trade_matrix = detailed_trade_matrix_quantity_EU_grouped %>% 
  filter(source_crop == "Soybeans")

# A higher number would yield more exporters
global_export_share_threshold = 0.85

# A higher number would yield more importers per country
national_import_share_threshold = 0.6

top_soybean_exporters_iso3 = soybean_trade_matrix %>% 
  group_by(reporter_countries, reporter_country_iso3) %>% 
  summarise(total_export = sum(export_quantity)) %>% 
  arrange(desc(total_export)) %>% 
  ungroup() %>% 
  mutate(cumul_perc = cumsum(total_export) / sum(total_export)) %>% 
  filter(cumul_perc <= global_export_share_threshold) %>% 
  pull(reporter_country_iso3)

# Treemap of soybean exports above 90% of total world exports
soybean_trade_matrix %>% 
  filter(reporter_country_iso3 %in% top_soybean_exporters_iso3) %>% 
  group_by(reporter_country_iso3, partner_country_iso3) %>% 
  summarise(total_exports = sum(export_quantity)) %>% 
  treemap(index = c("reporter_country_iso3", "partner_country_iso3"),
          vSize = "total_exports", title = "Soybean main exporters")

soybean_top_exporters_top_importers = soybean_trade_matrix %>% 
  filter(reporter_country_iso3 %in% top_soybean_exporters_iso3) %>% 
  group_by(reporter_country_iso3, partner_country_iso3) %>% 
  summarise(total_exports = sum(export_quantity)) %>% 
  arrange(reporter_country_iso3, desc(total_exports)) %>% 
  group_by(reporter_country_iso3) %>% 
  mutate(cum_perc = cumsum(total_exports) / sum(total_exports)) %>% 
  filter(cum_perc < national_import_share_threshold)

soybean_top_exporters_top_importers_NW = soybean_top_exporters_top_importers %>%
  rename(source = reporter_country_iso3, destination = partner_country_iso3) %>% 
  select(-cum_perc) 

### Alluvial
# library(ggalluvial)

iso_labs = unique(c(soybean_top_exporters_top_importers_NW %>% 
  pull(source), soybean_top_exporters_top_importers_NW %>% 
  pull(destination)))

subtitle_text = paste0("Export share min ", global_export_share_threshold*100, "%, national import share min ", national_import_share_threshold*100, "%")

soybean_top_exporters_top_importers_NW %>% 
  ggplot(aes(y = total_exports, axis1 = source, axis2 = destination)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_text(stat = "stratum", infer.label = TRUE, size = 4) +
  scale_x_discrete(limits = c("Exports", "Imports"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Global soybean exports and imports", subtitle = subtitle_text) +
  theme_set(theme_classic())

  
