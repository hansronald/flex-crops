library(ggrepel)

national_import = import_value %>%
  filter(year == 2016) %>%
  group_by(country, iso3_code) %>%
  summarise(national_import = sum(import_value))

national_export = export_value %>%
  filter(year == 2016) %>%
  group_by(country, iso3_code) %>%
  summarise(national_export = sum(export_value)) %>% 
  ungroup() %>% 
  select(-country)

national_trade = national_import %>% 
  ungroup() %>% 
  left_join(national_export, by = "iso3_code") %>% 
  mutate(trade_balance = national_export / national_import) %>% 
  mutate(trade_balance_dominance = ifelse(trade_balance > 1, "Exporting", "Importing")) %>% 
  select(country, iso3_code, trade_balance, trade_balance_dominance)

countries_df = crop_data %>%
  distinct(country, iso3_code, country_group) %>% 
  filter(!country_group %in% c("Micronesia", "Polynesia"))

national_crop_category_areaharvest_share = crop_data %>% 
  filter(year == 2016,
         measures == "Area harvested") %>% 
  select(country, iso3_code, source, crop_category, country_group, value) %>% 
  group_by(country) %>% 
  mutate(national_crop_category_area_harvested = sum(value)) %>% 
  ungroup() %>% 
  group_by(country, crop_category) %>% 
  mutate(national_crop_category_harvest = sum(value),
         national_crop_category_harvest_share = national_crop_category_harvest/national_crop_category_area_harvested) %>%
  distinct(country, iso3_code, crop_category, country_group, national_crop_category_harvest_share) %>% 
  filter(crop_category == "Flex crops") %>% 
  ungroup() %>% 
  select(-c(crop_category, country_group)) %>% 
  filter(!is.na(national_crop_category_harvest_share))

# Export data


trade_threshold = 0.4

national_crop_group_import_value_proportion_filtered = national_crop_group_import_value_proportion %>% 
  ungroup() %>% 
  select(iso3_code, source, import_value, national_crop_import_value_proportion) %>%
  rename(import_share = national_crop_import_value_proportion) %>% 
  rename(most_imported_flex_crop = source)

national_crop_group_export_value_proportion_filtered = national_crop_group_export_value_proportion %>% 
  ungroup() %>% 
  select(iso3_code, source, export_value, national_crop_export_value_proportion) %>%
  rename(export_share = national_crop_export_value_proportion) %>% 
  rename(most_export_flex_crop = source)

national_flex_crop_status = countries_df %>% 
  left_join(national_crop_category_areaharvest_share) %>% 
  left_join(national_crop_group_import_value_proportion_filtered) %>% 
  left_join(national_crop_group_export_value_proportion_filtered) %>% 
  left_join(national_trade) %>% 
  rename(harvest_share = national_crop_category_harvest_share) %>% 
  #rename(export_share = national_crop_export_value_proportion) %>% 
  mutate(harvest_share = ifelse(is.na(harvest_share), 0, harvest_share)) %>%
  mutate(import_value = ifelse(is.na(import_value), 0, import_value)) %>%
  mutate(import_share = ifelse(is.na(import_share), 0, import_share)) %>% 
  mutate(export_value = ifelse(is.na(export_value), 0, export_value)) %>%
  mutate(export_share = ifelse(is.na(export_share), 0, export_share)) %>% 
  mutate(low_trade = ifelse(import_share < trade_threshold & export_share < trade_threshold, TRUE, FALSE)) %>% 
  mutate(low_export = ifelse(export_share < trade_threshold, TRUE, FALSE)) %>% 
  mutate(low_import = ifelse(import_share < trade_threshold, TRUE, FALSE))
  filter(!is.na(source))
  
  #mutate(change_sign = change / abs(change)) %>% 
  #mutate(limit = ifelse(export_share_2016 > 0.1, "high", "low")) %>% 
  #filter(!is.na(limit)) %>%
  
theme_set(theme_light(base_size = 8))
  
national_flex_crop_status %>%
  filter(low_trade == FALSE) %>% 
  ggplot(aes(x = import_share, y = export_share, label = iso3_code,
             color = most_export_flex_crop)) + 
  geom_point(aes(size = harvest_share, shape = trade_balance_dominance)) +
  #geom_text() +
  geom_text_repel(hjust=0.5, vjust=0.5, size = 4) +
#  facet_wrap(~low_export, scales = "free") +
  labs(title = "National flex crop share") +
  annotate("rect", xmin = 0, xmax = trade_threshold, ymin = 0, ymax = trade_threshold, 
           alpha = .5)

suppressMessages(ggsave(file='images/export/export_vs_import_share_2016.png')) #saves g


library(stringi)

flex_crops = crop_production_data %>% filter(crop_category == "Flex crops") %>% distinct(source) %>% pull()
stri_paste(flex_crops, collapse=', ')

potential_flex_crops = crop_production_data %>% filter(crop_category == "Potential flex crops") %>% distinct(source) %>% pull()
stri_paste(potential_flex_crops, collapse=', ')

other_commodity_crops = crop_production_data %>% filter(crop_category == "Other commodity crops") %>% distinct(source) %>% pull()
stri_paste(other_commodity_crops, collapse=', ')

other_oil_seeds = crop_production_data %>% filter(crop_category == "Other oil seeds") %>% distinct(source) %>% pull()
stri_paste(other_oil_seeds, collapse=', ')

other_cereals = crop_production_data %>% filter(crop_category == "Other cereals") %>% distinct(source) %>% pull()
stri_paste(other_cereals, collapse=', ')

other_crops = crop_production_data %>% filter(crop_category == "Other crops") %>% distinct(source) %>% pull()
stri_paste(other_crops, collapse=', ')

