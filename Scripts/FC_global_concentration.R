source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")

FC_national_production_data_2016 = read_csv(here("Output", "FC_national_production_data_2016.csv"))

FC_national_exports_2016 = read_csv(here("Output", "FC_national_exports_2016.csv")) %>% 
  select(iso3_code, country, source_crop, value, share, cumulative_share)

FC_national_imports_2016 = read_csv(here("Output", "FC_national_imports_2016.csv")) %>% 
  select(iso3_code, country, source_crop, value, share, cumulative_share)

FC_national_production_data_2016 %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  filter(lag(cumulative_share <= 0.8, default = TRUE)) %>% 
  ungroup() %>% 
  #mutate(pct = value / sum(value), cumsum_pct = cumsum(pct))
  #filter(value > quantile(value, .95)) %>% 
  #filter(cumulative_share < 0.85) %>% 
  ungroup() %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, item)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = item)) +
  facet_wrap(~item, scales = "free_y") +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop production share", subtitle = "Covering 80% of global production", x = "", fill = "") +
  theme(legend.position = "none")

FC_national_exports_2016 %>% 
  group_by(source_crop) %>% 
  filter(lag(cumulative_share <= 0.8, default = TRUE)) %>% 
  #filter(cumulative_share < 0.8) %>% 
  ungroup() %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, source_crop)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = source_crop)) +
  facet_wrap(~source_crop, scales = "free_y") +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop equivalents export share", subtitle = "Covering 80% of global exports", x = "", fill = "") +
  theme(legend.position = "none")


FC_national_imports_2016 %>% 
  group_by(source_crop) %>% 
  filter(lag(cumulative_share <= 0.5, default = TRUE)) %>% 
  #filter(cumulative_share < 0.8) %>% 
  ungroup() %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, source_crop)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = source_crop)) +
  facet_wrap(~source_crop, scales = "free_y") +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop equivalents export share", subtitle = "Covering 50% of global exports", x = "", fill = "") +
  theme(legend.position = "none")


