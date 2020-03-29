source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")

FC_national_production_data_2016 = read_csv(paste0(base_path, "FC_national_production_data_2016.csv"))

FC_national_exports_2016 = read_csv(paste0(base_path, "FC_national_exports_2016.csv")) %>% 
  select(iso3_code, source_crop, value, share, cumulative_share)

FC_national_imports_2016 = read_csv(paste0(base_path, "FC_national_imports_2016.csv")) %>% 
  select(iso3_code, source_crop, value, share, cumulative_share)

FC_national_production_data_2016 %>% 
  group_by(item) %>% 
  mutate(pct = value / sum(value), cumsum_pct = cumsum(pct))
  filter(value > quantile(value, .95)) %>% 
  #filter(cumulative_share < 0.85) %>% 
  ungroup() %>% 
  mutate(country = tidytext::reorder_within(country, share, item)) %>% 
  ggplot(aes(x = country, y = share, fill = item)) +
  facet_wrap(~item, scales = "free_y") +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop production share", subtitle = "Covering 80% of global production", x = "", fill = "") +
  theme(legend.position = "none")

FC_national_exports_2016 %>% 
  group_by(source_crop) %>% 
  filter(cumulative_share < 0.8) %>% 
  ungroup() %>% 
  mutate(country = tidytext::reorder_within(country, share, source_crop)) %>% 
  ggplot(aes(x = country, y = share, fill = source_crop)) +
  facet_wrap(~source_crop, scales = "free_y") +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop equivalents export share", subtitle = "Covering 80% of global exports", x = "", fill = "") +
  theme(legend.position = "none")

production = FC_national_production_data_2016 %>% 
  select(-c(share, cumulative_share)) %>% 
  group_by(item) %>% 
  top_n(10, value) %>% 
  
  ungroup()

dput(production)


  

production %>% 
  group_by(item) %>% 
  mutate(share = value / sum(value),
         cumulative_share = cumsum(share)) %>% 
  filter(!cumulative_share >= 0.5)

  
FC_national_production_data_2016 %>% 
  group_by(item) %>% 
  filter(cume_dist(desc(value)) < 0.01)
  
test = FC_national_production_data_2016 %>% filter(item == "Maize") %>% select(value) %>% pull()
quantile(test, type = 1)



