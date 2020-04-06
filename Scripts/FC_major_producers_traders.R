source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")
here()
FC_national_production_data_2016 = read_csv(here("Output data", "FC_national_production_data_2016.csv"))

FC_national_exports_2016 = read_csv(here("Output data", "FC_national_exports_2016.csv")) %>% 
  select(iso3_code, country, source_crop, value, share, cumulative_share)

FC_national_imports_2016 = read_csv(here("Output data", "FC_national_imports_2016.csv")) %>% 
  select(iso3_code, country, source_crop, value, share, cumulative_share)


# Plot the top producers of flex crops -----

FC_major_producers = FC_national_production_data_2016 %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  filter(share > 0.05) %>% 
  mutate(cumulative_share = sum(share)) %>% 
  # This will give the producers that are covering at least 80% of the production
  # filter(lag(cumulative_share <= 0.8, default = TRUE)) %>% 
  ungroup()

# Make labels to show the total percentage that each crop is covered by the selected countries
temp = FC_major_producers %>% 
  distinct(item, cumulative_share) %>% 
  mutate(cumulative_share = paste0(as.character(round(cumulative_share*100)),"%")) %>% 
  pull(cumulative_share)

# Make the new labels with the cumulative share data
labels = c(Maize = paste0("Maize (", temp[1], ")"), `Oil palm fruit` = paste0("Oil palm fruit (", temp[2], ")"),
  Soybeans = paste0("Soybeans (", temp[3], ")"), `Sugar cane` = paste0("Sugar cane (", temp[4], ")"))

# Plot the major producers
FC_major_producers %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, item)) %>% 
  mutate(item = as.factor(item)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = item)) +
  facet_wrap(~item, scales = "free_y",
             labeller=labeller(item = labels)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop production share", subtitle = "Each county accounting for at least 5% of production", x = "", fill = "") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set2")

ggsave(here("Output images", "FC_major_producers.png"))

# FC_national_production_data_2016 %>% 
#   group_by(item) %>% 
#   arrange(desc(value), .by_group = TRUE) %>% 
#   filter(lag(cumulative_share <= 0.6, default = TRUE)) %>% 
#   select(country, share)

# Plot the major exports of flex crops -------
FC_major_exporters = FC_national_exports_2016 %>% 
  group_by(source_crop) %>% 
  filter(share > 0.05) %>% 
  mutate(cumulative_share = sum(share)) %>% 
  # This will give the producers that are covering at least 80% of the production
  # filter(lag(cumulative_share <= 0.8, default = TRUE)) %>% 
  ungroup()

# Make labels to show the total percentage that each crop is covered by the selected countries
temp = FC_national_major_exporters %>% 
  distinct(source_crop, cumulative_share) %>% 
  mutate(cumulative_share = paste0(as.character(round(cumulative_share*100)),"%")) %>% 
  pull(cumulative_share)

# Make the new labels with the cumulative share data
labels = c(Maize = paste0("Maize (", temp[1], ")"), `Oil palm fruit` = paste0("Oil palm fruit (", temp[2], ")"),
           Soybeans = paste0("Soybeans (", temp[3], ")"), `Sugar cane` = paste0("Sugar cane (", temp[4], ")"))


FC_national_major_exporters %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, source_crop)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = source_crop)) +
  facet_wrap(~source_crop, scales = "free_y", labeller = labeller(source_crop = labels)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop equivalents export share",
       subtitle = "Each county accounting for at least 5% of production", x = "", fill = "") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set2")

ggsave(here("Output images", "FC_major_exporters.png"))

# Plot the major importers of flex crops -------
FC_major_importers = FC_national_imports_2016 %>% 
  group_by(source_crop) %>% 
  filter(share > 0.03) %>% 
  mutate(cumulative_share = sum(share)) %>% 
  # This will give the producers that are covering at least 80% of the production
  # filter(lag(cumulative_share <= 0.8, default = TRUE)) %>% 
  ungroup()

# Make labels to show the total percentage that each crop is covered by the selected countries
temp = FC_major_importers %>% 
  distinct(source_crop, cumulative_share) %>% 
  mutate(cumulative_share = paste0(as.character(round(cumulative_share*100)),"%")) %>% 
  pull(cumulative_share)

# Make the new labels with the cumulative share data
labels = c(Maize = paste0("Maize (", temp[1], ")"), `Oil palm fruit` = paste0("Oil palm fruit (", temp[2], ")"),
           Soybeans = paste0("Soybeans (", temp[3], ")"), `Sugar cane` = paste0("Sugar cane (", temp[4], ")"))

FC_major_importers %>% 
  mutate(iso3_code = tidytext::reorder_within(iso3_code, share, source_crop)) %>% 
  ggplot(aes(x = iso3_code, y = share, fill = source_crop)) +
  facet_wrap(~source_crop, scales = "free_y", labeller = labeller(source_crop = labels)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(title = "Global flex crop equivalents export share",
       subtitle = "Each county accounting for at least 5% of production", x = "", fill = "") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set2")

ggsave(here("Output images", "FC_major_importers.png"))


