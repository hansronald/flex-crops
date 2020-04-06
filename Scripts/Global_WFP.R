library(tidyverse)
library(janitor)
library(here)
library(ggforce) # for 'geom_arc_bar'
library(ggpubr)
source(here("Scripts", "common_settings.R"))

crop_wfp = read_csv("/Users/robinlindstrom/Downloads/Global avg wfp.csv") %>% 
  clean_names() %>% 
  select(item_code = product_code_faostat, item_hs = product_description_hs, green_water = global_average_green_water,
         blue_water = global_average_blue_water, grey_water = global_average_grey_water)
FAO_codes = read_csv("/Users/robinlindstrom/Downloads/FAOSTAT_data_4-4-2020.csv") %>% 
  clean_names() %>% 
  select(item_code, item)

crop_production_data = read_csv("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Output processed data/crop_production_data_processed.csv")

production_2018 = crop_production_data %>% 
  filter(year == 2018,
         measures == "Production") %>% 
  group_by(item_code) %>% 
  summarise(production = sum(value))

crop_prod_wfp = crop_wfp %>% 
  left_join(FAO_codes, by = "item_code") %>% 
  left_join(production_2018, by = "item_code")

crop_prod_wfp %>% 
  filter(!is.na(item)) %>% 
  filter(item == "Maize")

global_wfp = crop_prod_wfp %>% 
  filter(!is.na(production)) %>% 
  replace_na(list(green_water = 0, blue_water = 0, grey_water = 0)) %>% 
  mutate(avg_wfp = green_water + blue_water + grey_water,
         total_wfp = avg_wfp * production,
         wfp_share = total_wfp / sum(total_wfp)) %>%
  select(item, total_wfp, wfp_share) %>% 
  arrange(desc(total_wfp)) %>% 
  mutate(wfp_cum_share = cumsum(wfp_share) / sum(wfp_share)) %>%
  mutate(lump = ifelse(wfp_share > 0.02, FALSE, TRUE)) %>% 
  mutate(item = ifelse(lump == TRUE, "Other", item)) %>%
  mutate(rown = row_number()) %>% 
  group_by(item) %>% 
  summarise(total_wfp = sum(total_wfp),
            rown = min(rown)) %>% 
  arrange(rown) %>% 
  mutate(wfp_share = total_wfp / sum(total_wfp),
         wfp_cum_share = cumsum(wfp_share) / sum(wfp_share))

write_csv(global_wfp, here("Output data", "global_wfp.csv"))  


global_wfp %>%
  mutate(item = as.factor(item),
         item = ((fct_reorder(item, rown, .desc = TRUE)))) %>% 
  ggplot(aes(x = 1, y = total_wfp, fill = item)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start=0, direction = -1) +
  theme(axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.title=element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        line = element_blank()) +
  scale_y_continuous(breaks=cumsum(global_wfp$total_wfp) - global_wfp$total_wfp / 2,
                     labels= paste0(round(global_wfp$wfp_share, 2)*100, "%")) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Share of total global water footprint (2018)", tag = "(a)")

ggsave(here("Output images", "global_wfp_share.png"), dpi = 500)

scale_value = 1

plot(brewer.pal(1, "Paired"))

temp <- 1:length(crop_colors)
barplot(temp, col = crop_colors)

crop_colors = brewer.pal(9, "Paired")

names(crop_colors) = levels(fct_relevel(as.factor(global_wfp$item), c("Other", "Barley", "Seed cotton", "Oil palm fruit", "Sugar cane",
                                                                      "Soybeans", "Rice, paddy", "Wheat", "Maize")))
  
  levels(as.factor(global_wfp$item))


  
  levels(as.factor())

global_wfp %>% 
  mutate(item = as.factor(item),
         item = fct_reorder(item, rown, .desc = TRUE)) %>%
  mutate(total_wfp = ifelse(item == "Other", 0, total_wfp)) %>% 
  ggplot(aes(x = item, y = total_wfp, fill = item)) +
  geom_bar(stat = "identity", width = 0.9*scale_value) +
  geom_text(aes(label = round(total_wfp/1e9)), hjust = 1.1, size = 2.5) +
  coord_flip() +
  labs(title = "Global water footprint (2018)", x = "", y = "water footprint"~(m^3), tag = "(b)") +
  scale_y_continuous(breaks = scales::extended_breaks(n = 6), labels = scaleFUN) +
  scale_fill_brewer(palette="Paired") +
  theme_classic(base_size = 8*scale_value) +
  theme(aspect.ratio = 1*scale_value,
        #legend.key.size = unit(0.5*scale_value,"line"),
        legend.position = "none")

ggsave(here("Output images", "global_wfp.png"), height = 2.5*scale_value, width = 4*scale_value, dpi = 500)

# Combine
library(gridExtra)

ggarrange(g1, g2, common.legend = TRUE)
grid.arrange(g1, g2)

