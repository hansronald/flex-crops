library(tidyverse)
library(janitor)
library(stringr)
library(treemap)

theme_set(theme_classic())

pesticides_data_raw = read_csv("/Users/robinlindstrom/Downloads/world_data120.csv")

pesticides_data_cleaned = pesticides_data_raw %>% 
  gather(pesticide, value, c(-wrld_simpl.NAME, -wrld_simpl.ISO3)) %>% 
  clean_names() %>% 
  rename(country = wrld_simpl_name,  iso3_code = wrld_simpl_iso3)

pesticides_data = pesticides_data_cleaned %>% 
  separate(col = pesticide, into =  c("apr", "crop", "active_ingredient",
                                      "year", "estimate", "statistic"), sep = "_")

# Which countries have the most pestecides applied totally for 2015
pesticides_data %>% 
  filter(statistic == "total",
         estimate == "L",
         year == 2015) %>% 
  group_by(country) %>% 
  summarise(national_application = sum(value)) %>% 
  arrange(desc(national_application))

# Who has the highest pesticide application for Corn in 2015
national_corn_application = pesticides_data %>%
  filter(statistic == "total",
         estimate == "L",
         year == 2015,
         crop == "Corn") %>% 
  group_by(country, iso3_code) %>% 
  summarise(national_application = sum(value)) %>% 
  arrange(desc(national_application)) %>% 
  ungroup()

# Who has the highest pesticide application for Corn in 2015
national_soybean_application = pesticides_data %>%
  filter(statistic == "total",
         estimate == "L",
         year == 2015,
         crop == "Soybean") %>% 
  group_by(country, iso3_code) %>% 
  summarise(national_application = sum(value)) %>% 
  arrange(desc(national_application)) %>% 
  ungroup()

# Plot a barplot of corn application
national_corn_application %>%
  top_n(10, national_application) %>% 
  mutate(iso3_code = fct_reorder(as.factor(iso3_code), national_application, .desc = TRUE)) %>% 
  ggplot(aes(x = iso3_code, y = national_application)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 90)) +
  labs(title = "Total national application of pestecides", x = "",
       subtitle = "Corn - top 20 active ingredients", y = "Application (tonnes)") +
  scale_y_continuous(labels = scaleFUN)

# Plot a barplot of soybean application
national_soybean_application %>%
  top_n(10, national_application) %>% 
  mutate(iso3_code = fct_reorder(as.factor(iso3_code), national_application, .desc = TRUE)) %>% 
  ggplot(aes(x = iso3_code, y = national_application)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 90)) +
  labs(title = "Total national application of pesticides", x = "",
       subtitle = "Soybean - top 20 active ingredients", y = "Application (tonnes)") +
  scale_y_continuous(labels = scaleFUN)

# Make a tree map of the results
pesticides_data %>% 
  filter(statistic == "total",
         estimate == "L",
         year == 2015) %>% 
  group_by(crop) %>% 
  summarise(total_application = sum(value, na.rm = TRUE)) %>% 
  arrange(desc(total_application)) %>% 
  mutate(application_share = total_application / sum(total_application),
         label = paste0(crop, "\n(", round(application_share,3)*100, "%)")) %>% 
  mutate(crop = fct_reorder(as.factor(crop), total_application)) %>% 
  treemap(index = c("label"),
          vSize = "application_share",
          title = "Global soybean import share")


df = pesticides_data %>% 
  filter(year == 2015,
         estimate == "L",
          statistic == "mean") %>% 
  select(c(-apr, -estimate, -statistic, -year)) %>% 
  mutate(crop_ai = paste0(crop, "_", active_ingredient)) %>% 
  select(country, iso3_code, crop_ai, value)

pivot_wider(df,
            id_cols = c(country, iso3_code),
            names_from = crop_ai,
            values_from = value) %>% 
  add_tally() %>% 
  View()
  
  
df %>% 
  group_by(country, iso3_code) %>% 
  summarise(mean_national_apr = mean(value)) %>% 
  arrange(desc(mean_national_apr))

            