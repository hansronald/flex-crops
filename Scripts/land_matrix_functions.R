
library(tidyverse) # For tidyr and dplyr
library(data.table) # For fread to read large data tables
library(janitor) # Fot clean names


deals = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Land matrix/deals.csv"))
investors = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Land matrix/investors.csv"))
involvements = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Land matrix/involvements.csv"))


deals_raw = read_tsv("~/Google Drive/SRC/Thesis/x.Code/Data/Land matrix/deals.txt") %>%
  clean_names()

exclude_columns = c(paste("location_", 3:21, sep = ""),
                    paste("contracts_", 3:21, sep = ""),
                    paste("data_source_", 3:29, sep = ""))

deals_filtered = deals_raw %>% 
  # Remove columns that are not used (there are hundreds of them and easier to navigate if removed)
  #dplyr::select(-matches(paste(exclude_columns, collapse="|"))) %>% 
  dplyr::select(deal_id, deal_scope, deal_size, top_parent_companies, location_1_location, location_1_target_country,
                contract_farming_crops, comment_on_contract_farming_crops, comment_on_in_country_processing_of_produce, crops_area)

deals_cleaned = deals_filtered %>% 
  # Remove everything after "#" and "Unknown" in column "top_parent_companies"
  # Remove "###" from crops_area
  # mutate(top_parent_companies = gsub("#.*", "\\1", top_parent_companies)) %>%
  # mutate(top_parent_companies = gsub("(Unknown).*", "\\1", top_parent_companies)) %>% 
  separate(top_parent_companies, into = c("top_parent_companies", "some_number", "company_location"), sep = "([\\#])") %>% 
  mutate(crops_area = str_remove(crops_area, "###"))

# Count occurence of deal per company
deals_cleaned %>% 
  count(top_parent_companies) %>% 
  arrange(desc(n))

# Count occurence of deal per country of company
deals_cleaned %>% 
  count(company_location) %>% 
  arrange(desc(n)) %>% 
  View()

deals_cleaned %>% 
  filter(company_location == "Sweden") %>% 
  View()


# Count occurence of deal per target country
deals_cleaned %>% 
  count(location_1_target_country) %>% 
  arrange(desc(n))

# Put crops into separate rows, separating on ","
deals_crop_separate = deals_cleaned %>% 
  separate_rows(crops_area, sep = ",") %>%
  # Remove the leading white space
  mutate(crops_area = trimws(crops_area, which = "left"))

# Which crops are the most common in land deals
deals_crop_separate %>% 
  group_by(crops_area) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  View()


deals_crop_separate %>%
  filter(location_1_target_country == "Ukraine") %>% 
  View()
  count(top_parent_companies)  
  arrange(desc(n))






