library(tidyverse)
library(janitor)
library(here)

theme_set(theme_classic())

#base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
#image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# Functions
scaleFUN <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )
  paste0(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
         c("","K","M","B","000B")[div] )}

make_map_data = function(map_input_data){
  # Join the map data with the crop data
  library(rnaturalearth)
  library(sf)
  library(rgeos)
  library(dplyr)
  world <- ne_countries(scale = "small", returnclass = "sf")
  
  world_filtered = world %>% 
    select(country = name_long, iso3_code = iso_a3, geometry) %>%
    filter(!is.na(iso3_code)) %>% 
    filter(country != "Antarctica") 
  
  map = full_join(world_filtered, map_input_data, by = "iso3_code") 
  
  return(map)
}
