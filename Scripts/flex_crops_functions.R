# Read data ---------------------------------------
#setwd("~/Google Drive/SRC/Thesis/")

library(tidyverse) # For tidyr and dplyr
library(data.table) # For fread to read large data tables
library(janitor) # To clean column names to snake_case
library(countrycode) # To exctract ISO country codes
library(spData) # For world data
library(scales)
library(segmented)
library(ggpubr) # For ggarrange
library(WDI)
library(zoo) # For read.zoo function
library(strucchange)

#library(sf) # For loading map data
library(tmap)    # for static and interactive maps

#established_flex_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize")
#emerging_flex_crops = c("Cassava", "Coconuts", "Rapeseed", "Sugar beet", "Sunflower seed")
#all_flex_crops = c(established_flex_crops, emerging_flex_crops)

read_data = function(data_path){
  
  crop_production_categories = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/crop-livestock-categories.csv")) %>% 
    clean_names() %>%
  #  filter(item_group == "Crops Primary") %>% 
  #  filter(category %in% c("Cereals", "Oilseeds", "Roots and tubers", "Sugars", "Vegetable oil", "Fruits")) %>% 
    dplyr::select(flex_crop_category, item_group, item) %>%
    filter(item_group == "Crops Primary")
  
  # Add regions and income level
  wits = as_tibble(fread("~/Downloads/WITS.csv")) %>% 
    clean_names()
  
  wits_filtered = wits %>% 
    filter(region != "") %>% 
    select(country_name, country_iso3, income_group, region, wto_member) %>% 
    mutate(iso2c = countrycode(country_name, 'country.name', 'iso2c')) %>% 
    mutate(iso2c=replace(iso2c, country_name=="Ethiopia(excludes Eritrea)", "ET")) %>%
    filter(!is.na(iso2c)) %>% 
    select(-country_name, -country_iso3)
  
  # FAO data only has a country code, not the iso code. This is used to map with other data using iso2code
  FAO_codes = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/FAO_codes.csv")) %>%
    clean_names() %>%
    dplyr::select(country_code, iso2_code) %>% 
    rename("iso2c" = "iso2_code") %>% 
    left_join(wits_filtered, by = "iso2c")
  

  
  # Read the production file. Rename countries with long names to shorter names.
  crop_production_data_raw = as_tibble(fread(data_path)) %>% 
    clean_names() %>%
#    filter(!item %in% c("Cassava leaves", "Palm kernels", "Oil, palm" )) %>% # This is just a temporary work-around
    rename("country" = "area", "country_code" = "area_code", "measures" = "element") %>% 
    mutate(country = replace(country, country == "United States of America", "USA")) %>%  
    mutate(country = replace(country, country == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>%  
    mutate(country = replace(country, country == "Syrian Arab Republic", "Syria")) %>% 
    mutate(country = replace(country, country == "Russian Federation", "Russia")) %>% 
    mutate(country = replace(country, country == "Republic of Moldova", "Moldova")) %>% 
    mutate(country = replace(country, country == "Republic of Korea", "South korea")) %>% 
    mutate(country = replace(country, country == "Iran (Islamic Republic of)", "Iran")) %>% 
    mutate(country = replace(country, country == "Lao People's Democratic Republic", "Laos")) %>% 
    mutate(country = replace(country, country == "Democratic Republic of the Congo", "DR Congo")) %>% 
    mutate(country = replace(country, country == "Democratic People's Republic of Korea", "North korea")) %>% 
    mutate(country = replace(country, country == "China, Taiwan Province of", "Taiwan")) %>% 
    mutate(country = replace(country, country == "China, mainland", "China")) %>% 
    mutate(country = replace(country, country == "Bolivia (Plurinational State of)", "Bolivia")) %>% 
    mutate(country = replace(country, country == "Bosnia and Herzegovina", "Bosnia")) %>%
    mutate(country = replace(country, country == "United Republic of Tanzania", "Tanzania")) %>%
    mutate(country = replace(country, country == "The former Yugoslav Republic of Macedonia", "North macedonia")) %>%
    mutate(country = replace(country, country == "C\xf4te d'Ivoire", "Cote d'Ivore")) %>%
    mutate(country = replace(country, country == "R\xe9union", "Reunion")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    left_join(FAO_codes) %>% 
    left_join(crop_production_categories) %>% 
    filter(iso2c != "") %>% 
    filter(!is.na(flex_crop_category))
    # clean_names renames columan names with snake_case
    #clean_names() %>%
    #rename(unit_weight = unit, weight = value) %>%
    #filter(item %in% all_flex_crops)
  
  # Replace all NA values in yield/production/area harvested with 0
  #crop_production_data_raw[,8:121][is.na(crop_production_data_raw[,8:121])] = 0
  
  return(crop_production_data_raw)
}

# Read data ---------------------------------------------------------------

#world <- st_read(system.file("shapes/world.gpkg", package="spData"))

# Clean data --------------------------------------------------------------

# Remove antarctica because it is ugly
#world = world %>%
#  filter(name_long != "Antarctica")
#world$iso_a2 = as.character.factor(world$iso_a2)
#world[world$name_long == "Norway",]$iso_a2 = "NO"
#world[world$name_long == "France",]$iso_a2 = "FR"
#world$iso_a2 = as.factor(world$iso_a2)

# Check the difference between the ISO2 codes between world and other data
#setdiff(world$iso_a2, test$iso2c)

# Add the ISO3 data to world
#world_iso3code = left_join(world, codelist %>%
#                             select(iso2c, iso3c), by = c("iso_a2" = "iso2c"))

get_crop_data = function(crop_production_data_raw, crops = unique(crop_production_data_raw$item), measure, year){
  
  # Need y before year since it is in the columns
  year_column = paste("y",year, sep = "")
  
  # Which columns am I interested in
  selected_columns = c("country", "iso2c", "item", "measures",
                       "flex_crop_category", "income_group", "region", year_column)
  
  # FAO data only has a country code, not the iso code. This is used to map with other data using iso2code
  FAO_codes = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/FAO_codes.csv")) %>%
    clean_names() %>%
    dplyr::select(country_code, iso2_code) %>% 
    rename("iso2c" = "iso2_code")
  
  # # Get land area from WDI, all years
  # country_land_area_raw <- as_tibble(data.frame(
  #   WDI(country = "all",
  #       # Land area come as sqkm
  #       indicator = c("AG.LND.TOTL.K2"),
  #       start = min(year),
  #       end = max(year),
  #       extra = TRUE)))
  # 
  # land_use_raw = as_tibble(fread("/Users/robinlindstrom/Google Drive/SRC/Thesis/x.Code/Data/Land use/land_use_cropland_agricultural_land_FAO.csv")) %>% 
  #   clean_names()
  # 
  # crop_area_share = land_use_raw %>% 
  #   filter(item == "Cropland",
  #          element == "Share in Land area") %>% 
  #   left_join(FAO_codes, by = c("area_code" = "country_code")) %>% 
  #   
  #   # Values are in %, make them into fractions for calculations, and rename them to crop_area_share
  #   mutate(value = value * 0.01) %>%
  #   rename(crop_area_share_of_land_area = value) %>% 
  #   select(area, item, year, crop_area_share_of_land_area, iso2c) %>%
  #   select(crop_area_share_of_land_area, iso2c, year)
  # 
  # # Filter only countries and convert to hectares
  # land_use_data = country_land_area_raw %>%
  #   
  #   # Retrieve total land area, agricultural land area nad crop land area from the World Bank
  #   filter(region != "Aggregates") %>% 
  #   left_join(crop_area_share, by = c("iso2c", "year")) %>% 
  #   mutate(land_area = AG.LND.TOTL.K2 * 100) %>% 
  #   # crop land comes as a proportion of total land area, this is to get the sqkm
  #   mutate(crop_land_area = crop_area_share_of_land_area * land_area) %>% 
  #   dplyr::select(iso2c, land_area, crop_land_area, year)

    # TODO This is not working correctly    
    # Values are missing in 2017, replace them with values from 2016
#    group_by(iso2c)
#    fill(crop_land_area, .direction = "down") %>%
#    fill(agri_land_area, .direction = "down")
  land_use_data = get_land_use_data()
  # Filter out the crops selected, remove NA and gather on year
  crop_data_out = crop_production_data_raw %>%
  #    dplyr::select(-land_area) %>% 
    filter(item %in% crops) %>%
    filter(measures %in% measure) %>%
    dplyr::select(selected_columns) %>%
#    mutate(country = sub("C\xf4te d'Ivoire", "Cote d'Ivore", country)) %>% 
#    mutate(country = sub("R\xe9union", "Reunion", country)) %>% 
    gather(year, value, -country, -iso2c, -item, -measures, -flex_crop_category, -income_group, -region) %>% 
    #mutate(value = value/1000000) %>%
    # Divide the different   
#  spread(measures, value) %>% 
#    clean_names() %>% 
#  mutate(`Area harvested` = `Area harvested` / 1000, `Production` = `Production` / 1000000, `Yield` = `Yield` / 1000) %>% 
#  gather("measures", "value", `Area harvested`, `Production`, `Yield`) %>%
    na.omit() %>% 
    mutate(year = as.numeric(gsub("y", "", year))) %>% 
    left_join(land_use_data, by = c("iso2c", "year"))
  
  return(crop_data_out)
}

get_land_use_data = function(){
  setwd("~/Google Drive/SRC/Thesis/x.Code/Data/Land use")
  path_name = "FAO_land_use.csv.zip"
  fread_path = paste("unzip -p", path_name)
  land_use_raw = as_tibble(fread(fread_path)) %>% 
    clean_names()
  
  land_use_filtered = land_use_raw %>% 
    select(area_code, item, year, value) %>% 
    rename(country_code = area_code)
  
  land_use_data = land_use_filtered %>% 
    mutate(value = value*1000) %>% 
    spread(item, value) %>% 
    clean_names()
  
  return(add_iso2_fao_data(land_use_data))
}

add_iso2_fao_data = function(data){

  FAO_codes = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/FAO_codes.csv")) %>%
    clean_names() %>%
    dplyr::select(country_code, iso2_code) %>% 
    rename("iso2c" = "iso2_code")
  
  return(data %>%
           left_join(FAO_codes))
}

make_crop_map_data = function(data){
  # Join the map data with the crop data

  world_filtered = world %>% 
    filter(name_long != "Antarctica")
  
  map = left_join(world_filtered %>% 
                    dplyr::select(iso_a2, geom), data, c("iso_a2"= "iso2c"))# %>% 
  #  filter(!is.na(country))
  
  map = map %>% 
    mutate(value = replace(value, is.na(value), 0))
#  filter(is.na(weight))
  return(map)
}

make_crop_map = function(crop_map){
  tm_shape(crop_map) +
    tm_polygons(col = "value", palette = "BuPu", style = "jenks", title = "Weight (tonnes)")
#    tm_layout(panel.labels = paste(crop, " (", production_year, ")", sep = ""))
}

point_plot = function(crop_data, category, measure, production_year, n_countries, x_axis_text){
  
  len = length(unique(crop_data$item))
  # Filter the which ctegories, measures and which year to use
  # Find the top producing country
  plot_data = crop_data %>%
    filter(measures == measure,
           year == production_year, flex_crop_category == category) %>% 
    group_by(item, value) %>% 
    arrange(item, desc(value)) %>%
    group_by(item) %>% 
    top_n(n_countries, value) %>%
    arrange(item, desc(value)) %>% 
    ungroup() %>% 
    mutate(rank = n_countries*len - row_number() + 1)
  
  # Value is divided by 1000 since 
  # crop_data %>% 
  #   filter(year == "2016", measures == "Area harvested") %>% 
  #   mutate(agri_area_prop = ifelse(measures == "Area harvested", (value * 10000) / agri_land_area , "")) %>%
  #   group_by(country) %>% 
  #   summarise(summa = sum(agri_area_prop)) %>% 
  #   arrange(desc(summa))
  #   View()
  #   arrange(desc(agri_area_prop))
  #   group_by(country) %>% 
  #   View()
  #   summarise(total = mean(ag_area_prop))
  # 
  plot_data %>% 
    ggplot() +
    geom_segment( aes(x=rank, xend=rank, y=0, yend=value), color="grey") +
    geom_point( aes(x=rank, y=value), size = 3) +
    coord_flip() +
    facet_wrap(~item, scales = "free", ncol = 2) +
    labs(title = measure, y = x_axis_text, x = "") +
    scale_x_continuous(
      breaks = plot_data$rank, # specify tick breaks using rank column
      labels = str_wrap(plot_data$country, width = 15) # specify tick labels using x column
    )
}

time_series_plot = function(crop_data, category, measure, scale){
  
  lbls = unique(crop_data$year)
  brks = as.Date(paste(lbls, 1, 1, sep = "-"))

  crop_data %>% 
    filter(flex_crop_category == category, measures == measure) %>% 
    dplyr::select(item, year, value) %>%
    group_by(item, year) %>% 
    summarize(total_value = sum(value)) %>% 
    ggplot(aes(year, total_value)) +
    geom_line() +
    facet_wrap(~item, scales = scale, ncol = 2) +
    scale_y_continuous(limits=c(0,NA)) +
    geom_vline(xintercept = 2009, linetype = "dotted", color = "red") +
    labs(y = "", x = measure)

    
  # crop_data %>% 
  #   dplyr::select(item, year, weight) %>%
  #   filter(item == crop) %>% 
  #   group_by(item, year) %>% 
  #   summarize(total_weight = sum(weight)) %>% 
  #   ggplot(aes(year, total_weight)) +
  #   geom_line() +
  #   geom_vline(xintercept = 2009, linetype = "dotted", color = "red") +
  #   labs(title = crop, y = "", x = "")
#    scale_x_date(labels = lbls, breaks = brks)
}

time_series_category_plot = function(crop_data, index_plot, scale){
  
  # flex_crops_total_value_and_index = crop_data %>% 
  #   dplyr::select(year, value, measures, flex_crop_category) %>%
  #   group_by(year, measures, flex_crop_category) %>% 
  #   summarize(total_value = sum(value)) %>% 
  #   group_by(measures) %>%
  #   mutate(value_index = total_value/total_value[year == min(year)])
  
  # TODO: this can be optimised, gets 5 duplicates
  flex_crops_total_value_and_index = crop_data %>%
    dplyr::select(year, value, measures, item, flex_crop_category) %>%
    group_by(year, measures, flex_crop_category) %>% 
    mutate(total_value = ifelse(measures == "Yield", mean(value), sum(value))) %>% 
    ungroup() %>% 
    distinct(year, measures, flex_crop_category, .keep_all = TRUE) %>% 
    select(-item, -value)
  
  if(index_plot == TRUE){
    ggplot(flex_crops_total_value_and_index) +
      geom_line(aes(x=year, y=value_index, colour = str_wrap(flex_crop_category, width = 10))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(y = "index", x = "") +
      facet_wrap(~measures, ncol = 2, scales = scale) +
      scale_y_continuous(limits=c(1,NA)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
#      guides(shape = guide_legend(override.aes = list(size = 0.5)))
      theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
    
  }else{
    hospital_names <- c(
      `Area harvested` = "Area harvested ('000 hectares)",
      `Production` = "Production ('000 000 tonnes)",
      `Yield` = "Yield ('000 tonnes/hectare)"
      )
    ggplot(flex_crops_total_value_and_index) +
      geom_line(aes(x=year, y=total_value, colour = str_wrap(flex_crop_category, width = 10))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#      geom_vline(xintercept = 2009, linetype = "dotted", color = "black") +
      labs(y = "ha (thousands), tonnes (millons) and ha/tonnes (thousands) respectively", x = "") +
      facet_wrap(~measures, ncol = 2, scales = scale, labeller = as_labeller(hospital_names)) +
      scale_y_continuous(limits=c(0,NA)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
#      guides(shape = guide_legend(override.aes = list(size = 0.5)))
      theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
    
  }
  
}

time_series_crop_comparison_plot = function(crop_data, category, measure, fraction){

  category
  crop_plot_data = crop_data %>% 
    filter(flex_crop_category == category, measures %in% measure) %>% 
    dplyr::select(year, value, measures, item) %>%
    group_by(year, measures, item) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(measures) %>%
    mutate(value_index = total_value / total_value) %>% 
    #print()
    group_by(measures, item) %>% 
    mutate(value_index = total_value/total_value[year == min(unique(year))]) %>% 
    group_by(measures, year) %>%
    mutate(fraction_value = total_value / sum(total_value))
  
  #  dplyr::select(-total_value)
  if(fraction == TRUE){
    ggplot(crop_plot_data) +
      geom_line(aes(x=year, y=fraction_value, colour = str_wrap(item, width = 8))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#      geom_vline(xintercept = 2009, linetype = "dotted", color = "black") +
      labs(y = "index", x = "") +
      facet_wrap(~measures, nrow = 2, scales = "free_y") +
      scale_y_continuous(limits=c(0,NA)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
#      guides(shape = guide_legend(override.aes = list(size = 0.5)))
      theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
    
    }else{
      ggplot(crop_plot_data) +
        geom_line(aes(x=year, y=total_value, colour = str_wrap(item, width = 10))) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#        geom_vline(xintercept = 2009, linetype = "dotted", color = "black") +
        labs(y = "value", x = "") +
        facet_wrap(~measures, nrow = 2, scales = "free_y") +
        scale_y_continuous(limits=c(0,NA)) +
        theme(axis.text.x = element_text(angle=60, hjust=1)) +
#        guides(shape = guide_legend(override.aes = list(size = 0.5)))
        theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
  }
}

time_series_crop_comparison_plot_proportion = function(crop_data, measure){
  
  flex_crops_total_value_and_index = crop_data %>% 
    dplyr::select(year, value, measures, flex_crop_category) %>%
    filter(measures %in% measure) %>% 
    group_by(year, measures, flex_crop_category) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(measures) %>% 
    mutate(value_index = total_value/total_value[year == min(year)]) %>% 
    group_by(year, measures) %>% 
    mutate(total_value_all = sum(total_value)) %>% 
    mutate(fraction_value = total_value/total_value_all)
  # mutate(flex_fraction = total_value[flex_crop_category == "Flex crop"] / sum(total_value))
  
  ggplot(flex_crops_total_value_and_index) +
    geom_line(aes(x=year, y=fraction_value, colour = str_wrap(flex_crop_category, width = 10))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(y = "proportion", x = "") +
    facet_wrap(~measures, ncol = 2, scales = "free_y") +
    scale_y_continuous(limits=c(0,NA)) +
    theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  #filter(flex_crop_category != "Non flex crop")
}


break_point_plot = function(crop_data, measure, crops, y_axis_text){

  flex_production = crop_data %>% 
    mutate(item = as.factor(item)) %>% 
    filter(item %in% crops, measures == measure) %>%
    group_by(item, year) %>% 
    summarize(total_value = sum(value)) %>%
    ungroup() %>% 
    select(item, year, total_value)
  
  my.lines = NULL
  for(i in 1:length(crops)){
    
    item_filter = crops[i]
    item_production = flex_production %>%
      filter(item == item_filter)
    
    my.lm <- lm(total_value ~ year, data = item_production)
    my.seg <- segmented(my.lm, 
                        seg.Z = ~ year,
                        psi = NA, control=seg.control(display=FALSE, K=2))
    
    # Another option is to put starting years
    # psi = list(year=c(1995, 2010)))
    
    if(is.null(my.seg$psi[, 2])){
      my.lines = rbind(my.lines, cbind(NA, matrix(c(item_filter, item_filter))))
    }else{
      my.lines = rbind(my.lines, cbind(matrix(my.seg$psi[, 2]), matrix(c(item_filter, item_filter))))
    }
  }
  
  
  liness = as.data.frame(my.lines)
  colnames(liness) = c("year", "item")
  breaks = liness %>% 
    mutate(year = round(as.numeric(as.character(year))))
  
  
  ggplot(flex_production, aes(year, total_value)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_line() +
    #scale_y_continuous(limits=c(0,NA)) +
    labs(title = measure, y = y_axis_text, x = "") +
    facet_wrap(~ item, ncol = 2, scales = "free_y") +
    geom_vline(data = breaks, aes(xintercept = year), size = 0.5, linetype = "dashed") +
    geom_text(data = breaks, aes(x = year, y = 0, label = year), hjust = -.25, size = 2.5) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  
  
}

break_point_plot2 = function(crop_data, measure, crops){
  
  flex_production = crop_data %>% 
    filter(item %in% crops, measures == measure) %>%
    group_by(item, year) %>% 
    summarize(total_value = sum(value)) %>%
    ungroup() %>% 
    select(item, year, total_value)
  
  plot_list = list()
  for(i in 1:length(crops)){
    
    item_filter = crops[i]
    item_production = flex_production %>%
      filter(item == item_filter)
    
    p = ggplot(item_production, aes(year, total_value)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      geom_line() +
      scale_y_continuous(limits=c(0,NA)) +
      labs(title = item_filter)
    #  geom_vline(xintercept = 2009, linetype = "dotted", color = "red")
    #  facet_wrap(~item)
    
    my.lm <- lm(total_value ~ year, data = item_production)
    #summary(my.lm)
    
    # From help on seg.Z: the segmented variables(s), i.e. the continuous covariate(s) understood to
    # have a piecewise-linear relationship with response. It is a formula with no response
    # variable, such as seg.Z=~x or seg.Z=~x1+x2. It can be missing when obj
    # (a "lm" or "glm" fit) includes only one covariate which is taken as segmented variable.
    # Currently, formulas involving functions, such as seg.Z=~log(x1) or even seg.Z=~sqrt(x1),
    # or selection operators, such as seg.Z=~d[,"x1"] or seg.Z=~d$x1, are not allowed.
    
    ## Segmented analysis
    my.seg <- segmented(my.lm, 
                        seg.Z = ~ year,
                        psi = list(year=c(1995, 2010)))
    
    # For number of breaks or for years as start value
    # npsi = 2)
    # psi = list(year=c(1995, 2000))
    
    # get the fitted data
    #my.fitted <- fitted(my.seg)
    #my.model <- data.frame(year = soybean_production$year, production = my.fitted)
    
    # plot the fitted model (vertical lines)
    my.lines <- my.seg$psi[, 2]
    #ggplot(my.model, aes(x = year, y = production)) + geom_line() +
    #  geom_vline(xintercept = my.lines, linetype = "dashed")
    
    if(is.null(my.lines)){
      plot_list[[i]] = p
    }else{
      
      line.data <- data.frame(xintercept = my.lines, Lines = round(my.lines),
                              color = c("red", "blue"), stringsAsFactors = FALSE)
      
      plot_list[[i]] = p +
        annotate("text", line.data$xintercept, 0, hjust = -.25, 
                 label = line.data$Lines, size = 2.5) +
        geom_vline(aes(xintercept = xintercept),
                   line.data, size = 0.5, linetype = "dashed")
      #scale_colour_manual(values = line.data$color, name = "Breakpoints") +
    }
    
  }
  
  figure = ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  annotate_figure(figure, top = text_grob(measure, face = "bold", size = 14))
  #grid.arrange(grobs = plot_list, top = measure)
  
}

stacked_area_plot = function(crop_data, category, crop, measure = "Production",
                             cut_off = 0.8, y_axis_text = "", proportion = TRUE){
  
  plot_data = crop_data %>% 
    filter(flex_crop_category == category) %>% 
    filter(item == crop) %>% 
    filter(measures == measure) %>%  
    #         year %in% 1960:1970,
    #         country %in% unique(crop_data$country)[50:70]) %>% 
    dplyr::select(country, year, value)
  
  plot_order = plot_data %>% 
    mutate(country = as.character(country)) %>%
    filter(year == last(year)) %>% 
    arrange(desc(value)) %>% 
    mutate(rank = row_number()) %>% 
    mutate(total_value = sum(value)) %>% 
    mutate(country_proportion = value/total_value)
  
  # This finds the number of countries that together account for at least 80% of the value
  n_countries = sum(as.numeric(cumsum(plot_order$country_proportion) < cut_off)) + 1
  
  final_plot <- plot_data %>% 
    mutate(country = as.character(country)) %>%
    mutate(plot_label = ifelse(country %in% plot_order$country[1:n_countries], country, 'Other')) %>%
    mutate(plot_label = factor(plot_label, levels = c((plot_order$country[1:n_countries]), 'Other'))) %>%
    group_by(plot_label, year) %>%
    summarise(value = sum(value)) %>% 
    group_by(year) %>% 
    mutate(percentage = value / sum(value))
  
  if(proportion == TRUE){
    final_plot %>%
      ggplot(aes(x=year, y=percentage, fill=plot_label)) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = paste(crop, measure, sep = ": "), fill = "", x = "", y = "proportional") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8))
      #guides(shape = guide_legend(override.aes = list(size = 0.2)))
      #theme(legend.text = element_text(function(x) str_wrap(x, width = 5))) +
      #scale_color_manual(labels = function(x) str_wrap(x, width = 5))
  }else{
    final_plot %>%
      ggplot(aes(x=year, y=value, fill=plot_label)) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = paste(crop, measure, sep = ": "), fill = "", x = "", y = y_axis_text) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8))
    # theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 6))
    #guides(shape = guide_legend(override.aes = list(size = 0.2)))
    #theme(legend.text = element_text(function(x) str_wrap(x, width = 5))) +
    #scale_color_manual(labels = function(x) str_wrap(x, width = 5))
  }
}

stacked_area_plot_per_country = function(crop_data, category, crop, measure = "Production",
                             n_countries = 5, y_axis_text = "", proportion = TRUE){
  
  plot_data = crop_data %>% 
    filter(flex_crop_category == category, item == crop,
           measures == measure) %>%  
    #         year %in% 1960:1970,
    #         country %in% unique(crop_data$country)[50:70]) %>% 
    select(-iso2c, -flex_crop_category, -measures, -item)
  
  plot_order = plot_data %>% 
    mutate(country = as.character(country)) %>%
    filter(year == last(year)) %>% 
    arrange(desc(value)) %>% 
    mutate(rank = row_number())
  
  final_plot <- plot_data %>% 
    mutate(country = as.character(country)) %>%
    mutate(plot_label = ifelse(country %in% plot_order$country[1:n_countries], country, 'Other')) %>%
    mutate(plot_label = factor(plot_label, levels = c((plot_order$country[1:n_countries]), 'Other'))) %>%
    group_by(plot_label, year) %>%
    summarise(value = sum(value)) %>% 
    group_by(year) %>% 
    mutate(percentage = value / sum(value))
  
  final_plot %>%
    ggplot(aes(x=year, y=percentage, fill=plot_label)) + 
    geom_area() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(title = paste(crop, measure, sep = ": "), fill = "", x = "", y = "proportional") +
    theme(axis.text.x = element_text(angle=60, hjust=1)) +
    theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8))

}

crop_proportion_plot_per_country = function(crop_data, category, min_land_area, min_proportion, max_proportion, stacked = FALSE){

  crop_data_proportion = crop_data %>% 
    filter(measures == "Area harvested",
           flex_crop_category == category) %>%  
    
    # First calculate the total area for all crops group
    group_by(country, year) %>%
    mutate(crop_category_area = sum(value)) %>% 
    mutate(crop_category_area_proportion = (crop_category_area) / crop_land_area) %>% 
    ungroup() %>% 
    
    # Then calculate total area per individual crop
    group_by(country, year, item) %>%
    mutate(crop_individual_area = sum(value)) %>%
    mutate(crop_individual_area_proportion = (crop_individual_area) / crop_land_area) %>% 
    select(country, year, crop_individual_area_proportion, crop_category_area_proportion, item, crop_land_area, land_area)
  
  
  # crop_data_proportion %>% 
  #   ungroup() %>% 
  #   filter(year %in% c(2000, 2016)) %>% 
  #   top_n(1, item) %>% 
  #   select(country, year, crop_category_area_proportion) %>% 
  #   mutate(year = paste("Y", year, sep = "")) %>% 
  #   spread(year, crop_category_area_proportion) %>% 
  #   mutate(crop_area_change_prop = Y2016 / Y2000,
  #          crop_area_change_abs = Y2016 - Y2000) %>% 
  # #  arrange(desc(crop_area_change_prop))
  #  arrange(desc(crop_area_change_abs))
  # 
  # crop_data_proportion %>% 
  #   ungroup() %>% 
  #   filter(year %in% c(2000, 2016)) %>%  
  #   select(country, year, crop_individual_area_proportion, item) %>% 
  #   mutate(year = paste("Y", year, sep = "")) %>% 
  #   spread(year, crop_individual_area_proportion) %>% 
  #   mutate(crop_area_change_prop = Y2016 / Y2000,
  #          crop_area_change_abs = Y2016 - Y2000) %>% 
  # #  arrange(desc(crop_area_change_prop))
  #  arrange(desc(crop_area_change_abs))
    
  
  
  # Get the countries which has the largest share of area harvested in 2017
  last_year = last(crop_data_proportion$year)
  top_countries = crop_data_proportion %>% 
    filter(year == last_year) %>% 
    arrange(desc(crop_category_area_proportion)) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    top_n(1, item) %>%
    filter(land_area > min_land_area) %>% 
    filter(crop_category_area_proportion > min_proportion && crop_category_area_proportion < max_proportion) %>%
    pull(country)
  
  final_plot = crop_data_proportion %>% 
    filter(country %in% top_countries)

  if(stacked == TRUE){
    final_plot %>%
      ggplot(aes(x=year, y=crop_individual_area_proportion, fill = item)) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(fill = "", x = "", y = "proportion of total crop production") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8)) +
      facet_wrap(~country)
    
  }else{
    final_plot %>%
      ggplot(aes(x=year, y=crop_individual_area_proportion, color = item)) + 
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(fill = "", x = "", y = "proportion of total crop production") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 8)) +
      facet_wrap(~country)
  }
  
}

plot_HH_index = function(crop_data, category, measure){
  
  # Plots the Herfindahl–Hirschman Index (HHI) for a crop category
  
  HH_index_data = crop_data %>%
    filter(flex_crop_category == category, measures %in% measure) %>% 
    group_by(item, measures, year) %>% 
    
    # The value is calculated by:
    # 1. Calculate the each crops share of production
    # 2. Sum the squares of the percentages of each number calculated in step 1
    mutate(HH_index = value / sum(value)) %>% 
    arrange(desc(HH_index)) %>% 
    summarise_at(vars(HH_index), function(x){return(sum((x*100)^2))})
  
  HH_index_data %>%
    ggplot(aes(x = year, y = HH_index, color = item)) +
    geom_line() +
    geom_line(aes(y=1500), color = "black", linetype = "dashed", size = 0.5) +
    geom_line(aes(y=2500), color = "black", linetype = "dashed", size = 0.5) +
#    geom_hline(aes(yintercept=1500), color = "red", linetyp = "dashed") +
    facet_wrap(~measures, ncol = 2, scale = "free_y") +
    labs(y = "Herfindahl-Hirschman Index") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
    theme(axis.text.x = element_text(angle=60, hjust=1))

}


plot_country_crop_data = function(crop_data, country_var, measure_var, cut_off = 0.8, year_var, stacked = TRUE){
  
  # function that takes crop data, country, time period and measures and plots a stacked
  # area graph of items for each measure over the time period defined. n_items is how many
  # items should be shown in the graph, the rest is grouped into "other".

  plot_data = crop_data %>% 
    filter(country == country_var,
           measures %in% measure_var, year %in% year_var) %>% 
    select(-iso2c, -flex_crop_category) %>% 
    ungroup()
  
  plot_order = plot_data %>% 
    filter(year == last(year)) %>%
    mutate(item = as.character(item)) %>%
    arrange(desc(value)) %>% 
    mutate(total_value = sum(value)) %>% 
    mutate(crop_proportion = value / total_value) %>% 
    mutate(rank = row_number())
  
  n_items = sum(as.numeric(cumsum(plot_order$crop_proportion) < cut_off)) + 1
  
  final_plot <- plot_data %>% 
    mutate(item = as.character(item)) %>%
    mutate(plot_label = ifelse(item %in% plot_order$item[1:n_items], item, 'Other')) %>%
    mutate(plot_label = factor(plot_label, levels = c((plot_order$item[1:n_items]), 'Other'))) %>%
    group_by(plot_label, year) %>%
    summarise(value = sum(value),
              cropland = unique(cropland)) %>% 
    group_by(year) %>% 
    mutate(percentage = value / sum(value))
  
  if(stacked == TRUE){
    final_plot %>%
      ggplot(aes(x=year)) + 
      geom_area(aes(y=value, fill=plot_label)) +
      geom_line(aes(y = cropland)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste(country_var, measure_var, sep = ": "), fill = "", x = "", y = get_measure_scale(measure_var)) +
      theme(legend.key.size = unit(0.2, "cm")) +
      scale_fill_brewer(palette = "Set3")
  }else{
    final_plot %>%
      ggplot(aes(x=year, y=value, color=plot_label)) + 
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste(country_var, measure_var, sep = ": "), color = "", x = "", y = get_measure_scale(measure_var)) +
      theme(legend.key.size = unit(0.2, "cm")) +
      scale_fill_brewer(palette = "Set3")
  }
}


crop_diversity_HH_index = function(crop_data, spatial_extension, country_var = NA){
  
  crop_data_HH_index = crop_data %>% 
    filter(measures == "Area harvested") %>% 
    group_by(country, year) %>% 
    mutate(total_area_harvested_per_country = sum(value)) %>%
    filter(total_area_harvested_per_country != 0) %>% 
    mutate(crop_area_proportion_of_total_area_harvested = value/total_area_harvested_per_country*100) %>% 
    mutate(HH_index = sum(crop_area_proportion_of_total_area_harvested^2)) %>% 
    ungroup() %>% 
    group_by(region, year) %>% 
    mutate(avg_region_HH_index = mean(HH_index)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    mutate(avg_global_HH_index = mean(HH_index))
  
  if(spatial_extension == "global"){
    crop_data_HH_index %>%
      ggplot(aes(x = year, y = avg_global_HH_index)) +
      geom_line()
  }else if(spatial_extension == "regional"){
    crop_data_HH_index %>%
      ggplot(aes(x = year, y = avg_region_HH_index, color = region)) +
      geom_line()
  }else if(spatial_extension == "country"){
    crop_data_HH_index %>%
      filter(country == country_var) %>% 
      ggplot(aes(x = year, y = HH_index)) +
      geom_line() +
      geom_hline(yintercept=c(1500, 2500), linetype="dashed") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste(country_var, "HH index", sep = ": "), color = "", x = "", y = "HH index") +
      theme(legend.key.size = unit(0.2, "cm"))
  }

      
    
  
  
}

get_measure_scale = function(measure){

  # Returns the scale of each measure, used for correct y-axis on graphs

  if(measure == "Production"){
    return("tonnes (millions)")
    }
  else if(measure == "Area harvested"){
    return("hectares (thousands)")
    }
  else{
    return("tonnes/ha (thousands)")
    }
  
}

get_break_points_per_country = function(crop_data, crop, measure){
  
  crop_time_data_filtered = crop_data %>% 
    filter(item %in% crop, measures == measure)
  
  countries = unique(crop_time_data_filtered$country)
  n_countries = length(countries)
  #temp = as.data.frame(matrix(0, n_countries, 2))
  
  country_data = NULL
  break_year_data = NULL
  
  for(i in 1:n_countries){
    crop_time_series_data = crop_time_data_filtered %>% 
      filter(country == countries[i]) %>% 
      select(year, value)
    
    # Don't add if the values sum up to 0, becuse as.ts wont work
    if(sum(crop_time_series_data$value) != 0){
      
      # TODO, why do some as.ts create 
      crop_time_series = as.ts(read.zoo(crop_time_series_data, FUN = as.yearmon))
      
      # If there are NA values for some reasons the as.ts won't work
      if(!any(is.na(crop_time_series))){
        
        # F statistics to indicate breakpoints, added from/to to get a 
        # largers range of years in output
        # TODO sort this one out, makes a lot of error messages when I use custom from/to
        #fs_crops <- Fstats(crop_time_series ~ 1, from = 0, to = 1)
        fs_crops <- Fstats(crop_time_series ~ 1)
        bp_crops = breakpoints(fs_crops)
        break_year = breakdates(bp_crops)
        #lines(breakpoints(fs_crops))
        #print(break_year)
        #dt = rbind(dt, c(countries[i], break_year))
        country_data = append(country_data, countries[i])
        break_year_data = append(break_year_data, break_year)
      }else
        print(paste("NA in ", crop, ": ", countries[i],sep = ""))
    }
  }
  
  # Join the countries and their corresponding breakpoints
  df = as.tbl(data.frame(country_data, break_year_data)) %>% 
    mutate(country_data = as.character(country_data))

  # Create a histogram of the breakpoints
  break_year_histogram = ggplot(df, aes(x=break_year_data)) +
    geom_histogram(bins = 15) +
    labs(title = "") +
    theme_classic()
  
  # Get FAO_codes
  FAO_codes = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/FAO_codes.csv")) %>%
    clean_names() %>%
    dplyr::select(country_code, iso2_code) %>% 
    rename("iso2c" = "iso2_code")
  
  # Join the break map data with map data
  break_year_map_data = df %>% 
    rename("country" = "country_data") %>%
    
    # Join it with my other data to get country_codes
    left_join(crop_production_data_raw %>%
                select(country, country_code) %>% 
                group_by(country) %>% 
                slice(1), by = "country") %>%

    # Join with FAO codes
    left_join(FAO_codes, by = "country_code") %>% 
    select(-country, -country_code) %>% 
    rename("value" = "break_year_data") %>% 
    
    # Join with maps
    left_join(maps::iso3166 %>% select(a2, mapname), by = c(iso2c = "a2")) %>%  
    right_join(world, by = c(mapname = "region"))
  
  # Plot the map data, color corresponds to a range of years
  break_year_map = break_year_map_data %>% 
    mutate(
      break_year = cut(value, breaks = c(1961, 1989, 2016), 
                       labels = c("1961-1989", "1990-2016"))
    ) %>% 
    ggplot(aes(long, lat, group = group, fill = break_year)) +
    geom_polygon() +
    #    scale_fill_discrete(labels = comma) +
    coord_map(xlim=c(-180,180)) +
    theme_void() +
    labs(title = paste("Distribution of break points", crop, ":", measure),
         fill = "Break year")
  
  # An option for automatic labels for the breaks
  #a = breakss[1:(length(breakss)-1)]
  #b = breakss[2:(length(breakss))]
  #paste(a,"-", b, sep = "")
  
  # Setup the design of the grid.arrange
  lay <- rbind(c(1,1,1),
               c(1,1,1),
               c(1,1,1),
               c(2,2,2))
  
  grid.arrange(break_year_map, break_year_histogram, layout_matrix = lay)
  #return(df)
}

get_ts_plot_crop_per_country = function(crop_data, crop, measure, country_var){
  
  ### A function that plots a time series of the production measure of a 
  ### specific crop in a specific country with its specific breakpoint
  
  crop_time_series_data = crop_data %>% 
    filter(item %in% crop, measures == measure) %>% 
    filter(country == country_var) %>% 
    select(year, value)
  
  # If the data is empty then there is no production and we cant plot the data
  if(dim(crop_time_series_data)[1] == 0){
    
    print(paste("No data/no production on", crop, "for", measure, "in", country_var))
    
  }else{
    
    # Make into time series and plot it
    crop_time_series = as.ts(read.zoo(crop_time_series_data, FUN = as.yearmon))
    
    # F statistics to indicate breakpoints, added from/to to get a 
    # largers range of years in output
    fs_crops <- Fstats(crop_time_series ~ 1, from = 0, to = 1)
    
    # Plot the time series and plot the breakpoint as a line
    plot(crop_time_series_data, type = "l", ylab = measure, xlab = "",
         main = paste(crop, measure, "in", country_var))
    lines(breakpoints(fs_crops))
    text(x=breakdates(breakpoints(fs_crops)), y=min(crop_time_series_data$value),
         pos=4, labels = paste("Break point (", breakdates(breakpoints(fs_crops)), ")", sep = ""))
    
    
    print(crop_time_series_data)
  }
  
}

land_use_plot = function(country, stacked = TRUE){
  land_use_all = as_tibble(fread("/Users/robinlindstrom/Google Drive/SRC/Thesis/x.Code/Data/Land use/land_use_all.csv")) %>% 
    clean_names()
  
  land_use_filtered = land_use_all %>% 
    filter(element == "Share in Land area") %>% 
    select(area_code, area, element, item, year, value) %>% 
    filter(area == country) %>%
    select(item, year, value)
  
  if(stacked == TRUE){
    plot = land_use_filtered %>% 
      ggplot(aes(x=year, y=value, fill=str_wrap(item, width = 15))) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste("Land use over time in", country), fill = "", x = "", y = "percent") +
      theme(legend.key.size = unit(0.5, "cm")) +
      scale_fill_brewer(palette = "Set3")
  }else{
    plot = land_use_filtered %>% 
      ggplot(aes(x=year, y=value, color=str_wrap(item, width = 15))) + 
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste("Land use over time in", country), color = "", x = "", y = "percent") +
      theme(legend.key.size = unit(0.5, "cm")) +
      scale_fill_brewer(palette = "Set3")
  }
  return(plot)
}

land_cover_plot = function(country){
  
  land_cover_all_raw = as_tibble(fread("/Users/robinlindstrom/Google Drive/SRC/Thesis/x.Code/Data/Land use/land_cover_all.csv")) %>% 
    clean_names()
  
  land_cover_all_filtered = land_cover_all_raw %>% 
    select(area_code, area, element, item, year, value) %>% 
    filter(area == country, element == "Area from MODIS") %>% 
    filter(!item %in% c("Permanent snow and glaciers")) %>% 
    na.omit()
  
  land_cover_all_filtered %>% 
    ggplot(aes(x=year, y=value, color=str_wrap(item, width = 15))) + 
    geom_line() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(angle=60, hjust=1)) +
    labs(title = paste("Land cover over time in", country), color = "", x = "", y = "Hectares") +
    theme(legend.key.size = unit(0.5, "cm")) +
    scale_fill_brewer(palette = "Set3")
}
