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

#library(sf) # For loading map data
library(tmap)    # for static and interactive maps

#established_flex_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize")
#emerging_flex_crops = c("Cassava", "Coconuts", "Rapeseed", "Sugar beet", "Sunflower seed")
#all_flex_crops = c(established_flex_crops, emerging_flex_crops)

read_data = function(){
  
  crop_production_categories = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/crop-livestock-categories.csv")) %>% 
    clean_names() %>%
  #  filter(item_group == "Crops Primary") %>% 
  #  filter(category %in% c("Cereals", "Oilseeds", "Roots and tubers", "Sugars", "Vegetable oil", "Fruits")) %>% 
    dplyr::select(flex_crop_category, item) %>%
    distinct()
  
  FAO_codes = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Categories/FAO_codes.csv")) %>%
    clean_names() %>%
    dplyr::select(country_code, iso2_code) %>% 
    rename("iso2c" = "iso2_code")
  
  # Read the production file. Rename countries with long names to shorter names.
  crop_production_data_raw = as_tibble(fread("~/Google Drive/SRC/Thesis/x.Code/Data/Crop production/Production_Crops_E_All_Data.csv")) %>% 
    clean_names() %>%
    filter(!item %in% c("Cassava leaves", "Palm kernels", "Oil, palm" )) %>% # This is just a temporary work-around
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

get_crop_data = function(crop_data_raw, crops = unique(crop_data_raw$item), measure, year){
  
  # Need y before year since it is in the columns
  year_column = paste("y",year, sep = "")
  
  # Which columns am I interested in
  selected_columns = c("country", "iso2c", "item", "measures", "flex_crop_category", year_column)
  
  # Get land area from WDI, use the latest year because I am most interested in countries existing now.
  country_land_area_raw <- data.frame(
    WDI(country = "all",
        indicator = c("AG.LND.TOTL.K2", "AG.LND.AGRI.K2", "AG.LND.CROP.ZS"),
        start = min(year),
        end = max(year),
        extra = TRUE))
  
  # Filter only countries and convert to hectares
  land_area_data = country_land_area_raw %>%
    
    # Retrieve total land area, agricultural land area nad crop land area from the World Bank
    filter(region != "Aggregates") %>% 
    rename("land_area" = "AG.LND.TOTL.K2", "agri_land_area" = "AG.LND.AGRI.K2", "crop_land_area_prop" = "AG.LND.CROP.ZS") %>%
    mutate(crop_land_area = (crop_land_area_prop / 100) * land_area) %>% # crop land comes as a proportion of total land area, this is to get the sqkm
    dplyr::select(iso2c, land_area, agri_land_area, crop_land_area, year, crop_land_area_prop)
  
  land_use_raw = as_tibble(fread("/Users/robinlindstrom/Google Drive/SRC/Thesis/x.Code/Data/Land use/land_use_cropland_agricultural_land_FAO.csv")) %>% 
    clean_names()
  
  land_use_filtered = land_use_raw %>% 
    filter(item == "Cropland",
           element == "Share in Land area") %>% 
    left_join(FAO_codes, by = c("area_code" = "country_code")) %>% 
    mutate(value = value * 0.01) %>% 
    rename(crop_area_share_of_land_area = value) %>% 
    select(area, item, year, crop_area_share_of_land_area, iso2c) %>% 
    arrange(desc(crop_area_share_of_land_area))

    # TODO This is not working correctly    
    # Values are missing in 2017, replace them with values from 2016
#    group_by(iso2c)
#    fill(crop_land_area, .direction = "down") %>%
#    fill(agri_land_area, .direction = "down")
  
  # Filter out the crops selected, remove NA and gather on year
  crop_data_out = crop_data_raw %>%
  #    dplyr::select(-land_area) %>% 
    filter(item %in% crops) %>%
    filter(measures %in% measure) %>%
    dplyr::select(selected_columns) %>%
    mutate(country = sub("C\xf4te d'Ivoire", "Cote d'Ivore", country)) %>% 
    mutate(country = sub("R\xe9union", "Reunion", country)) %>% 
    gather(year, value, -country, -iso2c, -item, -measures, -flex_crop_category) %>% 
    #mutate(value = value/1000000) %>%
    # Divide the different   
#  spread(measures, value) %>% 
#    clean_names() %>% 
#  mutate(`Area harvested` = `Area harvested` / 1000, `Production` = `Production` / 1000000, `Yield` = `Yield` / 1000) %>% 
#  gather("measures", "value", `Area harvested`, `Production`, `Yield`) %>%
    na.omit() %>% 
    mutate(year = as.numeric(gsub("y", "", year))) %>% 
    left_join(land_area_data, by = c("iso2c", "year"))
  
  return(crop_data_out)
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
    tm_polygons(col = "weight", palette = "BuPu", style = "jenks", title = "Weight (tonnes)")
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
  
  flex_crops_total_value_and_index = crop_data %>% 
    dplyr::select(year, value, measures, flex_crop_category) %>%
    group_by(year, measures, flex_crop_category) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(measures) %>%
    mutate(value_index = total_value/total_value[year == min(year)])
  
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

time_series_crop_comparison_plot_proportion = function(crop_data){
  
  flex_crops_total_value_and_index = crop_data %>% 
    dplyr::select(year, value, measures, flex_crop_category) %>%
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

crop_proportion_plot_per_country = function(crop_data, category, min_land_area, min_proportion, stacked = FALSE){

  
  crop_data_proportion = crop_data %>% 
    filter(measures == "Area harvested",
           flex_crop_category == category) %>%  
    
    # First calculate the total area for the crop group
    group_by(country, year) %>%
    mutate(crop_category_area = sum(value*0.01)) %>% 
    mutate(crop_category_area_proportion = (crop_category_area) / crop_land_area) %>% 
    ungroup() %>% 
    
    # Then calculate total area per individual crop
    group_by(country, year, item) %>%
    mutate(crop_individual_area = sum(value*0.01)) %>%
    mutate(crop_individual_area_proportion = (crop_individual_area) / crop_land_area) %>% 
    select(country, year, crop_individual_area_proportion, crop_category_area_proportion, item, crop_land_area, land_area)
  
  
  
  # Get the countries which has the largest share of area harvested in 2017
  last_year = last(crop_data_proportion$year)
  top_countries = crop_data_proportion %>% 
    filter(year == last_year) %>% 
    arrange(desc(crop_category_area_proportion)) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    top_n(1, item) %>%
    filter(land_area > min_land_area) %>% 
    filter(crop_category_area_proportion > min_proportion) %>%
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


plot_country_crop_data = function(crop_data, country_var, measure_var, n_items, year_var){
  
  # function that takes crop data, country, time period and measures and plots a stacked
  # area graph of items for each measure over the time period defined. n_items is how many
  # items should be shown in the graph, the rest is grouped into "other".
  
  plot_list = list()
  
  for(i in 1:length(measure_var)){
    plot_data = crop_data %>% 
      filter(country == country_var,
             measures %in% measure_var[i], year %in% year_var) %>% 
      select(-iso2c, -flex_crop_category) %>% 
      ungroup()
    
    plot_order = plot_data %>% 
      mutate(item = as.character(item)) %>%
      filter(year == last(year)) %>%
      arrange(desc(value)) %>% 
      mutate(rank = row_number())
    
    final_plot <- plot_data %>% 
      mutate(item = as.character(item)) %>%
      mutate(plot_label = ifelse(item %in% plot_order$item[1:n_items], item, 'Other')) %>%
      mutate(plot_label = factor(plot_label, levels = c((plot_order$item[1:n_items]), 'Other'))) %>%
      group_by(plot_label, year) %>%
      summarise(value = sum(value)) %>% 
      group_by(year) %>% 
      mutate(percentage = value / sum(value))
    
    plot_list[[i]] = final_plot %>%
      ggplot(aes(x=year, y=percentage, fill=plot_label)) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = paste(country_var, measure_var[i], sep = ": "), fill = "", x = "", y = get_measure_scale(measure_var[i])) +
      theme(legend.key.size = unit(0.2, "cm")) +
      scale_fill_brewer(palette = "Set3")
  }
  ggarrange(plotlist=plot_list)
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

#year = c("2004", "2006")
#year_column = paste("y",year, sep = "")
#crop_data = get_crop_data(crop_production_data_raw, "Maize", "Area harvested", year)

#crop_map = make_crop_data_map(crop_data)

#tm_shape(crop_map) +
#  tm_polygons(col = "weight", palette = "Set3", style = "jenks", title = "Weight (tonnes)")

#tm_shape(crop_map) +
#  tm_polygons(col = "weight", palette = "Set3", style = "jenks", title = "Weight (tonnes)") +
#  tm_facets(by = "year")

#  tm_layout("Concentration of maize production",
#            inner.margins=c(0,.1,.1,.1), title.size=1)


#tm_shape(b) + tm_polygons("weight", palette = "Set3", style = "jenks", title = "Weight (tonnes)") +
#  tm_facets(by = "year")
#  tm_facets(nrow = 1, sync = TRUE)

