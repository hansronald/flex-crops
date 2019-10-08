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

#library(sf) # For loading map data
#library(tmap)    # for static and interactive maps

established_flex_crops = c("Soybeans", "Sugar cane", "Oil palm fruit", "Maize")
emerging_flex_crops = c("Cassava", "Coconuts", "Rapeseed", "Sugar beet", "Sunflower seed")
all_flex_crops = c(established_flex_crops, emerging_flex_crops)

crop_production_categories = as_tibble(fread("5.Data/Categories/crop-livestock-categories.csv")) %>% 
  clean_names() %>%
#  filter(item_group == "Crops Primary") %>% 
#  filter(category %in% c("Cereals", "Oilseeds", "Roots and tubers", "Sugars", "Vegetable oil", "Fruits")) %>% 
  dplyr::select(flex_crop_category, item) %>%
  distinct()

FAO_codes = as_tibble(fread("5.Data/Categories/FAO_codes.csv")) %>%
  clean_names() %>%
  dplyr::select(country_code, iso2_code)

#exclude_years = paste("y",1961:1979, sep ="")
#exclude_years_type =  paste("y",1961:1979, "f", sep ="")
# Flex crop production
crop_production_data_raw = as_tibble(fread("5.Data/Crop production/Production_Crops_E_All_Data.csv")) %>% 
  clean_names() %>%
  filter(!item %in% c("Cassava leaves", "Palm kernels", "Oil, palm" )) %>%
#  select(-exclude_years) %>% 
#  select(-exclude_years_type) %>% 
  rename("country" = "area", "country_code" = "area_code", "harvest_measure" = "element") %>% 
  left_join(FAO_codes) %>% 
  left_join(crop_production_categories) %>% 
  filter(iso2_code != "") %>% 
  filter(!is.na(flex_crop_category))
  # clean_names renames columan names with snake_case
  #clean_names() %>%
  #rename(unit_weight = unit, weight = value) %>%
  #filter(item %in% all_flex_crops)

# Replace all NA values in yield/production/area harvested with 0
#crop_production_data_raw[,8:121][is.na(crop_production_data_raw[,8:121])] = 0

world_filtered = world %>% 
  filter(name_long != "Antarctica")
  

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

get_crop_data = function(data, crops = unique(data$item), measure, year){
  
  # Need y before year since it is in the columns
  year_column = paste("y",year, sep = "")
  
  # Which columns am I interested in
  selected_columns = c("country", "iso2_code", "item", "harvest_measure", "flex_crop_category", year_column)
  
  # Filter out the crops selected, remove NA and gather on year
  data = data %>%
    mutate(country = sub("C\xf4te d'Ivoire", "Cote d'Ivore", country)) %>% 
    mutate(country = sub("R\xe9union", "Reunion", country)) %>% 
    filter(item %in% crops) %>%
    dplyr::select(selected_columns) %>%
    gather(year, value, -country, -iso2_code, -item, -harvest_measure, -flex_crop_category) %>% 
    #mutate(value = value/1000000) %>%
    # Divide the different   
    spread(harvest_measure, value) %>% 
#    clean_names() %>% 
    mutate(`Area harvested` = `Area harvested` / 1000, `Production` = `Production` / 1000000, `Yield` = `Yield` / 1000) %>% 
    gather("harvest_measure", "value", `Area harvested`, `Production`, `Yield`) %>%
    filter(harvest_measure %in% measure) %>%
    na.omit()

  data$year = as.numeric(gsub("y", "", data$year))
  
  return(data)
}

make_crop_map_data = function(data){
  # Join the map data with the crop data
  map = left_join(world_filtered %>% 
                    dplyr::select(iso_a2, geom), data, c("iso_a2"= "iso2_code"))# %>% 
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
  
  # crop_data %>%
  #   filter(harvest_measure == measure, year == production_year) %>% 
  #   mutate(country_f = as.factor(country)) %>% 
  #   group_by(item, value) %>% 
  #   arrange(item, desc(value)) %>%
  #   group_by(item) %>% 
  #   top_n(n_countries, value) %>% 
  #   #group_by(item) %>% 
  #   #mutate(country = fct_reorder(country, value)) %>% 
  #   ggplot(aes(value, country_f)) +
  #   geom_point() +
  #   labs(title = measure, y = "", x = measure) +
  #   facet_wrap(~item, scales = "free", ncol = 2)
  
  len = length(unique(crop_data$item))
  plot_data = crop_data %>%
    filter(harvest_measure == measure,
           year == production_year, flex_crop_category == category) %>% 
    group_by(item, value) %>% 
    arrange(item, desc(value)) %>%
    group_by(item) %>% 
    top_n(n_countries, value) %>%
    arrange(item, desc(value)) %>% 
    ungroup() %>% 
    mutate(rank = n_countries*len - row_number() + 1)
  
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
  #scale_color_manual(labels = function(x) str_wrap(x, width = 5))

#   crop_data %>% 
#     filter(item == crop) %>% 
#     filter(year == production_year) %>% 
#     arrange(desc(weight)) %>% 
#     head(20) %>%
#     mutate(country = fct_reorder(country, weight)) %>% 
#     ggplot(aes(weight, country)) +
#     geom_point() +
# #    scale_x_continuous(labels = comma) +
#     labs(title = paste(crop, " (", production_year, ")", sep = ""), y = "", x ="")
}

time_series_plot = function(crop_data, category, measure, scale){
  
  lbls = unique(crop_data$year)
  brks = as.Date(paste(lbls, 1, 1, sep = "-"))

  crop_data %>% 
    filter(flex_crop_category == category, harvest_measure == measure) %>% 
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
    dplyr::select(year, value, harvest_measure, flex_crop_category) %>%
    group_by(year, harvest_measure, flex_crop_category) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(harvest_measure) %>%
    mutate(value_index = total_value/total_value[year == min(year)])
  
  if(index_plot == TRUE){
    ggplot(flex_crops_total_value_and_index) +
      geom_line(aes(x=year, y=value_index, colour = str_wrap(flex_crop_category, width = 10))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(y = "index", x = "") +
      facet_wrap(~harvest_measure, ncol = 2, scales = scale) +
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
      facet_wrap(~harvest_measure, ncol = 2, scales = scale, labeller = as_labeller(hospital_names)) +
      scale_y_continuous(limits=c(0,NA)) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
#      guides(shape = guide_legend(override.aes = list(size = 0.5)))
      theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
    
  }
  
}

time_series_crop_comparison_plot = function(crop_data, category, measure, fraction){

  category
  crop_plot_data = crop_data %>% 
    filter(flex_crop_category == category, harvest_measure %in% measure) %>% 
    dplyr::select(year, value, harvest_measure, item) %>%
    group_by(year, harvest_measure, item) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(harvest_measure) %>%
    mutate(value_index = total_value / total_value) %>% 
    #print()
    group_by(harvest_measure, item) %>% 
    mutate(value_index = total_value/total_value[year == min(unique(year))]) %>% 
    group_by(harvest_measure, year) %>%
    mutate(fraction_value = total_value / sum(total_value))
  
  #  dplyr::select(-total_value)
  if(fraction == TRUE){
    ggplot(crop_plot_data) +
      geom_line(aes(x=year, y=fraction_value, colour = str_wrap(item, width = 8))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#      geom_vline(xintercept = 2009, linetype = "dotted", color = "black") +
      labs(y = "index", x = "") +
      facet_wrap(~harvest_measure, nrow = 2, scales = "free_y") +
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
        facet_wrap(~harvest_measure, nrow = 2, scales = "free_y") +
        scale_y_continuous(limits=c(0,NA)) +
        theme(axis.text.x = element_text(angle=60, hjust=1)) +
#        guides(shape = guide_legend(override.aes = list(size = 0.5)))
        theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank())
  }
}

time_series_crop_comparison_plot_proportion = function(crop_data){
  
  flex_crops_total_value_and_index = crop_data %>% 
    dplyr::select(year, value, harvest_measure, flex_crop_category) %>%
    group_by(year, harvest_measure, flex_crop_category) %>% 
    summarize(total_value = sum(value)) %>% 
    group_by(harvest_measure) %>% 
    mutate(value_index = total_value/total_value[year == min(year)]) %>% 
    group_by(year, harvest_measure) %>% 
    mutate(total_value_all = sum(total_value)) %>% 
    mutate(fraction_value = total_value/total_value_all)
  # mutate(flex_fraction = total_value[flex_crop_category == "Flex crop"] / sum(total_value))
  
  ggplot(flex_crops_total_value_and_index) +
    geom_line(aes(x=year, y=fraction_value, colour = str_wrap(flex_crop_category, width = 10))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(y = "proportion", x = "") +
    facet_wrap(~harvest_measure, ncol = 2, scales = "free_y") +
    scale_y_continuous(limits=c(0,NA)) +
    theme(legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = 6), legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
  #filter(flex_crop_category != "Non flex crop")
}


break_point_plot = function(crop_data, measure, crops, y_axis_text){

  flex_production = crop_data %>% 
    mutate(item = as.factor(item)) %>% 
    filter(item %in% crops, harvest_measure == measure) %>%
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
    filter(item %in% crops, harvest_measure == measure) %>%
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
           harvest_measure == measure) %>%  
    #         year %in% 1960:1970,
    #         country %in% unique(crop_data$country)[50:70]) %>% 
    select(-iso2_code, -flex_crop_category, -harvest_measure, -item)
  
  plot_order = plot_data %>% 
    mutate(country = as.character(country)) %>%
    filter(year == last(year)) %>% 
    arrange(desc(value)) %>% 
    mutate(rank = row_number())
  
  final_plot <- plot_data %>% 
    mutate(country = as.character(country)) %>%
    mutate(plot_label = ifelse(country %in% plot_order$country[1:n_countries], country, 'Other')) %>%
    mutate(plot_label = factor(plot_label, levels = c(rev(plot_order$country[1:n_countries]), 'Other'))) %>%
    group_by(plot_label, year) %>%
    summarise(value = sum(value)) %>% 
    group_by(year) %>% 
    mutate(percentage = value / sum(value))
  
  if(proportion == TRUE){
    final_plot %>%
      ggplot(aes(x=year, y=percentage, fill=str_wrap(plot_label, width = 10))) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = paste(crop, measure, sep = ": "), fill = "", x = "", y = "proportional") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 6))
      #guides(shape = guide_legend(override.aes = list(size = 0.2)))
      #theme(legend.text = element_text(function(x) str_wrap(x, width = 5))) +
      #scale_color_manual(labels = function(x) str_wrap(x, width = 5))
  }else{
    final_plot %>%
      ggplot(aes(x=year, y=value, fill=str_wrap(plot_label, width = 10))) + 
      geom_area() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = paste(crop, measure, sep = ": "), fill = "", x = "", y = y_axis_text) +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      theme(legend.key.size = unit(0.2, "cm"), legend.text = element_text(size = 6))
    #guides(shape = guide_legend(override.aes = list(size = 0.2)))
    #theme(legend.text = element_text(function(x) str_wrap(x, width = 5))) +
    #scale_color_manual(labels = function(x) str_wrap(x, width = 5))
  }
}

plot_HH_index = function(crop_data, category, measure){
  
  HH_index_data = crop_data %>%
    filter(flex_crop_category == category, harvest_measure %in% measure) %>% 
    group_by(item, harvest_measure, year) %>% 
    mutate(HH_index = value / sum(value)) %>% 
    arrange(desc(HH_index)) %>% 
    summarise_at(vars(HH_index), function(x){return(sum((x*100)^2))})
  
  HH_index_data %>%
    ggplot(aes(x = year, y = HH_index, color = item)) +
    geom_line() +
    geom_line(aes(y=1500), color = "black", linetype = "dashed", size = 0.5) +
    geom_line(aes(y=2500), color = "black", linetype = "dashed", size = 0.5) +
#    geom_hline(aes(yintercept=1500), color = "red", linetyp = "dashed") +
    facet_wrap(~harvest_measure, ncol = 2, scale = "free_y") +
    labs(y = "Herfindahl-Hirschman Index") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
    theme(axis.text.x = element_text(angle=60, hjust=1))

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

