library(raster)
library(rgdal)
library(maptools)
library(rnaturalearth)
library(tidyverse)
library(here)

data(wrld_simpl)
world_data = data.frame(wrld_simpl$NAME, wrld_simpl$ISO3)

#raster_files_path = "/Users/robinlindstrom/Downloads/PEST-CHEMGRIDS_v1_01_APR/GEOTIFF/Low/2015"
#setwd(raster_files_path)

raster_files = list.files(here("GEOTIFF"))

len = length(raster_files)
for(i in 1:1){
  start.time <- Sys.time()
  rast_id = raster_files[i]
  rast <- raster(rast_id)
  
  # Print file and number of file
  print(i)
  print(rast_id)
  
  # Remove .TIFF
  col_name = gsub("\\..*","",rast_id)
  
  # # Split the file name at "_" and them make it into vector
  # split_file_name = tolower(unlist(strsplit(file_name, "_")))
  # 
  # rast_year = split_file_name[4]
  # split_file_name_noyear = split_file_name[c(-1, -4, -5)]
  # 
  # # Concatenate the vectors with a "_" again.
  # col_name = paste0(split_file_name_noyear, collapse = "_")
  
  # Remove all the negative values of the raster
  rast_positive = rast
  rast_positive <- clamp(rast, lower=0, useValues=FALSE)
  
  # start.time <- Sys.time()
  # rast.new[rast.new < 0] = NA
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # time.taken
  # 
  # start.time <- Sys.time()  
  # rast.new2 <- reclassify(rast, cbind(-Inf, 0, NA), right=FALSE)
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # time.taken

  # Multiply the kg/ha with the area of each cell to get the total kgs
  
  start.time <- Sys.time()
  rast_total_pesticide_application = rast_positive * area(rast_positive) / 100
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  # Get the mean applcation rate (kg/ha) for each country
  mean_application_rate_extract = raster::extract(rast_positive, wrld_simpl, fun = mean, na.rm = TRUE) # sp = T for keeping original dataframe
  
  # Get the total applcation rate (kg/ha) for each country
  total_application_extract = raster::extract(rast_total_pesticide_application, wrld_simpl, fun = sum, na.rm = TRUE)
  
  # plot(rast)
  
  # Put the mean pesticide use per country in a dataframe and name the column after the pesticide
  # NaNs created because all raster cells are NA in country polygon, replace with 0
  mean_application_rate_extract[is.nan(mean_application_rate_extract)] = 0
  mean_application_rate = data.frame(mean_application_rate_extract)
  colnames(mean_application_rate) = paste0(col_name, "_mean")
  
  total_application = data.frame(total_application_extract)
  colnames(total_application) = paste0(col_name, "_tot")
  
  # Check which country has the most
  #r.vals %>% 
  #  as_tibble() %>% 
  #  arrange(desc(V1))
  
  world_data = data.frame(world_data, mean_application_rate, total_application)
  
  # End time taking
  end.time <- Sys.time()
  
  # Extract the minutes it took for this file and print
  time.taken <- as.numeric(end.time - start.time, units = "mins")
  print(paste("This file took", round(time.taken, 1), "mins"))
  print(paste(i, "of", len, "files done"))
  
  # Calculate total file size left and see speed of this round, to estimate time left
  total_file_size_left = sum(file.info(raster_files[i:len])[1])
  file_size = file.info(rast_id)$size
  mins_left = round(total_file_size_left / (file_size / calc_time))
  print(paste("Minutes left:", mins_left))
  
  if(i%%10 == 0){
    write_csv(world_data, here("data", paste0("world_data", i/10, ".csv")))
  }
  
}

# Plot data -------------
#plot(rast)

#names(rast)

#summary(rast)

raster_files_path = "/Users/robinlindstrom/Downloads/PEST-CHEMGRIDS_v1_01_APR/GEOTIFF/High/APR_Corn_Glyphosate_2015_H.tif"
rast <- raster(raster_files_path)

rast.df <- as.data.frame(rast, xy = TRUE)
names(rast.df)

rast.df %>% 
  as_tibble() %>% 
  count(layer) %>% 
  arrange((n))

ggplot() +
  geom_raster(data = rast.df , aes(x = x, y = y, fill = cut(APR_Corn_Glyphosate_2015_H, c(-Inf, -2, -1.5, -1, -0.5, 0, Inf)))) +
  scale_color_manual(name = "APR_Corn_Glyphosate_2015_H",
                     values = c("(-Inf, -2]" = "black",
                                "(-2, -1.5]" = "yellow",
                                "(-1.5, -1]" = "red",
                                "(-1, 0]" = "green",
                                "(0, Inf]" = "blue"),
                     labels = c("-2", "-1.5", "-1", "0", ">0")) +
  labs(fill = "") +
  coord_quickmap()

# -1 = B/NA
# -1.5 = No data
# -2 = Water 
