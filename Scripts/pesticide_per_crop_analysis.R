library(raster)
library(tidyverse)

pest_path = "/Users/robinlindstrom/Downloads/PEST-CHEMGRIDS_v1_01_APR/GEOTIFF"
setwd(pest_path)
pest_files_full = as.data.frame(list.files(pest_path,pattern="*.tif$", full.names=TRUE))
pest_files = as.data.frame(list.files(pest_path,pattern="*.tif$"))

df = cbind(pest_files_full, pest_files)
colnames(df) = c("file_name_full", "file_name")


    
corn_2015_L = pest_files %>%  
  as_tibble() %>% 
  #mutate(file_name = value)
  mutate_all(as.character) %>% 
  mutate(file_name = sub('\\.tif$', '', file_name)) %>% 
  separate(col = file_name, into =  c("apr", "crop", "active_ingredient",
                                      "year", "estimate"), sep = "_") %>% 
  filter(crop == "Corn",
         year == 2015,
         estimate == "L") %>% 
  pull(file_name_full)

files = corn_2015_L

start.time <- Sys.time()
rs <- stack(files)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
rs1 <- calc(rs, sum)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pest_path = "/Users/robinlindstrom/Downloads/PEST-CHEMGRIDS_v1_01_APR/NC"
setwd(pest_path)
file = "APR_Alfalfa_Phosmet_H_L.nc"

rast = raster(file, band = 4)
plot(rast)

minValue(rast)

df = as.data.frame(rast)
df
