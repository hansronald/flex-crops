
library(raster)
library(here)
library(tidyverse)

raster_id = "oilpalm_Production.tif"
raster_id = "sugarcane_Production.tif"
rast <- raster(here("Data", "Raster", raster_id))

#rast.df <- as.data.frame(rast, xy = TRUE)

# rast.df %>% 
#   ggplot() +
#   geom_raster(aes(x = x, y = y))
# 
# plot(rast, legend=FALSE, axes=FALSE, box=FALSE)

library(tmap)
#data(World)
#data(land)
tm_shape(rast) +
  tm_raster("sugarcane_Production", title = "Global ") +
  tm_layout(legend.show = FALSE)
