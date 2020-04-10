# Read the data
source("~/Google Drive/Skola/SRC/Thesis/Code/Scripts/common_settings.R")
crop_production_data = read_csv("/Users/robinlindstrom/Google Drive/Skola/SRC/Thesis/Code/Output processed data/crop_production_data_processed.csv")

map_input_data = crop_production_data %>% 
  filter(year == 2018,
         measures == "Production",
         item %in% c("Maize", "Oil palm fruit", "Soybeans", "Sugar cane")) %>% 
  select(iso3_code, item, value)

map_input_data_spread = map_input_data %>%
  filter(iso3_code != "SDN") %>%
  spread(item, value)

map_data = make_map_data(map_input_data_spread)

# st_drop_geometry()

map_data_clean = map_data %>%
  gather(item, value, -country, -iso3_code, -geometry) %>% 
  replace_na(list(value = 0)) %>% 
  mutate(value_indicator = ifelse(value > 0, "Grown", "Not grown"))


#map_data_clean %>% 
#  tm_shape() +
#  tm_polygons("value_indicator") +
#  tm_facets(by = "item")

map_data_transformed = st_transform(map_data_clean, crs = "+proj=moll")

map_data_transformed %>% 
  mutate(value_indicator = as.factor(value_indicator),
         value_indicator = fct_relevel(value_indicator, c("Not grown", "Grown"))) %>% 
  ggplot() +
  geom_sf(aes(fill = value_indicator)) +
  facet_wrap(~item) +
  labs(fill = "", title = "Where flex crops are grown in 2018 (FAOSTAT 2020)") +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, vjust = 4))

ggsave(here("Output images", "producer_map.png"))



# Other example with really nice maps
# naturalearth world map geojson
URL <- "https://github.com/nvkelso/natural-earth-vector/blob/master/geojson/ne_50m_admin_0_countries.geojson"
fil <- basename(URL)

if (!file.exists(fil)) download.file(URL, fil)
R.utils::gunzip(fil)
world <- readOGR(dsn="ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")

# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]

world <- spTransform(world, CRS("+proj=wintri"))

dat_url <- getURL("https://gist.githubusercontent.com/hrbrmstr/7a0ddc5c0bb986314af3/raw/6a07913aded24c611a468d951af3ab3488c5b702/pop.csv")
pop <- read.csv(text=dat_url, stringsAsFactors=FALSE, header=TRUE)

map <- fortify(world, region="iso_a3")

# data frame of markers 
labs <- data.frame(lat=c(39.5, 35.50), 
                   lon=c(-98.35, 103.27), 
                   title=c("US", "China"))

# pre-project them to winkel-tripel
coordinates(labs) <-  ~lon+lat
c_labs <- as.data.frame(SpatialPointsDataFrame(spTransform(
  SpatialPoints(labs, CRS("+proj=longlat")), CRS("+proj=wintri")),
  labs@data))

gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color=NA)
gg <- gg + geom_map(data=pop, map=map, color="white", size=0.15,
                    aes(fill=log(X2013), group=Country.Code, map_id=Country.Code))
gg <- gg + geom_point(data=c_labs, aes(x=lon, y=lat), size=4)
gg <- gg + scale_fill_gradient(low="#f7fcb9", high="#31a354", name="Population by Country\n(2013, log scale)")
gg <- gg + labs(title="2013 Population")
gg <- gg + coord_equal(ratio=1)
gg <- gg + ggthemes::theme_map()
gg <- gg + theme(legend.position="bottom")
gg <- gg + theme(legend.key = element_blank())
gg <- gg + theme(plot.title=element_text(size=16))
gg


worldmap <- getMap(resolution = "high")

worldmap


