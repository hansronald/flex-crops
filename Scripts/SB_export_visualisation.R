

# Total flex crop trade network

soybean_exports %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3, weight = export_quantity) %>% 
  group_by(source) %>% 
  summarise(total_exports = sum(weight))


soybean_exports_network = soybean_exports %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3, weight = export_quantity)
  filter(weight > 1e6)

sources <- soybean_exports_network %>%
  distinct(source) %>%
  rename(label = source)

destinations <- flex_crop_total_trade_network %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column("id")
per_route = flex_crop_total_trade_network

# Edges
edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)


#per_route
#nodes
#edges

# Sankey
#library(networkD3)

USA_id = nodes_d3 %>% filter(label == "USA") %>% pull(id)


edges_d3 <- mutate(edges, from = from - 1, to = to - 1) %>%
  filter(!is.na(from)) %>% 
  filter(from == USA_id)

inluded_destinations = edges_d3$to

nodes_d3 <- mutate(nodes, id = id - 1) %>%
  filter(id %in% inluded_destinations)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

### Alluvial
library(ggalluvial)

soybean_exports_network %>% 
  group_by(source) %>% 
  summarise(total_exports = sum(weight))

soybean_exports_network %>% 
  filter(source %in% c("BRA", "USA")) %>% 
  ggplot(aes(y = weight, axis1 = source, axis2 = destination)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Source", "Destination"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("US soybean exports in 2016 (tonnes)")


