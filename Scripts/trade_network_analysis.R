library(ggraph)
library(igraph)

# Total flex crop trade network
flex_crop_total_trade_network = flex_crop_total_trade %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3, weight = total_trade) %>% 
  filter(weight > 1e6)
  
sources <- flex_crop_total_trade_network %>%
  distinct(source) %>%
  rename(label = source)

destinations <- flex_crop_total_trade_network %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route = flex_crop_total_trade_network
per_route


edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)



# Network
# library(network)
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 1)
plot(routes_network, vertex.cex = 1, mode = "circle")

# Igraph
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(routes_igraph, edge.arrow.size = 0.2)

# ggraph doesnt 
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges %>% mutate(weight = log(weight)), directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

#ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

# GGrapg - this plot could actually be quite nice with some modifications
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label, color = "tomato"), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

graph = routes_igraph

graph <- graph_from_data_frame(highschool)

V(graph)$friends <- degree(graph, mode = 'in')
V(graph)$friends <- ifelse(V(graph)$friends < 5, 'few', 
                           ifelse(V(graph)$friends >= 15, 'many', 'medium'))

# Not working
#ggraph(graph, 'hive', axis = 'friends', sort.by = 'degree') + 
#  geom_edge_hive(aes(colour = factor(year), alpha = ..index..)) + 
#  geom_axis_hive(aes(colour = friends), size = 3, label = FALSE) + 
#  coord_fixed()


#ggraph(routes_igraph, layout = 'dendrogram', circular = TRUE) + 
#  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
#  coord_fixed()

# Sankey
library(networkD3)


nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

  
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")


## Soybean trading ---

soybean_total_trade_network = soybean_total_trade %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3, weight = total_trade) %>% 
  filter(weight > 1e6)

# Create the network data

sources <- soybean_total_trade_network %>%
  distinct(source) %>%
  rename(label = source)

destinations <- soybean_total_trade_network %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route = soybean_total_trade_network
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges




