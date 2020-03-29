library(ggraph)
library(igraph)
graph <- graph_from_data_frame(highschool)


flex_crop_total_trade_network = flex_crop_total_trade %>% 
  rename(source = reporter_country_iso3, destination = partner_country_iso3, weight = total_trade) %>% 
  filter(weight > 1000)
  
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
edges

edges_filtered = edges %>% 
  filter(weight > 10000) 
  ggplot(aes(x = weight)) +
  geom_histogram(bins = 100)

# Network
#library(network)
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

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()


# Sankey
#library(networkD3)

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")
