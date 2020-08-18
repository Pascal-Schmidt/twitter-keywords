library(magrittr)

network_graph <- function(network) {
  
  # Store the degree.
  V(network)$degree <- strength(graph = network)
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
  # Create networkD3 object.
  network.D3 <- igraph_to_networkD3(g = network)
  # Define node size.
  network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
  # Degine color group (I will explore this feature later).
  network.D3$nodes %<>% mutate(Group = 1)
  # Define edges width. 
  network.D3$links$Width <- 10*E(network)$width
  
  forceNetwork(
    Links = network.D3$links, 
    Nodes = network.D3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'Group', 
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree', 
    # We input a JavaScript function.
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  ) -> text_network
  
  return(text_network)
  
}