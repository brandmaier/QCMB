edgelist_matrix <- cbind(c("A","A","B","A", "A2","B2","C2","D2","E2", "E2","E2","E2","F2","G2","A2","E2","E2","H3"), 
                         c("B","C","C","A2","B2","C2","A2","A2","B2", "A2","A", "B2","E2","B2","H2","G3","A2","A2"))


edgelist_matrix <- cbind(c("A","A","B","B", "A2","B2","C2","D2","E2", "E2","E2","E2","F2","G4","A2","E4","B2","C2"), 
                         c("B","C","C","A2","B2","C2","A2","A2","B2", "F2","G2", "B","B4","B4","G4","G4","G4","A"))

edgelist_matrix <- cbind(c("A1","A1","B1","B1", "A2","B2","C2","D2","E2", "E2","E2","E2","F2","G4","A2","E4","B2","C2"), 
                         c("B1","C1","V1","A2","B2", "C2","A2","A2","B2", "F2","G2", "B1","B4","B4","G4","G4","G4","A1"))


got_graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
  igraph::set.edge.attribute("weight", value = 1)



# run louvain with edge weights

louvain_partition <- igraph::cluster_louvain(got_graph, weights = E(got_graph)$weight)

got_graph$community <- louvain_partition$membership

sizes(louvain_partition) %>% 
  knitr::kable()

# small communities

membership(louvain_partition)[which(membership(louvain_partition) %in% c(6,7))] 


# high degree


high_degree_nodes <- c()

for (i in 1:8) {
  subgraph <- induced_subgraph(got_graph, v = which(got_graph$community == i))
  degree <-  igraph::degree(subgraph)
  high_degree_nodes[i] <- names(which(degree == max(degree)))
}

high_degree_nodes[c(1:5, 8)]


# compute betweeness
high_btwn_nodes <- c()

for (i in 1:8) {
  subgraph <- induced_subgraph(got_graph, v = which(got_graph$community == i))
  btwn <-  igraph::betweenness(subgraph)
  high_btwn_nodes[i] <- names(which(btwn == max(btwn)))
}

high_btwn_nodes[c(1:5, 8)]

# give our nodes some properties, incl scaling them by degree and coloring them by community

V(got_graph)$size <- degree(got_graph)*10
V(got_graph)$frame.color <- "white"
V(got_graph)$color <- got_graph$community
V(got_graph)$label <- V(got_graph)$name

# also solor edges according to their starting node
edge.start <- ends(got_graph, es = E(got_graph), names = F)[,1]
E(got_graph)$color <- V(got_graph)$color[edge.start]
E(got_graph)$arrow.mode <- 0

# only label central characters

v_labels <- which(V(got_graph)$name %in% high_degree_nodes[c(1:5, 8)])
#v_labels <- NULL

for (i in 1:length(V(got_graph))) {
  if (!(i %in% v_labels)) {
  #  V(got_graph)$label[i] <- ""
  }
}

V(got_graph)$label[V(got_graph)$label=="A2"]<-"Q"
V(got_graph)$label[V(got_graph)$label=="B2"]<-"C"
V(got_graph)$label[V(got_graph)$label=="B1"]<-"M"
V(got_graph)$label[V(got_graph)$label=="E2"]<-"B"

V(got_graph)$label[!V(got_graph)$label %in% c("Q","C","M","B")]<-""


#par(mfrow=c(1,2))

l1 <- layout_on_sphere(got_graph)
plot(got_graph, rescale = F, layout = l1)


png("./logos/qcmb.png")
l2 <- layout_with_mds(got_graph)
plot(got_graph, layout=l2, label.cex=20, vertex.label.color="white",size=20, vertex.label.cex=2.5)
dev.off()

# w/o letters
V(got_graph)$label<-""
png("./logos/qcmb-noletter.png")
l2 <- layout_with_mds(got_graph)
plot(got_graph, layout=l2, label.cex=20, vertex.label.color="white",size=20, vertex.label.cex=2.5)
dev.off()
