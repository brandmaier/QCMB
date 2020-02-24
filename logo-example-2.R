set.seed(324509)

edgelist_matrix <- matrix(nrow=20,ncol=2)

MAX <- 10

for (i in 1:nrow(edgelist_matrix)) {
  a <- runif(MAX)
  b <- a+2
  a <- min(max(a,0),MAX)
  b <- min(max(b,0),MAX)
  a <- paste0("A",a)
  b <- paste0("A",b)
  edgelist_matrix[i,] <- c(a,b)
}

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



# w/o letters
V(got_graph)$label<-""
png("./logos/002-logo.png")
l2 <- layout_with_mds(got_graph)
plot(got_graph, layout=l2, label.cex=20, vertex.label.color="white",size=20, vertex.label.cex=2.5,
     vertex.size=15)
dev.off()


plot(got_graph, layout=l2, label.cex=20, vertex.label.color="white",size=20, vertex.label.cex=2.5,
     vertex.size=15)
