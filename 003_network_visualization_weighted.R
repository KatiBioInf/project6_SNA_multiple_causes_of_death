<<<<<<< HEAD


#######################################################################

# CIRCULAR LAYOUT

#######################################################################

nb.cols <- length(unique(V(largest_w)$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(V(largest_w)$histcat)), "Set3"))(nb.cols)

# plot

plot(largest_w, edge.curved=0.2, vertex.label=V(largest_w)$code, vertex.label.cex=1.3, vertex.size=14,
                       vertex.label.color="black",
                       edge.width=log(E(largest_w)$weight/3),  layout=layout_in_circle,
                       vertex.color = pal[(as.numeric(as.factor(vertex_attr(largest_w, "histcat"))))])
legend("left",legend=sort(unique(V(largest_w)$histcat)),
       col=pal,pt.cex=3,bty="n", pch=15,
       title="Disease categories", ncol=1, inset=c(-0.02,-0.01))




#######################################################################

# Fruchterman-Reingold layout 

#######################################################################


## weighted
### since this drawing method contains random elements, we need to set the seed so the graph is replicable
### node size is dependent on degree centrality
### alternative centrality measures can be used as well

### we use the color palette as above
# we create a palette for the first period
nb.cols <- length(unique(V(largest_w)$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(V(largest_w)$histcat)), "Set3"))(nb.cols)

# extra: we just want to label the five nodes with the highest degree centrality
# calculate degree centrality per node

################# visualization by degree

# text label
V(largest_w)$degree <- degree(largest_w, normalized = TRUE)
V(largest_w)$label_degree <- ""
V(largest_w)$label_degree[V(largest_w)$degree>=0.125] <- V(largest_w)$code[V(largest_w)$degree>=0.125]


# node size
V(largest_w)$size_degree <- 7
V(largest_w)$size_degree[V(largest_w)$degree>=0.125] <- V(largest_w)$degree[V(largest_w)$degree>=0.125]*70

# edge color

E(largest_w)$color[E(largest_w)$weight >= median(E(largest_w)$weight)] <- 'black'
E(largest_w)$color[E(largest_w)$weight < median(E(largest_w)$weight)] <- 'grey80'


# plot

# with all text labels

set.seed(1234)

plot(largest_w, edge.curved=0.2, vertex.label=V(largest_w)$code, vertex.label.cex=1, vertex.size=10,
                     vertex.label.color="black",
                     layout=layout.fruchterman.reingold,
                     edge.width=E(largest_w)$weight/10,
                     vertex.color = pal[(as.numeric(as.factor(vertex_attr(largest_w, "histcat"))))])
legend("left",legend=sort(unique(V(largest_w)$histcat)),
       col=pal,pt.cex=4,bty="n", pch=15,
       title="Disease categories", ncol=1, inset=c(-0.25, -0.25))


=======


#######################################################################

# CIRCULAR LAYOUT

#######################################################################

nb.cols <- length(unique(V(largest_w)$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(V(largest_w)$histcat)), "Set3"))(nb.cols)

# plot

plot(largest_w, edge.curved=0.2, vertex.label=V(largest_w)$code, vertex.label.cex=1.3, vertex.size=14,
                       vertex.label.color="black",
                       edge.width=log(E(largest_w)$weight/3),  layout=layout_in_circle,
                       vertex.color = pal[(as.numeric(as.factor(vertex_attr(largest_w, "histcat"))))])
legend("left",legend=sort(unique(V(largest_w)$histcat)),
       col=pal,pt.cex=3,bty="n", pch=15,
       title="Disease categories", ncol=1, inset=c(-0.02,-0.01))




#######################################################################

# Fruchterman-Reingold layout 

#######################################################################


## weighted
### since this drawing method contains random elements, we need to set the seed so the graph is replicable
### node size is dependent on degree centrality
### alternative centrality measures can be used as well

### we use the color palette as above
# we create a palette for the first period
nb.cols <- length(unique(V(largest_w)$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(V(largest_w)$histcat)), "Set3"))(nb.cols)

# extra: we just want to label the five nodes with the highest degree centrality
# calculate degree centrality per node

################# visualization by degree

# text label
V(largest_w)$degree <- degree(largest_w, normalized = TRUE)
V(largest_w)$label_degree <- ""
V(largest_w)$label_degree[V(largest_w)$degree>=0.125] <- V(largest_w)$code[V(largest_w)$degree>=0.125]


# node size
V(largest_w)$size_degree <- 7
V(largest_w)$size_degree[V(largest_w)$degree>=0.125] <- V(largest_w)$degree[V(largest_w)$degree>=0.125]*70

# edge color

E(largest_w)$color[E(largest_w)$weight >= median(E(largest_w)$weight)] <- 'black'
E(largest_w)$color[E(largest_w)$weight < median(E(largest_w)$weight)] <- 'grey80'


# plot

# with all text labels

set.seed(1234)

plot(largest_w, edge.curved=0.2, vertex.label=V(largest_w)$code, vertex.label.cex=1, vertex.size=10,
                     vertex.label.color="black",
                     layout=layout.fruchterman.reingold,
                     edge.width=E(largest_w)$weight/10,
                     vertex.color = pal[(as.numeric(as.factor(vertex_attr(largest_w, "histcat"))))])
legend("left",legend=sort(unique(V(largest_w)$histcat)),
       col=pal,pt.cex=4,bty="n", pch=15,
       title="Disease categories", ncol=1, inset=c(-0.25, -0.25))


>>>>>>> e6a43197eb614a8f29e816fa8ac755b8305febe7
