
#######################################################################

# Fruchterman-Reingold layout 

#######################################################################

## uweighted
### since this drawing method contains random elements, we need to set the seed so the graph is replicable
### node size is dependent on degree centrality
### alternative centrality measures can be used as well

### we use the color palette as above
# we create a palette for the first period
nb.cols <- length(unique(V(largest_unw)$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(V(largest_unw)$histcat)), "Set3"))(nb.cols)

# extra: we just want to label the five nodes with the highest degree centrality
# calculate degree centrality per node

################# visualization by degree

# text label
V(largest_unw)$degree <- degree(largest_unw, normalized = TRUE)
V(largest_unw)$label_degree <- ""
V(largest_unw)$label_degree[V(largest_unw)$degree>=0.125] <- V(largest_unw)$code[V(largest_unw)$degree>=0.125]


# node size
V(largest_unw)$size_degree <- 7
V(largest_unw)$size_degree[V(largest_unw)$degree>=0.125] <- V(largest_unw)$degree[V(largest_unw)$degree>=0.125]*70


# plot

# with all text labels

set.seed(1234)

 plot(largest_unw, edge.curved=0.2, vertex.label=V(largest_unw)$code, vertex.label.cex=1.1, vertex.size=10,
                       vertex.label.color="black",
                       layout=layout.fruchterman.reingold,
                       edge.width=1,
                       vertex.color = pal[(as.numeric(as.factor(vertex_attr(largest_unw, "histcat"))))])
legend("left",legend=sort(unique(V(largest_unw)$histcat)),
       col=pal,pt.cex=3,bty="n", pch=15,
       title="Disease categories", ncol=1, inset=c(-0.01, -0.01))



