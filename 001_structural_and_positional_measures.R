<<<<<<< HEAD
# this R script calculates all structural and positional measures on the largest component
# of both the weighted and unweighted networks

library(tidyverse)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(cowplot)
library(Hmisc)
library(GGally)
library(gridExtra)
library(ggrepel)
library(writexl)
library(forcats)
library(ggalluvial)

#######################################################################

# 1. IMPORT DATA

#######################################################################

# period 1

#### nodes

nodes <- readRDS("nodes_sample_data.rds")

#### edges

##### weighted
edges <- readRDS("edges_sample_data.rds")
edges_w <- edges

##### unweighted
edges_unw <- edges_w %>% 
  select(cause1, cause2)

#######################################################################

# 2. BUILD NETWORKS

#######################################################################


# weighted network
network_w <- graph_from_data_frame(d=edges_w, vertices=nodes, directed=F) 

# unweighted network
network_unw <- graph_from_data_frame(d=edges_unw, vertices=nodes, directed=F)

# checking weightedness
is_weighted(network_w)
is_weighted(network_unw)


#######################################################################

# 3. KEEPING ONLY LARGEST COMPONENTS

#######################################################################


largest_w <- igraph::induced_subgraph(network_w, with(components(network_w), membership == 1))
largest_unw <- igraph::induced_subgraph(network_unw, with(components(network_unw), membership == 1))


##########################################################################

# 4. STRUCTURAL PROPERTIES - Largest component only

############################################################################

###### the number of edges
# no difference between weighted and unweighted

gsize(largest_unw)

###### largest summary

summary(largest_unw)
summary(largest_w)

##### density
# no difference between weighted and unweighted
edge_density(largest_unw)
edge_density(largest_w)

###### components
# no difference between weighted and unweighted

components(largest_unw)

###### diameter

diameter(largest_unw)
diameter(largest_w)


##### mean distance

mean_distance(largest_unw)
mean_distance(largest_w)


##########################################################################

# 5. POSITIONAL PROPERTIES - LARGEST COMPONENT

############################################################################

##### computing measures for unweighted network: degree, closeness, betweenness


V(largest_unw)$degree <- degree(largest_unw, normalized = TRUE)
V(largest_unw)$closeness_unw <- closeness(largest_unw, normalized = TRUE)
V(largest_unw)$betweenness_unw <- betweenness(largest_unw, normalized = TRUE)

measures_unw <- as.data.frame(cbind(V(largest_unw)$code, 
                                            V(largest_unw)$degree,
                                            V(largest_unw)$closeness_unw,
                                            V(largest_unw)$betweenness_unw,
                                            V(largest_unw)$histcat))

colnames(measures_unw) <- c("code", "degree", "closeness_unw", "betweenness_unw", "histcat")

measures_unw$degree <- as.numeric(measures_unw$degree)
measures_unw$closeness_unw <- as.numeric(measures_unw$closeness_unw)
measures_unw$betweenness_unw <- as.numeric(measures_unw$betweenness_unw)


##### computing measures for weighted network: degree, closeness, betweenness

V(largest_w)$closeness_w <- closeness(largest_w, normalized = TRUE)
V(largest_w)$betweenness_w <- betweenness(largest_w, normalized = TRUE)

measures_w <- as.data.frame(cbind(V(largest_w)$code, 
                                          V(largest_w)$closeness_w,
                                          V(largest_w)$betweenness_w))

colnames(measures_w) <- c("code", "closeness_w", "betweenness_w")

measures_w$closeness_w <- as.numeric(measures_w$closeness_w)
measures_w$betweenness_w <- as.numeric(measures_w$betweenness_w)

MEASURES <- merge(measures_unw, measures_w, by="code")



#######################################################################################

# scatter plots

########################################################################################

# weighted


scatter_closeness_betweenness_w <- ggplot(MEASURES, aes(x=closeness_w, y=betweenness_w)) + 
  geom_jitter(aes(size=degree))+
  geom_text_repel(aes(label = code), size=4, max.overlaps = 10)+
    theme_light()+
  xlab("closeness (weighted)")+
  ylab("betweenness (weighted)")+
  theme(text = element_text(size = 14),
        legend.text=element_text(size=14))+
  ggtitle("Visualizing centrality measures")
scatter_closeness_betweenness_w

ggsave("figures/scatter_weighted.tiff", plot=last_plot(), width=2700, height=2900, units="px", dpi=300)

# unweighted


scatter_closeness_betweenness_unw <- ggplot(MEASURES, aes(x=closeness_unw, y=betweenness_unw)) + 
  geom_jitter(aes(size=degree))+
  geom_text_repel(aes(label = code), size=4, max.overlaps = 10)+
  theme_light()+
  xlab("closeness (weighted)")+
  ylab("betweenness (weighted)")+
  theme(text = element_text(size = 14),
        legend.text=element_text(size=14))+
  ggtitle("Visualizing centrality measures")
scatter_closeness_betweenness_unw

ggsave("figures/scatter_unweighted.tiff", plot=last_plot(), width=2700, height=2900, units="px", dpi=300)


############################################ ranking 

MEASURES <- MEASURES %>% 
  dplyr::mutate(rank_degree=dense_rank(dplyr::desc(degree)),
         rank_closeness_unw=dense_rank(dplyr::desc(closeness_unw)),
         rank_betweenness_unw=dense_rank(dplyr::desc(betweenness_unw)),
         rank_closeness_w=dense_rank(dplyr::desc(closeness_w)),
         rank_betweenness_w=dense_rank(dplyr::desc(betweenness_w))
  )





=======
# this R script calculates all structural and positional measures on the largest component
# of both the weighted and unweighted networks

library(tidyverse)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(cowplot)
library(Hmisc)
library(GGally)
library(gridExtra)
library(ggrepel)
library(writexl)
library(forcats)
library(ggalluvial)

#######################################################################

# 1. IMPORT DATA

#######################################################################

# period 1

#### nodes

nodes <- readRDS("nodes_sample_data.rds")

#### edges

##### weighted
edges <- readRDS("edges_sample_data.rds")
edges_w <- edges

##### unweighted
edges_unw <- edges_w %>% 
  select(cause1, cause2)

#######################################################################

# 2. BUILD NETWORKS

#######################################################################


# weighted network
network_w <- graph_from_data_frame(d=edges_w, vertices=nodes, directed=F) 

# unweighted network
network_unw <- graph_from_data_frame(d=edges_unw, vertices=nodes, directed=F)

# checking weightedness
is_weighted(network_w)
is_weighted(network_unw)


#######################################################################

# 3. KEEPING ONLY LARGEST COMPONENTS

#######################################################################


largest_w <- igraph::induced_subgraph(network_w, with(components(network_w), membership == 1))
largest_unw <- igraph::induced_subgraph(network_unw, with(components(network_unw), membership == 1))


##########################################################################

# 4. STRUCTURAL PROPERTIES - Largest component only

############################################################################

###### the number of edges
# no difference between weighted and unweighted

gsize(largest_unw)

###### largest summary

summary(largest_unw)
summary(largest_w)

##### density
# no difference between weighted and unweighted
edge_density(largest_unw)
edge_density(largest_w)

###### components
# no difference between weighted and unweighted

components(largest_unw)

###### diameter

diameter(largest_unw)
diameter(largest_w)


##### mean distance

mean_distance(largest_unw)
mean_distance(largest_w)


##########################################################################

# 5. POSITIONAL PROPERTIES - LARGEST COMPONENT

############################################################################

##### computing measures for unweighted network: degree, closeness, betweenness


V(largest_unw)$degree <- degree(largest_unw, normalized = TRUE)
V(largest_unw)$closeness_unw <- closeness(largest_unw, normalized = TRUE)
V(largest_unw)$betweenness_unw <- betweenness(largest_unw, normalized = TRUE)

measures_unw <- as.data.frame(cbind(V(largest_unw)$code, 
                                            V(largest_unw)$degree,
                                            V(largest_unw)$closeness_unw,
                                            V(largest_unw)$betweenness_unw,
                                            V(largest_unw)$histcat))

colnames(measures_unw) <- c("code", "degree", "closeness_unw", "betweenness_unw", "histcat")

measures_unw$degree <- as.numeric(measures_unw$degree)
measures_unw$closeness_unw <- as.numeric(measures_unw$closeness_unw)
measures_unw$betweenness_unw <- as.numeric(measures_unw$betweenness_unw)


##### computing measures for weighted network: degree, closeness, betweenness

V(largest_w)$closeness_w <- closeness(largest_w, normalized = TRUE)
V(largest_w)$betweenness_w <- betweenness(largest_w, normalized = TRUE)

measures_w <- as.data.frame(cbind(V(largest_w)$code, 
                                          V(largest_w)$closeness_w,
                                          V(largest_w)$betweenness_w))

colnames(measures_w) <- c("code", "closeness_w", "betweenness_w")

measures_w$closeness_w <- as.numeric(measures_w$closeness_w)
measures_w$betweenness_w <- as.numeric(measures_w$betweenness_w)

MEASURES <- merge(measures_unw, measures_w, by="code")



#######################################################################################

# scatter plots

########################################################################################

# weighted


scatter_closeness_betweenness_w <- ggplot(MEASURES, aes(x=closeness_w, y=betweenness_w)) + 
  geom_jitter(aes(size=degree))+
  geom_text_repel(aes(label = code), size=4, max.overlaps = 10)+
    theme_light()+
  xlab("closeness (weighted)")+
  ylab("betweenness (weighted)")+
  theme(text = element_text(size = 14),
        legend.text=element_text(size=14))+
  ggtitle("Visualizing centrality measures")
scatter_closeness_betweenness_w

ggsave("figures/scatter_weighted.tiff", plot=last_plot(), width=2700, height=2900, units="px", dpi=300)

# unweighted


scatter_closeness_betweenness_unw <- ggplot(MEASURES, aes(x=closeness_unw, y=betweenness_unw)) + 
  geom_jitter(aes(size=degree))+
  geom_text_repel(aes(label = code), size=4, max.overlaps = 10)+
  theme_light()+
  xlab("closeness (weighted)")+
  ylab("betweenness (weighted)")+
  theme(text = element_text(size = 14),
        legend.text=element_text(size=14))+
  ggtitle("Visualizing centrality measures")
scatter_closeness_betweenness_unw

ggsave("figures/scatter_unweighted.tiff", plot=last_plot(), width=2700, height=2900, units="px", dpi=300)


############################################ ranking 

MEASURES <- MEASURES %>% 
  dplyr::mutate(rank_degree=dense_rank(dplyr::desc(degree)),
         rank_closeness_unw=dense_rank(dplyr::desc(closeness_unw)),
         rank_betweenness_unw=dense_rank(dplyr::desc(betweenness_unw)),
         rank_closeness_w=dense_rank(dplyr::desc(closeness_w)),
         rank_betweenness_w=dense_rank(dplyr::desc(betweenness_w))
  )





>>>>>>> e6a43197eb614a8f29e816fa8ac755b8305febe7
