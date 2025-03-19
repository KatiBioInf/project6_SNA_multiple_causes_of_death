#######################################################################

# LOUVAIN ALGORITHM

#######################################################################

# based on unweighted network

set.seed(1234)
cluster1_unw = cluster_louvain(largest_unw)
modularity(cluster1_unw)

sizes(cluster1_unw)


# visualization 1

set.seed(1234)
plot(cluster1_unw,largest_unw, vertex.label=V(largest_unw)$code, 
     vertex.label.cex=1.3, vertex.size=14)

# visualization 2
V(largest_unw)$louvain_unw <- cluster1_unw$membership
V(largest_unw)$degree <- degree(largest_unw, normalized = TRUE)



summary_louvain_unw <- as.data.frame(cbind(V(largest_unw)$code, V(largest_unw)$louvain_unw, V(largest_unw)$degree, V(largest_unw)$histcat))
colnames(summary_louvain_unw) <- c("code", "louvain_unw", "degree", "histcat")

summary_louvain_unw$louvain_unw <- as.factor(summary_louvain_unw$louvain_unw)
summary_louvain_unw$degree <- as.numeric(summary_louvain_unw$degree)

nb.cols <- length(unique(summary_louvain_unw$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(summary_louvain_unw$histcat)), "Set3"))(nb.cols)

plot_louvain_unw <- ggplot(summary_louvain_unw, aes(x=reorder(code, -degree), y=degree, col=histcat, fill=histcat)) + 
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.text=element_text(size=10))+
  scale_color_manual(values = pal, name="broad category")+
  scale_fill_manual(values = pal, name="broad category")+
  xlab("")+
  facet_wrap(~louvain_unw, ncol=3, scales = "free")+
  theme(legend.position="bottom")
plot_louvain_unw

ggsave("figures/community_unw.tiff", plot=last_plot(), width=2000, height=1400, units="px", dpi=300)


# based on weighted network

set.seed(1234)
cluster1_w = cluster_louvain(largest_w)
modularity(cluster1_w)

sizes(cluster1_w)


# visualization 1

set.seed(1234)
plot(cluster1_w,largest_w, vertex.label=V(largest_w)$code, 
     vertex.label.cex=1.3, vertex.size=14)

# visualization 2
V(largest_w)$louvain_w <- cluster1_w$membership
V(largest_w)$degree <- degree(largest_w, normalized = TRUE)



summary_louvain_w <- as.data.frame(cbind(V(largest_w)$code, V(largest_w)$louvain_w, V(largest_w)$degree, V(largest_w)$histcat))
colnames(summary_louvain_w) <- c("code", "louvain_w", "degree", "histcat")

summary_louvain_w$louvain_w <- as.factor(summary_louvain_w$louvain_w)
summary_louvain_w$degree <- as.numeric(summary_louvain_w$degree)

nb.cols <- length(unique(summary_louvain_w$histcat))
pal <- colorRampPalette(brewer.pal(length(unique(summary_louvain_w$histcat)), "Set3"))(nb.cols)

plot_louvain_w <- ggplot(summary_louvain_w, aes(x=reorder(code, -degree), y=degree, col=histcat, fill=histcat)) + 
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.text=element_text(size=10))+
  scale_color_manual(values = pal, name="broad category")+
  scale_fill_manual(values = pal, name="broad category")+
  xlab("")+
  facet_wrap(~louvain_w, ncol=3, scales = "free")+
  theme(legend.position="bottom")
plot_louvain_w

ggsave("figures/community_weighted.tiff", plot=last_plot(), width=2500, height=1800, units="px", dpi=300)

#### alluvial plot - sensitivity to the application of weights

alluvial_unw <- summary_louvain_unw %>% 
  dplyr::select(code, louvain_unw)

alluvial_w <- summary_louvain_w %>% 
  select(code, louvain_w)

ALLUVIAL <- merge(alluvial_unw, alluvial_w, by="code")

ALLUVIAL <- ALLUVIAL %>% 
  select(louvain_unw, louvain_w) %>% 
  group_by(louvain_unw, louvain_w) %>% 
  dplyr::summarise(freq=n())

alluvial <- ggplot(ALLUVIAL,
                      aes(y = freq, axis1 = louvain_unw, axis2 = louvain_w)) +
  theme(legend.position="none")+
  geom_alluvium(aes(fill = louvain_unw), width = 1/24) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("louvain (unweighted)", "louvain (weighted)")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ylab("frequency")+
  ggtitle("Comparing community detection based on \nthe unweighted and weighted network")
alluvial

ggsave("figures/alluvial_weight_sensitivity.tiff", plot=last_plot(), width=1600, height=2000, units="px", dpi=300)