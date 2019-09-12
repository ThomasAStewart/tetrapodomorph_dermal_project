# Measure and plot fin rays for Eusthenopteron NUFV109    d DV comparison
# May 2019
# TAS


# initialize packages ####
library(ggplot2) 


# load data ####
NUV109_all_data <- read.delim("data/NUFV109_measured_rays.txt") # load data

NUV109_all_data <- NUV109_all_data[c("Label", "Slice","CSA","Imin_._.4.","Imax_._.4.")] # readuce to variables we compare
colnames(NUV109_all_data) <- c("fin", "slice","csa","second_moment_min", "second_moment_max") # re-name columns for clarity.


# plot data ####

# set color scheme for d vs v
group.colors <- c(Aligned_pair1_d="#E89E1C", Aligned_pair1_v="#0D929A",
                  Aligned_pair2_d="#E89E1C", Aligned_pair2_v="#0D929A",
                  Aligned_pair3_d="#E89E1C", Aligned_pair3_v="#0D929A")

# cross sectional area.
p_csa <- ggplot(data=NUV109_all_data, aes(x=fin, y=csa)) +
  geom_boxplot(alpha=1) +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,1.6) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))

ggsave("output_figures/NUFV109_csa.pdf", plot=p_csa,  width=5, height=5, units = "cm")


# second moment of area - max 
p_2nd_mom <- ggplot(data=NUV109_all_data, aes(x=fin, y=second_moment_max))  +
  geom_boxplot(alpha=1.0) +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  theme_classic() +
 ylim(0,0.5) +
  theme(legend.position="none") +
  scale_color_manual(values = group.colors)

ggsave("output_figures/NUFV109_csa_mom.pdf", plot=p_2nd_mom,  width=6, height=5, units = "cm")

# second moment of area - max # I think not going to use. In this spp., it corresponds to the AP axis of the fin. 

# analyzing NUV109 rays ####
NUFV109_d1 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair1_d",]
NUFV109_v1 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair1_v",]
NUFV109_d2 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair2_d",]
NUFV109_v2 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair2_v",]
NUFV109_d3 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair3_d",]
NUFV109_v3 <- NUV109_all_data[NUV109_all_data$fin=="Aligned_pair3_v",]

# comparing area - for figure
(mean(NUFV109_d1$csa)/mean(NUFV109_v1$csa) +
  mean(NUFV109_d2$csa)/mean(NUFV109_v2$csa) +
  mean(NUFV109_d3$csa)/mean(NUFV109_v3$csa)) /3

# comparing moment of area, max - for figure
(mean(NUFV109_d1$second_moment_max)/mean(NUFV109_v1$second_moment_max) +
  mean(NUFV109_d2$second_moment_max)/mean(NUFV109_v2$second_moment_max) +
  mean(NUFV109_d3$second_moment_max)/mean(NUFV109_v3$second_moment_max)) /3


# statistics  
# test if  data are normally distributed.
shapiro.test(NUFV109_d1$csa)
shapiro.test(NUFV109_d2$csa)
shapiro.test(NUFV109_d3$csa)
shapiro.test(NUFV109_v1$csa)
shapiro.test(NUFV109_v2$csa)
shapiro.test(NUFV109_v3$csa) # greater than 0.05 = normal
shapiro.test(NUFV109_d1$second_moment_min) # greater than 0.05= normal
shapiro.test(NUFV109_d2$second_moment_min)
shapiro.test(NUFV109_d3$second_moment_min) # greater than 0.05= normal
shapiro.test(NUFV109_v1$second_moment_min)
shapiro.test(NUFV109_v2$second_moment_min) # greater than 0.05 = normal
shapiro.test(NUFV109_v3$second_moment_min)


#  table of these results
d1_vs_v1_csa <- as.vector(c("Tiktaalik", "NUV109", "d1_vs_v1", "csa", wilcox.test(NUFV109_d1$csa, NUFV109_v1$csa)$statistic, wilcox.test(NUFV109_d1$csa, NUFV109_v1$csa)$p.value))
d2_vs_v2_csa <- as.vector(c("Tiktaalik", "NUV109", "d2_vs_v3", "csa", wilcox.test(NUFV109_d2$csa, NUFV109_v2$csa)$statistic, wilcox.test(NUFV109_d2$csa, NUFV109_v2$csa)$p.value))
d3_vs_v3_csa <- as.vector(c("Tiktaalik", "NUV109", "d2_vs_v3", "csa", wilcox.test(NUFV109_d3$csa, NUFV109_v3$csa)$statistic, wilcox.test(NUFV109_d3$csa, NUFV109_v3$csa)$p.value))
d1_vs_v1_mom <- as.vector(c("Tiktaalik", "NUV109", "d1_vs_v1", "second_moment", wilcox.test(NUFV109_d1$second_moment_min, NUFV109_v1$second_moment_min)$statistic, wilcox.test(NUFV109_d1$second_moment_min, NUFV109_v1$second_moment_min)$p.value))
d2_vs_v2_mom <- as.vector(c("Tiktaalik", "NUV109", "d2_vs_v3", "second_moment", wilcox.test(NUFV109_d2$second_moment_min, NUFV109_v2$second_moment_min)$statistic, wilcox.test(NUFV109_d2$second_moment_min, NUFV109_v2$second_moment_min)$p.value))
d3_vs_v3_mom <- as.vector(c("Tiktaalik", "NUV109", "d2_vs_v3", "second_moment", wilcox.test(NUFV109_d3$second_moment_min, NUFV109_v3$second_moment_min)$statistic, wilcox.test(NUFV109_d3$second_moment_min, NUFV109_v3$second_moment_min)$p.value))

suppl_table <- rbind(d1_vs_v1_csa, d2_vs_v2_csa, d3_vs_v3_csa,
                     d1_vs_v1_mom, d2_vs_v2_mom, d3_vs_v3_mom)

colnames(suppl_table) <- c("species", "specimen","rays_compared", "comparison", "W", "p-value")

write.table(suppl_table,"output_files/tetrapodomorph_dv_wilcox_test.txt",
            row.names = FALSE,
            quote=FALSE,
            append = TRUE,
            col.names = FALSE)