# Measure and plot fin rays for Eusthenopteron CMN10926 DV comparison
# May 2019
# TAS


# initialize packages ####
library(ggplot2)


# load data ####
eusthenopteron_all_data <- read.delim("data/CMNH8153_measured_rays.txt") # load data

eusthenopteron_all_data <- eusthenopteron_all_data[c("Label", "Slice","CSA","Imin_._.4.","Imax_._.4.")] # readuce to variables we compare
colnames(eusthenopteron_all_data) <- c("fin", "slice","csa","second_moment_min", "second_moment_max") # re-name columns for clarity.


# plot data ####

# set color scheme for d vs v
group.colors <- c(Aligned_pair1_d="#E89E1C", Aligned_pair1_v="#0D929A",
                  Aligned_pair2_d="#E89E1C", Aligned_pair2_v="#0D929A",
                  Aligned_pair3_d="#E89E1C", Aligned_pair3_v="#0D929A",
                  Aligned_pair3_d2="#E89E1C", Aligned_pair3_v2="#0D929A")

# cross sectional area.
p_eust_csa <- ggplot(data=eusthenopteron_all_data, aes(x=fin, y=csa)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,2) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))

ggsave("output_figures/CMNH8153_csa.pdf", plot=p_eust_csa,  width=5, height=5, units = "cm")


# second moment of area - minimum 
p_eust_2nd_mom <- ggplot(data=eusthenopteron_all_data, aes(x=fin, y=second_moment_min))  +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  theme_classic() +
  ylim(0,0.23) +
  theme(legend.position="none") +
  scale_color_manual(values = group.colors)

ggsave("output_figures/CMNH8153_mom.pdf", plot=p_eust_2nd_mom,  width=6, height=5, units = "cm")

# analyzing eusthenopteron rays ####
eusth_d1 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair1_d",]
eusth_v1 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair1_v",]
eusth_d2 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair2_d",]
eusth_v2 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair2_v",]
eusth_d3 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair3_d",]
eusth_v3 <- eusthenopteron_all_data[eusthenopteron_all_data$fin=="Aligned_pair3_v",]

# comparing difference in CSA -- for text
(mean(eusth_d1$csa)/mean(eusth_v1$csa) + mean(eusth_d2$csa)/mean(eusth_v2$csa) + mean(eusth_d3$csa)/mean(eusth_v3$csa)) /3

# comparing difference in SMA -- for text
(mean(eusth_d1$second_moment_min)/mean(eusth_v1$second_moment_min) + mean(eusth_d2$second_moment_min)/mean(eusth_v2$second_moment_min) + mean(eusth_d3$second_moment_min)/mean(eusth_v3$second_moment_min)) /3

# statistics  
# test if  data are normally distributed.
shapiro.test(eusth_d1$csa)
shapiro.test(eusth_d2$csa)
shapiro.test(eusth_d3$csa)
shapiro.test(eusth_v1$csa)
shapiro.test(eusth_v2$csa)
shapiro.test(eusth_d3$csa)
shapiro.test(eusth_d1$second_moment_min)
shapiro.test(eusth_d2$second_moment_min)
shapiro.test(eusth_d3$second_moment_min)
shapiro.test(eusth_v1$second_moment_min)
shapiro.test(eusth_v2$second_moment_min)
shapiro.test(eusth_v3$second_moment_min)

#  table of these results
d1_vs_v1_csa <- as.vector(c("Eusthenopteron", "CMNH8153", "d1_vs_v1", "csa", wilcox.test(eusth_d1$csa, eusth_v1$csa)$statistic, wilcox.test(eusth_d1$csa, eusth_v1$csa)$p.value))
d2_vs_v2_csa <- as.vector(c("Eusthenopteron", "CMNH8153", "d2_vs_v2", "csa", wilcox.test(eusth_d2$csa, eusth_v2$csa)$statistic, wilcox.test(eusth_d2$csa, eusth_v2$csa)$p.value))
d3_vs_v3_csa <- as.vector(c("Eusthenopteron", "CMNH8153", "d3_vs_v3", "csa", wilcox.test(eusth_d3$csa, eusth_v3$csa)$statistic, wilcox.test(eusth_d2$csa, eusth_v3$csa)$p.value))

d1_vs_v1_mom <- as.vector(c("Eusthenopteron", "CMNH8153", "d1_vs_v1", "second_moment", wilcox.test(eusth_d1$second_moment_min, eusth_v1$second_moment_min)$statistic, wilcox.test(eusth_d1$second_moment_min, eusth_v1$second_moment_min)$p.value))
d2_vs_v2_mom <- as.vector(c("Eusthenopteron", "CMNH8153", "d2_vs_v2", "second_moment", wilcox.test(eusth_d2$second_moment_min, eusth_v2$second_moment_min)$statistic, wilcox.test(eusth_d2$second_moment_min, eusth_v2$second_moment_min)$p.value))
d3_vs_v3_mom <- as.vector(c("Eusthenopteron", "CMNH8153", "d3_vs_v3", "second_moment", wilcox.test(eusth_d3$second_moment_min, eusth_v3$second_moment_min)$statistic, wilcox.test(eusth_d3$second_moment_min, eusth_v3$second_moment_min)$p.value))

suppl_table <- rbind(d1_vs_v1_csa, d2_vs_v2_csa, d3_vs_v3_csa,
                     d1_vs_v1_mom, d2_vs_v2_mom, d3_vs_v3_mom)
colnames(suppl_table) <- c("species", "specimen","rays_compared", "comparison", "W", "p-value")

write.table(suppl_table,"output_files/tetrapodomorph_dv_wilcox_test.txt",
            row.names = FALSE,
            quote=FALSE,
            append = TRUE,
            col.names = FALSE)
