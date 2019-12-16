# Purpose: measure and plot fin rays for specimen Tiktaalik NUFV110
#
# note: 
#  CSA == cross sectional area
#  second_moment_min == second moment of area around major axis
#  second_moment_max == second moment of area around minor axis
#
# 2019-05-05
# TAS


# initialize packages ####
library(ggplot2)


# load data ####
rm(list = ls()) # clear any old vars
NUFV110_all_data <- read.table("data_raw/NUFV110_CSA_and_SMA.txt", head=TRUE) # load data


# plot data ####

# set color scheme for d vs v
group.colors <- c(Aligned_pair1_d="#E89E1C", Aligned_pair1_v="#0D929A",
                  Aligned_pair2_d="#E89E1C", Aligned_pair2_v="#0D929A",
                  Aligned_pair3_d="#E89E1C", Aligned_pair3_v="#0D929A")

# cross sectional area
p_csa <- ggplot(data=NUFV110_all_data, aes(x=fin, y=csa)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,0.72) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))
ggsave("output_figures/NUFV110_csa.pdf", plot=p_csa,  width=5, height=5, units = "cm")

# second moment of area
p_2nd_mom_max <- ggplot(data=NUFV110_all_data, aes(x=fin, y=second_moment_max))  +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,0.08) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))
ggsave("output_figures/NUFV110_mom_max.pdf", plot=p_2nd_mom_max,  width=5, height=5, units = "cm")


# analyzing eusthenopteron rays ####
NUFV110_d1 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair1_d",]
NUFV110_v1 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair1_v",]
NUFV110_d2 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair2_d",]
NUFV110_v2 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair2_v",]
NUFV110_d3 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair3_d",]
NUFV110_v3 <- NUFV110_all_data[NUFV110_all_data$fin=="Aligned_pair3_v",]

# comparing area - for figure
( mean(NUFV110_d1$csa)/mean(NUFV110_v1$csa) +
  mean(NUFV110_d2$csa)/mean(NUFV110_v2$csa) +
  mean(NUFV110_d3$csa)/mean(NUFV110_v3$csa)) /3

# comparing moment of area, minimum - for figure
(mean(NUFV110_d1$second_moment_max)/mean(NUFV110_v1$second_moment_max) +
  mean(NUFV110_d2$second_moment_max)/mean(NUFV110_v2$second_moment_max)+
  mean(NUFV110_d3$second_moment_max)/mean(NUFV110_v3$second_moment_max)) /3

# assessing whether moment of area comparisons are affected by alternative orientation of the ventral ray.
(mean(NUFV110_d1$second_moment_max)/mean(NUFV110_v1$second_moment_min) +
    mean(NUFV110_d2$second_moment_max)/mean(NUFV110_v2$second_moment_min)+
    mean(NUFV110_d3$second_moment_max)/mean(NUFV110_v3$second_moment_min)) /3


# statistics ####
# test if  data are normally distributed.
shapiro.test(NUFV110_d1$csa)
shapiro.test(NUFV110_d2$csa)  
shapiro.test(NUFV110_d3$csa)
shapiro.test(NUFV110_v1$csa)
shapiro.test(NUFV110_v2$csa)
shapiro.test(NUFV110_v3$csa) 
shapiro.test(NUFV110_d1$second_moment_max) 
shapiro.test(NUFV110_d2$second_moment_max)
shapiro.test(NUFV110_d3$second_moment_max) 
shapiro.test(NUFV110_v1$second_moment_max)
shapiro.test(NUFV110_v2$second_moment_max) 
shapiro.test(NUFV110_v3$second_moment_max)

# table of these results
d1_vs_v1_csa <- as.vector(c("Tiktaalik", "NUFV110", "d1_vs_v1", "csa", wilcox.test(NUFV110_d1$csa, NUFV110_v1$csa)$statistic, wilcox.test(NUFV110_d1$csa, NUFV110_v1$csa)$p.value))
d2_vs_v2_csa <- as.vector(c("Tiktaalik", "NUFV110", "d2_vs_v3", "csa", wilcox.test(NUFV110_d2$csa, NUFV110_v2$csa)$statistic, wilcox.test(NUFV110_d2$csa, NUFV110_v2$csa)$p.value))
d3_vs_v3_csa <- as.vector(c("Tiktaalik", "NUFV110", "d2_vs_v3", "csa", wilcox.test(NUFV110_d3$csa, NUFV110_v3$csa)$statistic, wilcox.test(NUFV110_d3$csa, NUFV110_v3$csa)$p.value))
d1_vs_v1_mom <- as.vector(c("Tiktaalik", "NUFV110", "d1_vs_v1", "sma", wilcox.test(NUFV110_d1$second_moment_max, NUFV110_v1$second_moment_max)$statistic, wilcox.test(NUFV110_d1$second_moment_max, NUFV110_v1$second_moment_max)$p.value))
d2_vs_v2_mom <- as.vector(c("Tiktaalik", "NUFV110", "d2_vs_v3", "sma", wilcox.test(NUFV110_d2$second_moment_max, NUFV110_v2$second_moment_max)$statistic, wilcox.test(NUFV110_d2$second_moment_max, NUFV110_v2$second_moment_max)$p.value))
d3_vs_v3_mom <- as.vector(c("Tiktaalik", "NUFV110", "d2_vs_v3", "sma", wilcox.test(NUFV110_d3$second_moment_max, NUFV110_v3$second_moment_max)$statistic, wilcox.test(NUFV110_d3$second_moment_max, NUFV110_v3$second_moment_max)$p.value))

suppl_table <- rbind(d1_vs_v1_csa, d2_vs_v2_csa, d3_vs_v3_csa, d1_vs_v1_mom, d2_vs_v2_mom, d3_vs_v3_mom)

colnames(suppl_table) <- c("species", "specimen","rays_compared", "comparison", "W", "p-value")

write.table(suppl_table,"output_files/tetrapodomorph_dv_wilcox_test.txt",
            row.names = FALSE,
            quote=FALSE)