# Purpose: measure and plot fin rays for shortnose sturgeon, specimen FMNH144017
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
sturgeon_ray_data <- read.table("data_raw/FMNH144017_CSA_and_SMA.txt", head=TRUE) # load data

# plot data ####

# set color scheme for d vs v
group.colors <- c(Aligned_ray1_d="#E89E1C", Aligned_ray1_v="#0D929A",
                  Aligned_ray2_d="#E89E1C", Aligned_ray2_v="#0D929A",
                  Aligned_ray3_d="#E89E1C", Aligned_ray3_v="#0D929A")


# cross sectional area.
p_csa <-ggplot(data=sturgeon_ray_data, aes(x=fin, y=csa)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,.1) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))
p_csa
ggsave("output_figures/sturgeon_csa.pdf", plot=p_csa,  width=5, height=5, units = "cm")


p_2mom_min <-ggplot(data=sturgeon_ray_data, aes(x=fin, y=second_moment_min)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  ylim(0,.0004) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))
p_2mom_min
ggsave("output_figures/sturgeon_2ndmom_min.pdf", plot=p_2mom_min,  width=5, height=5, units = "cm")


# analyzing rays ####
ray_d1 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray1_d",]
ray_v1 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray1_v",]
ray_d2 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray2_d",]
ray_v2 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray2_v",]
ray_d3 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray3_d",]
ray_v3 <- sturgeon_ray_data[sturgeon_ray_data$fin=="Aligned_ray3_v",]

# comparing difference in CSA -- for text
(mean(ray_d1$csa)/mean(ray_v1$csa) + mean(ray_d2$csa)/mean(ray_v2$csa) + mean(ray_d3$csa)/mean(ray_v3$csa)) /3

# comparing difference in 2ndmom_DV -- for text
(mean(ray_d1$second_moment_min)/mean(ray_v1$second_moment_min) + mean(ray_d2$second_moment_min)/mean(ray_v2$second_moment_min) + mean(ray_d3$second_moment_min)/mean(ray_v3$second_moment_min)) /3


# statistics  ####
# test if  data are normally distributed.
shapiro.test(ray_d1$csa)
shapiro.test(ray_d2$csa)
shapiro.test(ray_d3$csa)
shapiro.test(ray_v1$csa)
shapiro.test(ray_v2$csa)
shapiro.test(ray_d3$csa)
shapiro.test(ray_d1$second_moment_min) 
shapiro.test(ray_d2$second_moment_min)
shapiro.test(ray_d3$second_moment_min) 
shapiro.test(ray_v1$second_moment_min)
shapiro.test(ray_v2$second_moment_min) 
shapiro.test(ray_v3$second_moment_min)

#  table of these results
d1_vs_v1_csa <- as.vector(c("Acipernser", "FMNH144017", "d1_vs_v1", "csa", wilcox.test(ray_d1$csa, ray_v1$csa)$statistic, wilcox.test(ray_d1$csa, ray_v1$csa)$p.value))
d2_vs_v2_csa <- as.vector(c("Acipernser", "FMNH144017", "d2_vs_v2", "csa", wilcox.test(ray_d2$csa, ray_v2$csa)$statistic, wilcox.test(ray_d2$csa, ray_v2$csa)$p.value))
d3_vs_v3_csa <- as.vector(c("Acipernser", "FMNH144017", "d3_vs_v3", "csa", wilcox.test(ray_d3$csa, ray_v3$csa)$statistic, wilcox.test(ray_d2$csa, ray_v3$csa)$p.value))
d1_vs_v1_second_moment <- as.vector(c("Acipernser", "FMNH144017", "d1_vs_v1", "sma", wilcox.test(ray_d1$second_moment_min, ray_v1$second_moment_min)$statistic, wilcox.test(ray_d1$second_moment_min, ray_v1$second_moment_min)$p.value))
d2_vs_v2_second_moment <- as.vector(c("Acipernser", "FMNH144017", "d2_vs_v2", "sma", wilcox.test(ray_d2$second_moment_min, ray_v2$second_moment_min)$statistic, wilcox.test(ray_d2$second_moment_min, ray_v2$second_moment_min)$p.value))
d3_vs_v3_second_moment <- as.vector(c("Acipernser", "FMNH144017", "d3_vs_v3", "sma", wilcox.test(ray_d3$second_moment_min, ray_v3$second_moment_min)$statistic, wilcox.test(ray_d2$second_moment_min, ray_v3$second_moment_min)$p.value))

suppl_table <- rbind(d1_vs_v1_csa, d2_vs_v2_csa, d3_vs_v3_csa,
                   d1_vs_v1_second_moment, d2_vs_v2_second_moment, d3_vs_v3_second_moment)

colnames(suppl_table) <- c("species", "specimen","rays_compared", "comparison", "W", "p-value")

write.table(suppl_table,"output_files/tetrapodomorph_dv_wilcox_test.txt",
            row.names = FALSE,
            quote=FALSE,
            append = TRUE,
            col.names = FALSE)