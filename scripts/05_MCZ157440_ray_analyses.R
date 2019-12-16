# Purpose: measure and plot fin rays for Australian lungfish, specimen MCZ157440
#
# note: 
#  CSA == cross sectional area
#  second_moment_min == second moment of area around major axis
#  second_moment_max == second moment of area around minor axis
#
# note:
#   The rays in the data set are ordered from posterior to anterior. This is opposite of all other species. 
#   The manuscript presents them in the correct AP order (i.e., swapped with the #s here).  
#
# 2019-12-05
# TAS


# initialize packages ####
library(ggplot2)


# load data ####
rm(list = ls()) # clear any old vars
neoceratodus_ray_data <- read.table("data_raw/MCZ157440_CSA_and_SMA.txt", head=TRUE)


# plot data ####

# set color scheme for d vs v
group.colors <- c(Aligned_ray1_d="#E89E1C", Aligned_ray1_v="#0D929A",
                  Aligned_ray2_d="#E89E1C", Aligned_ray2_v="#0D929A",
                  Aligned_ray3_d="#E89E1C", Aligned_ray3_v="#0D929A",
                  Aligned_ray4_d="#E89E1C", Aligned_ray4_v="#0D929A",
                  Aligned_ray5_d="#E89E1C", Aligned_ray5_v="#0D929A",
                  Aligned_ray6_d="#E89E1C", Aligned_ray6_v="#0D929A")


# cross sectional area.
p_csa <-ggplot(data=neoceratodus_ray_data, aes(x=fin, y=csa)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2), size=1, alpha=0.25, aes(color=fin)) +
  scale_color_manual(values = group.colors) +
  theme_classic() +
  theme(legend.position="none", text=element_text(size=6))
p_csa
ggsave("output_figures/neoceratodus_csa.pdf", plot=p_csa,  width=5, height=5, units = "cm")


# analyzing rays ####
ray_d1 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray1_d",]
ray_v1 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray1_v",]
ray_d2 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray2_d",]
ray_v2 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray2_v",]
ray_d3 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray3_d",]
ray_v3 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray3_v",]
ray_d4 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray4_d",]
ray_v4 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray4_v",]
ray_d5 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray5_d",]
ray_v5 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray5_v",]
ray_d6 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray6_d",]
ray_v6 <- neoceratodus_ray_data[neoceratodus_ray_data$fin=="Aligned_ray6_v",]

# comparing difference in CSA
(mean(ray_d1$csa)/mean(ray_v1$csa) + mean(ray_d2$csa)/mean(ray_v2$csa) + mean(ray_d3$csa)/mean(ray_v3$csa)) /3

(mean(ray_d4$csa)/mean(ray_v4$csa) + mean(ray_d5$csa)/mean(ray_v5$csa) + mean(ray_d6$csa)/mean(ray_v6$csa)) /3


# statistics ####
# test if  data are normally distributed.
shapiro.test(ray_d1$csa)
shapiro.test(ray_d2$csa)
shapiro.test(ray_d3$csa)
shapiro.test(ray_v1$csa)
shapiro.test(ray_v2$csa)
shapiro.test(ray_d3$csa)

# table of these results
#   here I reverse the order to propper A to P; the output file is correct,
d6_vs_v6_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d6_vs_v6", "csa", wilcox.test(ray_d1$csa, ray_v1$csa)$statistic, wilcox.test(ray_d1$csa, ray_v1$csa)$p.value))
d5_vs_v5_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d5_vs_v5", "csa", wilcox.test(ray_d2$csa, ray_v2$csa)$statistic, wilcox.test(ray_d2$csa, ray_v2$csa)$p.value))
d4_vs_v4_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d4_vs_v4", "csa", wilcox.test(ray_d3$csa, ray_v3$csa)$statistic, wilcox.test(ray_d2$csa, ray_v3$csa)$p.value))
d3_vs_v3_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d3_vs_v3", "csa", wilcox.test(ray_d4$csa, ray_v4$csa)$statistic, wilcox.test(ray_d4$csa, ray_v4$csa)$p.value))
d2_vs_v2_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d2_vs_v2", "csa", wilcox.test(ray_d5$csa, ray_v5$csa)$statistic, wilcox.test(ray_d5$csa, ray_v5$csa)$p.value))
d1_vs_v1_csa <- as.vector(c("Neoceratodus", "MCZ157440", "d1_vs_v1", "csa", wilcox.test(ray_d6$csa, ray_v6$csa)$statistic, wilcox.test(ray_d6$csa, ray_v6$csa)$p.value))

suppl_table <- rbind(d1_vs_v1_csa, d2_vs_v2_csa, d3_vs_v3_csa, d4_vs_v4_csa, d5_vs_v5_csa, d6_vs_v6_csa)

colnames(suppl_table) <- c("species", "specimen","rays_compared", "comparison", "W", "p-value")

write.table(suppl_table,"output_files/tetrapodomorph_dv_wilcox_test.txt",
            row.names = FALSE,
            quote=FALSE,
            append = TRUE,
            col.names = FALSE)