# compare random sampling results to actual results
# September 16 2023

library(tidyverse)
library(reshape)

#load random sampling data
random_result_37pct <- read_csv("outputs/random_sampling_result_37pct_mean_sd_confint_biomes.csv")
random_result_44pct <- read_csv("outputs/random_sampling_result_44pct_mean_sd_confint_biomes.csv")

# load species represented by high NCP areas data
pct_pop_per_spp_cna_sum <- read_csv("outputs/pct_pop_per_spp_cna_sum.csv")
pct_pop_per_spp_carbon_sum <- read_csv("outputs/pct_pop_per_spp_carbon_90pct_sum.csv")

#join tables
names(pct_pop_per_spp_cna_sum)
names(random_result_37pct)
result_37pct_comparison <- left_join(random_result_37pct, pct_pop_per_spp_cna_sum, by="species")
result_44pct_comparison <- left_join(random_result_44pct, pct_pop_per_spp_carbon_sum, by="species")

write_csv(result_37pct_comparison, "outputs/result_37pct_comparison.csv")
write_csv(result_44pct_comparison, "outputs/result_44pct_comparison.csv")


# plot random sampling results, by habitat group

#load complete random sampling results:
random_sampling_result_37pct <- read_csv("outputs/random_sampling_result_37pct.csv")
random_sampling_result_44pct <- read_csv("outputs/random_sampling_result_44pct.csv")

#pivot_longer
random_sampling_result_37pct_longer <- melt(random_sampling_result_37pct)
random_sampling_result_44pct_longer <- melt(random_sampling_result_44pct)

#select and join other data
names(result_37pct_comparison)
selected_cols_37 <- result_37pct_comparison %>%
  select(species, habitat, tipping_pt, sum_cna)
result_37_pct_longer_join <- left_join(random_sampling_result_37pct_longer, selected_cols_37, by="species", multiple="all")

write_csv(result_37_pct_longer_join,"outputs/random_sampling_result_37pct_longer_join.csv" )

#plot random sampling results only
ggplot(result_37_pct_longer_join, aes(x = value*100, y = habitat, fill = habitat)) + 
  geom_density_ridges2(rel_min_height = 0.001) + #played with rel_min_height but it didn't help
  geom_vline(xintercept = 37, linetype = "dashed") +
  xlim(35,40) +
  scale_y_discrete(expansion(add = c(0, 1))) + #played with this but it didn't help
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("lightyellow", "#117733","#999933","#DDDDDD","#CC6677","#88CCEE")) + 
  xlab("Birds represented by a random sample of 37% of USA land area")

#add observed results (37pct)to plot
ggplot(result_37_pct_longer_join, aes(x = sum_cna*100, y = habitat, fill = habitat)) + 
  geom_density_ridges2(rel_min_height = 0.001) + #played with rel_min_height but it didn't help
  geom_density_ridges2(aes(x=value*100, y=habitat, fill=habitat)) +
  geom_vline(xintercept = 37, linetype = "dashed") +
  xlim(0,100) +
  scale_y_discrete(expansion(add = c(0, 1))) + #played with this but it didn't help
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("lightyellow", "#117733","#999933","#DDDDDD","#CC6677","#88CCEE")) + 
  xlab("Birds represented by high NCP areas")

