# compare random sampling results to actual results
# September 16 2023

library(tidyverse)
library(reshape)
library(data.table)

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


# format data for plotting

#load complete random sampling results:
random_sampling_result_37pct <- read_csv("outputs/random_sampling_result_37pct.csv")
random_sampling_result_44pct <- read_csv("outputs/random_sampling_result_44pct.csv")

#melt
random_sampling_result_37pct_melt <- melt(random_sampling_result_37pct)
random_sampling_result_44pct_melt <- melt(random_sampling_result_44pct)

#select and join other data
names(result_37pct_comparison)
selected_cols_37 <- result_37pct_comparison %>%
  select(species, habitat, tipping_pt, sum_cna) #avg, stdev, lower95, upper95
result_37_pct_melt_join <- left_join(random_sampling_result_37pct_melt, selected_cols_37, by="species", multiple="all")

names(result_44pct_comparison)
selected_cols_44 <- result_44pct_comparison %>%
  select(species, habitat, tipping_pt, sum_carbon) #avg, stdev, lower95, upper95
result_44_pct_melt_join <- left_join(random_sampling_result_44pct_melt, selected_cols_44, by="species", multiple="all")

#rename "value" column to "sample_result" and "variable" column to "iteration"
result_37_pct_melt_join <- result_37_pct_melt_join %>%
  dplyr::rename("sample_result" = "value",
                "iteration" = "variable")
result_44_pct_melt_join <- result_44_pct_melt_join %>%
  dplyr::rename("sample_result" = "value",
                "iteration" = "variable")

write_csv(result_37_pct_melt_join,"outputs/random_sampling_result_37pct_melt_join.csv")
write_csv(result_44_pct_melt_join,"outputs/random_sampling_result_44pct_melt_join.csv")

#load data if needed
#result_37_pct_melt_join <- read_csv("outputs/random_sampling_result_37pct_melt_join.csv")
#result_44_pct_melt_join <- read_csv("outputs/random_sampling_result_44pct_melt_join.csv")

# collapse habitat and tipping point into a single column
names(result_37_pct_melt_join)
selected_37pct_data_1 <- result_37_pct_melt_join %>%
  dplyr::select(!tipping_pt)
selected_37pct_data_2 <- result_37_pct_melt_join %>%
  dplyr::select(!habitat) %>%
  dplyr::filter(tipping_pt=="Tipping Point")

colnames(selected_37pct_data_1)[4] <- colnames(selected_37pct_data_2)[4] <- "group" #rename column

result_37_pct_full <- rbind(selected_37pct_data_1, selected_37pct_data_2)

names(result_44_pct_melt_join)
selected_44pct_data_1 <- result_44_pct_melt_join %>%
  dplyr::select(!tipping_pt)
selected_44pct_data_2 <- result_44_pct_melt_join %>%
  dplyr::select(!habitat) %>%
  dplyr::filter(tipping_pt=="Tipping Point")

colnames(selected_44pct_data_1)[4] <- colnames(selected_44pct_data_2)[4] <- "group" #rename column

result_44_pct_full <- rbind(selected_44pct_data_1, selected_44pct_data_2)

# #rename columns for clarity
# names(result_37_pct_full)
# names(result_44_pct_full)
# result_37_pct_full <- result_37_pct_full %>%
#   dplyr::rename("sample_avg" = "avg",
#                 "sample_stdev" = "stdev")
# result_44_pct_full <- result_44_pct_full %>%
#   dplyr::rename("sample_avg" = "avg",
#                 "sample_stdev" = "stdev")

write_csv(result_37_pct_full,"outputs/random_sampling_result_37pct_full.csv")
write_csv(result_44_pct_full,"outputs/random_sampling_result_44pct_full.csv")


# plot density curves

#load data if needed
result_37_pct_full <- read_csv("outputs/random_sampling_result_37pct_full.csv")
result_44_pct_full <- read_csv("outputs/random_sampling_result_44pct_full.csv")

#calculate group-level mean, SD of random sampling results
result_37_pct_group_summary <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

result_44_pct_group_summary <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# 37% data

#plot random sampling results only, 37% data
ggplot(result_37_pct_full) +
  geom_density(aes(x = sample_result*100, fill = group))+
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(35,40) +
  ylim(0,5)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77", "#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by a random sample of 37% of USA land area")

#plot observed results only, 37% pct data
ggplot(result_37_pct_full) + 
  geom_density(aes(x=sum_cna*100, fill=group)) +
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high NCP areas")

#plot both, 37% data
ggplot(result_37_pct_full) + 
  geom_density(aes(x=sum_cna*100, fill=group)) +
  geom_density(aes(x=sample_result*100)) +
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high NCP areas, overlaid with random sampling result")

#plot 44% data

#plot random sampling results only, 44% data
ggplot(result_44_pct_full) +
  geom_density(aes(x = sample_result*100, fill = group))+
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(40,50) +
  ylim(0,5)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by a random sample of 44% of USA land area")

#plot observed results only, 44% pct data
ggplot(result_44_pct_full) + 
  geom_density(aes(x=sum_carbon*100, fill=group)) +
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high carbon areas")

#plot both, 44% data
ggplot(result_44_pct_full) + 
  geom_density(aes(x=sum_carbon*100, fill=group)) +
  geom_density(aes(x=sample_result*100)) +
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high carbon areas, overlaid with random sampling result")




