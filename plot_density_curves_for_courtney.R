# plot density curves
library(tidyverse)

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
