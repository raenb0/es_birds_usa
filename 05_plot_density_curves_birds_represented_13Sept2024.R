# plot density curves, bird spp represented within CNA and high carbon areas
# October 31 2023 #updated Sept 13 2024

library(ggplot2)
library(tidyverse)

#load data if needed
result_37_pct_full <- read_csv("outputs/random_sampling_result_37pct_full.csv")
result_44_pct_full <- read_csv("outputs/random_sampling_result_44pct_full.csv")

#calculate group-level mean, SD of random sampling results
result_37_pct_group_summary <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_37_pct_group_summary_confint <- result_37_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

result_44_pct_group_summary <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_44_pct_group_summary_confint <- result_44_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

#write.csv(result_44_pct_group_summary_confint, "outputs/result_44_pct_group_summary.csv")
#write.csv(result_37_pct_group_summary_confint, "outputs/result_37_pct_group_summary.csv")

#create data frames with mean, lower quantile and higher quantile #note change this to 95% CI?
result_37_pct_full_group <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(mean = mean(sample_result*100), 
            lq = quantile(sample_result*100, 0.025),
            hq = quantile(sample_result*100, 0.975))

#write.csv(result_37_pct_full_group, "result_37_pct_full_group.csv")

result_44_pct_full_group <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(mean = mean(sample_result*100), 
            lq = quantile(sample_result*100, 0.025),
            hq = quantile(sample_result*100, 0.975))

#write.csv(result_44_pct_full_group, "result_44_pct_full_group.csv")



# 37% data

# #plot random sampling results only, 37% data
# ggplot(result_37_pct_full) +
#   geom_density(aes(x = sample_result*100, fill = group))+
#   geom_vline(xintercept = 37, linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(35,40) +
#   ylim(0,5)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77", "#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by a random sample of 37% of USA land area")

# # play with histograms, random sampling results
# ggplot(result_37_pct_full) +
#   geom_histogram(aes(x = sample_result*100, y=after_stat(count/1000), fill = group))+
#   geom_vline(data = result_37_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(30,45) +
#   #ylim(0,5)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         #axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77", "#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Average % of bird species abundance represented by 1000 random samples of 37% of USA land area") +
#   ylab("Count of species")

# #plot observed results only, 37% pct data, density curves
# ggplot(result_37_pct_full) + 
#   geom_density(aes(x=sum_cna*100, fill=group)) +
#   geom_vline(xintercept = 37, linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(0,100) +
#   ylim(0,0.1)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by high NCP areas")


# #plot both observed and random sampling data, 37% data, density curves, QUARTILES, NOT CIs
# ggplot() + 
#   geom_density(data = result_37_pct_full, aes(x=sum_cna*100, fill=group)) +
#   geom_vline(data = result_37_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   geom_rect(data = result_37_pct_full_group, 
#             mapping = aes(xmin = lq, xmax = hq, ymin = -Inf, ymax = Inf), col = "grey80", fill = "grey80", alpha = 0.5) +
#   facet_wrap(~group, scales = "free_x") +
#   scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0,0.1))+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by critical natural assets, overlaid with random sampling result")
# 
# #play with histogram, change gray rectangle to 95% CIs instead of quartiles
# ggplot() + 
#   geom_histogram(data = result_37_pct_full, aes(x=sum_cna*100, y=after_stat(count/1000), fill=group)) +
#   geom_vline(data = result_37_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   geom_rect(data = result_37_pct_group_summary_confint, 
#             mapping = aes(xmin = lower95*100, xmax = upper95*100, ymin = -Inf, ymax = Inf), col = "grey80", fill = "grey80", alpha = 0.5) +
#   facet_wrap(~group, scales = "free_x") +
#   scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         #axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species abundance represented by critical natural assets, overlaid with random sampling result") +
#   ylab("Count of species")

#plot 44% data

# #plot random sampling results only, 44% data
# ggplot(result_44_pct_full) +
#   geom_density(aes(x = sample_result*100, fill = group))+
#   geom_vline(xintercept = 44, linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(40,50) +
#   ylim(0,5)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by a random sample of 44% of USA land area")

# # play with histograms, random sampling results
# ggplot(result_44_pct_full) +
#   geom_histogram(aes(x = sample_result*100, y=after_stat(count/1000), fill = group))+
#   geom_vline(data = result_44_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(40,50) +
#   #ylim(0,5)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         #axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77", "#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Average % of bird species abundance represented by 1000 random samples of 44% of USA land area") +
#   ylab("Count of species")

# #plot observed results only, 44% pct data
# ggplot(result_44_pct_full) + 
#   geom_density(aes(x=sum_carbon*100, fill=group)) +
#   geom_vline(xintercept = 44, linetype = "dashed") +
#   facet_wrap(~group, scales = "free_x") +
#   xlim(0,100) +
#   ylim(0,0.1)+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by high carbon areas")

# #plot both observed and random sampling results, 44% data, density curves, QUARTILES NOT CIs
# ggplot() + 
#   geom_density(data = result_44_pct_full, aes(x=sum_carbon*100, fill=group)) +
#   geom_vline(data = result_44_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   geom_rect(data = result_44_pct_full_group, 
#             mapping = aes(xmin = lq, xmax = hq, ymin = -Inf, ymax = Inf), col = "grey80", fill = "grey80", alpha = 0.5) +
#   facet_wrap(~group, scales = "free_x") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0,0.1))+
#   scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species represented by high carbon areas, overlaid with random sampling result")
# 
# #play with histograms, change gray rectangle to 95% CIs instead of quartiles
# ggplot() + 
#   geom_histogram(data = result_44_pct_full, aes(x=sum_carbon*100, y=..count../1000, fill=group)) +
#   geom_vline(data = result_44_pct_full_group, mapping = aes(xintercept = mean, group = group), linetype = "dashed") +
#   geom_rect(data = result_44_pct_group_summary_confint, 
#             mapping = aes(xmin = lower95*100, xmax = upper95*100, ymin = -Inf, ymax = Inf), col = "grey80", fill = "grey80", alpha = 0.5) +
#   facet_wrap(~group, scales = "free_x") +
#   scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none",
#         #axis.title.y = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
#   scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
#   xlab("Percent of bird species abundance represented by high carbon areas, overlaid with random sampling result")+
#   ylab("Count of species")
# 


# COMBINE 37 AND 44 PCT RESULTS INTO A SINGLE SET OF PLOTS ---------------------
###  12/11/2023

#load data if needed
result_37_pct_full <- read_csv("outputs/random_sampling_result_37pct_full.csv")
result_44_pct_full <- read_csv("outputs/random_sampling_result_44pct_full.csv")

#calculate group-level mean, SD of random sampling results
result_37_pct_group_summary <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_37_pct_group_summary_confint <- result_37_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

result_44_pct_group_summary <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_44_pct_group_summary_confint <- result_44_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

#create data frames with mean, lower quantile and higher quantile #note change this to 95% CI?
result_37_pct_full_group <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(mean = mean(sample_result*100), 
            lq = quantile(sample_result*100, 0.025),
            hq = quantile(sample_result*100, 0.975))

#write.csv(result_37_pct_full_group, "result_37_pct_full_group.csv")

result_44_pct_full_group <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(mean = mean(sample_result*100), 
            lq = quantile(sample_result*100, 0.025),
            hq = quantile(sample_result*100, 0.975))

#write.csv(result_44_pct_full_group, "result_44_pct_full_group.csv")


#overlapping plots
ggplot() + 
  geom_histogram(data = result_37_pct_full, 
                 aes(x = sum_cna*100, y = ..count../1000), 
                 fill = "darkgoldenrod1", alpha = 0.5) +
  geom_histogram(data = result_44_pct_full, 
                 aes(x = sum_carbon*100, y =..count../1000), 
                 fill = "cyan", alpha = 0.5) +
  geom_vline(data = result_37_pct_full_group, 
             mapping = aes(xintercept = mean, group = group), 
             col = "darkgoldenrod1") +
  geom_rect(data = result_37_pct_group_summary_confint, 
            mapping = aes(xmin = lower95*100, xmax = upper95*100, 
                          ymin = -Inf, ymax = Inf),  alpha = 0.3, 
            fill = "darkgoldenrod1", alpha = 0.5) +
  geom_vline(data = result_44_pct_full_group, 
             mapping = aes(xintercept = mean, group = group), 
             col = "darkcyan") + #linetype = "dashed",
  geom_rect(data = result_44_pct_group_summary_confint, 
            mapping = aes(xmin = lower95*100, xmax = upper95*100, 
                          ymin = -Inf, ymax = Inf), 
            fill = "cyan", alpha = 0.3) +
  facet_wrap(~group, scales = "free_x") +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill="white"),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  xlab("Percent of bird species abundance represented")+
  ylab("Count of species")


# separate plots --------------

## 37% results -------------------------
ggplot() + 
  geom_histogram(data = result_37_pct_full, 
                 aes(x = sum_cna*100, y = ..count../1000), 
                 fill = "deepskyblue3", alpha = 0.5) +
  geom_vline(data = result_37_pct_full_group, 
             mapping = aes(xintercept = mean, group = group), 
             col = "deepskyblue4") +
  geom_rect(data = result_37_pct_group_summary_confint, 
            mapping = aes(xmin = lower95*100, xmax = upper95*100, 
                          ymin = -Inf, ymax = Inf),  alpha = 0.3, 
            fill = "deepskyblue4", alpha = 0.5) +
  facet_wrap(~group, scales = "free_x") +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill="white"),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  xlab("Percent of maximum U.S. abundance")+
  ylab("Count of bird species")

# 44% results ---------------------

ggplot() + 
  geom_histogram(data = result_44_pct_full, 
                 aes(x = sum_carbon*100, y =..count../1000), 
                 fill = "darkmagenta", alpha = 0.5) +
  geom_vline(data = result_44_pct_full_group, 
             mapping = aes(xintercept = mean, group = group), 
             col = "darkorchid4") + #linetype = "dashed",
  geom_rect(data = result_44_pct_group_summary_confint, 
            mapping = aes(xmin = lower95*100, xmax = upper95*100, 
                          ymin = -Inf, ymax = Inf), 
            fill = "darkorchid4", alpha = 0.3) +
  facet_wrap(~group, scales = "free_x") +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill="white"),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  xlab("Percent of maximum U.S. abundance")+
  ylab("Count of bird species")

