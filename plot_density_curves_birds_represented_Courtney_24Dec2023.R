
library(ggplot2)
library(tidyverse)

####### COMBINE 37 AND 44 PCT RESULTS INTO A SINGLE SET OF PLOTS
### Code from Courtney Davis 12/11/2023

#load data if needed (change file path)
result_37_pct_full <- read_csv("random_sampling_result_37pct_full.csv")
result_44_pct_full <- read_csv("random_sampling_result_44pct_full.csv")
#result_37_pct_full <- read_csv("outputs/random_sampling_result_37pct_full.csv")
#result_44_pct_full <- read_csv("outputs/random_sampling_result_44pct_full.csv")

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
  scale_x_continuous(expand = c(0,0), limits = c(0,102)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,21))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill="white"),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  xlab("Percent of US population represented")+
  ylab("Count of species")
