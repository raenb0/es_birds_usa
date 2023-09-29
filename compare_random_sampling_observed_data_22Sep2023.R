# compare random sampling results to actual results
# September 29 2023

library(tidyverse)
library(reshape)
library(data.table)

setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa")

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

#write_csv(result_37pct_comparison, "outputs/result_37pct_comparison.csv")
#write_csv(result_44pct_comparison, "outputs/result_44pct_comparison.csv")


# calculate count of spp that are over- or under-represented compared to random sample
result_37pct_comparison <- read_csv("outputs/result_37pct_comparison.csv")
result_44pct_comparison <- read_csv("outputs/result_44pct_comparison.csv")

result_37pct_comparison <- result_37pct_comparison %>%
  dplyr::mutate(over_upper95 = ifelse(sum_cna > upper95,"1","0"))
result_37pct_comparison <- result_37pct_comparison %>%
  dplyr::mutate(under_lower95 = ifelse(sum_cna < lower95,"1","0"))

result_44pct_comparison <- result_44pct_comparison %>%
  dplyr::mutate(over_upper95 = ifelse(sum_carbon > upper95,"1","0"))
result_44pct_comparison <- result_44pct_comparison %>%
  dplyr::mutate(under_lower95 = ifelse(sum_carbon < lower95,"1","0"))

result_37pct_comparison$over_upper95 <- as.integer(result_37pct_comparison$over_upper95)

write_csv(result_37pct_comparison, "outputs/result_37pct_comparison.csv")
write_csv(result_44pct_comparison, "outputs/result_44pct_comparison.csv")

#summarize by habitat
summary_37pct_spp_represented_by_group <- result_37pct_comparison %>%
  group_by(habitat) %>%
  summarize(count_spp = n(),
            count_spp_over95CI = sum(as.integer(over_upper95), na.rm=T),
            count_spp_under95CI = sum(as.integer(under_lower95), na.rm=T))

summary_44pct_spp_represented_by_group <- result_44pct_comparison %>%
  group_by(habitat) %>%
  summarize(count_spp = n(),
            count_spp_over95CI = sum(as.integer(over_upper95), na.rm=T),
            count_spp_under95CI = sum(as.integer(under_lower95), na.rm=T))

#summarize for tipping pt spp
summary_37pct_spp_represented_tp <- result_37pct_comparison %>%
  group_by(tipping_pt) %>%
  summarize(count_spp = n(),
            count_spp_over95CI = sum(as.integer(over_upper95), na.rm=T),
            count_spp_under95CI = sum(as.integer(under_lower95), na.rm=T))

summary_37pct_spp_represented_tp <- summary_37pct_spp_represented_tp %>%
  filter(tipping_pt == "Tipping Point") %>% #drop last row
  dplyr::rename("habitat" = "tipping_pt") #rename to combine

summary_44pct_spp_represented_tp <- result_44pct_comparison %>%
  group_by(tipping_pt) %>%
  summarize(count_spp = n(),
            count_spp_over95CI = sum(as.integer(over_upper95), na.rm=T),
            count_spp_under95CI = sum(as.integer(under_lower95), na.rm=T))

summary_44pct_spp_represented_tp <- summary_44pct_spp_represented_tp %>%
  filter(tipping_pt == "Tipping Point") %>% #drop last row
  dplyr::rename("habitat" = "tipping_pt") #rename to combine

# combine
summary_37pct_spp_represented_by_group <- rbind(summary_37pct_spp_represented_by_group, summary_37pct_spp_represented_tp)

summary_44pct_spp_represented_by_group <- rbind(summary_44pct_spp_represented_by_group, summary_44pct_spp_represented_tp)

write_csv(summary_37pct_spp_represented_by_group, "outputs/summary_37pct_spp_represented_by_group.csv")
write_csv(summary_44pct_spp_represented_by_group, "outputs/summary_44pct_spp_represented_by_group.csv")

# format data for plotting

#load complete random sampling results:
random_sampling_result_37pct <- read_csv("outputs/random_sampling_result_37pct.csv")
random_sampling_result_44pct <- read_csv("outputs/random_sampling_result_44pct.csv")

#melt
random_sampling_result_37pct_melt <- melt(random_sampling_result_37pct)
random_sampling_result_44pct_melt <- melt(random_sampling_result_44pct)


#calculate mean and SD of entire random sample
mean_37pct <- mean(random_sampling_result_37pct_melt$value, na.rm=T)
sd_37pct <- sd(random_sampling_result_37pct_melt$value, na.rm=T)

mean_44pct <- mean(random_sampling_result_44pct_melt$value, na.rm=T)
sd_44pct <- sd(random_sampling_result_44pct_melt$value, na.rm=T)


#create summary table
sample_size <- c("37pct", "44pct")
mean_sample <- c(mean_37pct, mean_44pct)
sd_sample <- c(sd_37pct, sd_44pct)
mean_sd_sample_summary <- cbind(sample_size, mean_sample, sd_sample)

write.csv(mean_sd_sample_summary, "outputs/random_sample_mean_sd_summary.csv")

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


# plot tipping point species, comparison of CNA results to random sampling -----------

result_37pct_comparison <- read_csv("outputs/result_37pct_comparison.csv")
result_44pct_comparison <- read_csv("outputs/result_44pct_comparison.csv")

tp_sps_37pct_comparison <- result_37pct_comparison %>%
  filter(tipping_pt == "Tipping Point") %>%
  arrange(sum_cna) #arrange spp in descending order by representation level

tp_sps_44pct_comparison <- result_44pct_comparison %>%
  filter(tipping_pt == "Tipping Point") %>%
  arrange(sum_carbon) #arrange spp in descending order by representation level

# convert to factor to retain order of spp
tp_sps_37pct_comparison$species <- factor(tp_sps_37pct_comparison$species, tp_sps_37pct_comparison$species)
tp_sps_44pct_comparison$species <- factor(tp_sps_44pct_comparison$species, tp_sps_44pct_comparison$species)

#create a color gradient red to blue
library(scales)
red_blue_scale <- scales::seq_gradient_pal("red","blue","Lab")(seq(0,1,length.out=57))

#plot CNA 37% results, tipping pt spp
ggplot(tp_sps_37pct_comparison, aes(x = species, y = sum_cna*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 34, ymax = 40), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=37, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of tipping point species represented within critical natural assets") +
  theme(legend.position = "none")

#plot high carbon 44% results, tipping pt spp
ggplot(tp_sps_44pct_comparison, aes(x = species, y = sum_carbon*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 41, ymax = 47), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=44, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of tipping point species represented within high carbon areas") +
  theme(legend.position = "none")
  


