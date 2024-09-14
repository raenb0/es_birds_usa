# compare random sampling results to actual results
# October 25 2023

library(tidyverse)
library(reshape)
library(data.table)

#setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa") #Cornell laptop location
setwd("C:/Users/rneugarten/Documents/GitHub/es_birds_usa") #WCS laptop location

#load random sampling data
random_result_37pct <- read_csv("outputs/random_sampling_result_37pct_mean_sd_confint_biomes.csv")
random_result_44pct <- read_csv("outputs/random_sampling_result_44pct_mean_sd_confint_biomes.csv")

# load species represented by high NCP areas data #updated
pct_pop_per_spp_cna_sum <- read_csv("outputs/pct_pop_per_spp_cna_sum_23Oct2023.csv")
pct_pop_per_spp_carbon_sum <- read_csv("outputs/pct_pop_per_spp_carbon_sum_25Oct2023.csv")

#join tables
names(pct_pop_per_spp_cna_sum)
names(random_result_37pct)
result_37pct_comparison <- left_join(random_result_37pct, pct_pop_per_spp_cna_sum, by="species")
result_44pct_comparison <- left_join(random_result_44pct, pct_pop_per_spp_carbon_sum, by="species")

#write_csv(result_37pct_comparison, "outputs/result_37pct_comparison.csv")
#write_csv(result_44pct_comparison, "outputs/result_44pct_comparison.csv")


# calculate count of spp that are over- or under-represented compared to random sample --------------
#result_37pct_comparison <- read_csv("outputs/result_37pct_comparison.csv")
#result_44pct_comparison <- read_csv("outputs/result_44pct_comparison.csv")

result_37pct_comparison <- result_37pct_comparison %>%
  dplyr::mutate(over_upper95 = ifelse(sum_cna > upper95,"1","0"))
result_37pct_comparison <- result_37pct_comparison %>%
  dplyr::mutate(under_lower95 = ifelse(sum_cna < lower95,"1","0"))

result_44pct_comparison <- result_44pct_comparison %>%
  dplyr::mutate(over_upper95 = ifelse(sum_carbon > upper95,"1","0"))
result_44pct_comparison <- result_44pct_comparison %>%
  dplyr::mutate(under_lower95 = ifelse(sum_carbon < lower95,"1","0"))

result_37pct_comparison$over_upper95 <- as.integer(result_37pct_comparison$over_upper95)

#write_csv(result_37pct_comparison, "outputs/result_37pct_comparison.csv")
#write_csv(result_44pct_comparison, "outputs/result_44pct_comparison.csv")


# bring in species common and scientific names ------------------
library(tidyverse)

biome_sps_sel_all_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome groups

spp_names <- biome_sps_sel_all_vars %>%
  select(species_code, common_name, scientific_name)

#load data if necessary
result_37pct_comparison <- read_csv("outputs/result_37pct_comparison.csv")
result_44pct_comparison <- read_csv("outputs/result_44pct_comparison.csv")

result_37pct_comparison_spp_names <- left_join(spp_names, result_37pct_comparison, by=join_by(species_code == species))
result_44pct_comparison_spp_names <- left_join(spp_names, result_44pct_comparison, by=join_by(species_code == species))

#write_csv(result_37pct_comparison_spp_names, "outputs/result_37pct_comparison_spp_names.csv")
#write_csv(result_44pct_comparison_spp_names, "outputs/result_44pct_comparison_spp_names.csv")


#summarize by habitat -----------------------------
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

#write_csv(summary_37pct_spp_represented_by_group, "outputs/summary_37pct_spp_represented_by_group.csv")
#write_csv(summary_44pct_spp_represented_by_group, "outputs/summary_44pct_spp_represented_by_group.csv")

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

#write.csv(mean_sd_sample_summary, "outputs/random_sample_mean_sd_summary.csv")

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

#write_csv(result_37_pct_full,"outputs/random_sampling_result_37pct_full.csv")
#write_csv(result_44_pct_full,"outputs/random_sampling_result_44pct_full.csv")


# plot tipping point species, comparison of CNA results to random sampling -----------
library(tidyverse)

result_37pct_comparison <- read_csv("outputs/result_37pct_comparison.csv")
result_44pct_comparison <- read_csv("outputs/result_44pct_comparison.csv")

#filter tipping point species
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
  
# repeat plotting for forest species

# filter forest species
forest_sps_37pct_comparison <- result_37pct_comparison %>%
  filter(habitat == "Forest") %>%
  arrange(sum_cna) #arrange spp in descending order by representation level

forest_sps_44pct_comparison <- result_44pct_comparison %>%
  filter(habitat == "Forest") %>%
  arrange(sum_carbon) #arrange spp in descending order by representation level

# convert to factor to retain order of spp
forest_sps_37pct_comparison$species <- factor(forest_sps_37pct_comparison$species, forest_sps_37pct_comparison$species)
forest_sps_44pct_comparison$species <- factor(forest_sps_44pct_comparison$species, forest_sps_44pct_comparison$species)

#create a color gradient red to blue
library(scales)
n_colors <- nrow(forest_sps_37pct_comparison) #count of colors you need
red_blue_scale <- scales::seq_gradient_pal("red","blue","Lab")(seq(0,1,length.out=n_colors))

#plot CNA 37% results, forest spp (too many spp to see)
ggplot(forest_sps_37pct_comparison, aes(x = species, y = sum_cna*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 36, ymax = 38), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=37, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of forest species represented within critical natural assets") +
  theme(legend.position = "none")

#plot high carbon 44% results, forest spp (too many spp to see)
ggplot(forest_sps_44pct_comparison, aes(x = species, y = sum_carbon*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 43, ymax = 45), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=44, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of forest species represented within high carbon areas") +
  theme(legend.position = "none")


# repeat plotting for aridland species

# filter aridland species
arid_sps_37pct_comparison <- result_37pct_comparison %>%
  filter(habitat == "Aridlands") %>%
  arrange(sum_cna) #arrange spp in descending order by representation level

arid_sps_44pct_comparison <- result_44pct_comparison %>%
  filter(habitat == "Aridlands") %>%
  arrange(sum_carbon) #arrange spp in descending order by representation level

# convert to factor to retain order of spp
arid_sps_37pct_comparison$species <- factor(arid_sps_37pct_comparison$species, arid_sps_37pct_comparison$species)
arid_sps_44pct_comparison$species <- factor(arid_sps_44pct_comparison$species, arid_sps_44pct_comparison$species)

#create a color gradient red to blue
library(scales)
n_colors <- nrow(arid_sps_37pct_comparison) #count of colors you need
red_blue_scale <- scales::seq_gradient_pal("red","blue","Lab")(seq(0,1,length.out=n_colors))

#plot CNA 37% results, arid spp
ggplot(arid_sps_37pct_comparison, aes(x = species, y = sum_cna*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 36, ymax = 38), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=37, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of aridland species represented within critical natural assets") +
  theme(legend.position = "none")

#plot high carbon 44% results, arid spp
ggplot(arid_sps_44pct_comparison, aes(x = species, y = sum_carbon*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 43, ymax = 45), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=44, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of aridland species represented within high carbon areas") +
  theme(legend.position = "none")

# repeat plotting for grassland species

# filter grassland species
grass_sps_37pct_comparison <- result_37pct_comparison %>%
  filter(habitat == "Grasslands") %>%
  arrange(sum_cna) #arrange spp in descending order by representation level

grass_sps_44pct_comparison <- result_44pct_comparison %>%
  filter(habitat == "Grasslands") %>%
  arrange(sum_carbon) #arrange spp in descending order by representation level

# convert to factor to retain order of spp
grass_sps_37pct_comparison$species <- factor(grass_sps_37pct_comparison$species, grass_sps_37pct_comparison$species)
grass_sps_44pct_comparison$species <- factor(grass_sps_44pct_comparison$species, grass_sps_44pct_comparison$species)

#create a color gradient red to blue
library(scales)
n_colors <- nrow(grass_sps_37pct_comparison) #count of colors you need
red_blue_scale <- scales::seq_gradient_pal("red","blue","Lab")(seq(0,1,length.out=n_colors))

#plot CNA 37% results, grass spp
ggplot(grass_sps_37pct_comparison, aes(x = species, y = sum_cna*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 36.5, ymax = 37.5), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=37, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of grassland species represented within critical natural assets") +
  theme(legend.position = "none")

#plot high carbon 44% results, grass spp
ggplot(grass_sps_44pct_comparison, aes(x = species, y = sum_carbon*100)) +
  geom_rect(mapping = aes(xmin = -Inf, xmax = Inf, ymin = 43.4, ymax = 44.6), #based on random sampling
            col = "grey80", fill = "grey80", alpha = 0.5) +  
  geom_bar(aes(fill = species), stat="identity") +
  scale_fill_manual(values=red_blue_scale) +
  coord_flip() +
  geom_hline(yintercept=44, linetype="dashed") +
  theme_minimal() +
  ylab("Percent of grassland species represented within high carbon areas") +
  theme(legend.position = "none")

# calculate p-values
#load all results (combined and modified manually in Excel)
results_combined <- read_csv("outputs/results_combined_RAN.csv")

#what is the cutoff for >95% CI?
over_represented_threshold <- mean(results_combined$upper95_37pct)
over_represented_threshold #0.375977 or 37.6%  on average, but varies by species
max(results_combined$upper95_37pct) #maximum value is 0.465  so any bird represented more than 46.5%
min(results_combined$upper95_37pct)

#code from chatGPT:
# Assuming avg_37pct is the mean and stdev_37pct is the standard deviation
# and we want to calculate the p-value for an observed value of 0.75

mean_value <- mean(results_combined$avg_37pct, na.rm=T)
std_deviation <- mean(results_combined$stdev_37pct)
sample_size <- 1000
observed_value <- 0.75
observed_value2 <- 0.50

# Calculate the t-statistic
t_stat <- (observed_value - mean_value) / (std_deviation / sqrt(sample_size))

# Calculate the degrees of freedom
df <- sample_size - 1

# Calculate the p-value for an observed value of 0.75 or greater
p_value <- pt(t_stat, df, lower.tail = FALSE)

# Print the p-value
cat("P-Value (T-Test):", p_value, "\n") #p-value of 0.75 or greater is 0


# Calculate the p-value for an observed value of 0.50 or greater
t_stat2 <- (observed_value2 - mean_value) / (std_deviation / sqrt(sample_size))
p_value2 <- pt(t_stat, df, lower.tail = FALSE)

# Print the p-value
cat("P-Value (T-Test):", p_value2, "\n") #p-value of 0.50 or greater is 0

# code from Courtney to add p values to all spp
sample_size <- 1000
df <- sample_size - 1 # Calculate the degrees of freedom
p_value_df <- c()
for(i in 1:nrow(results_combined)){
  mean_value_37pct <- results_combined$avg_37pct[i]
  std_deviation_37pct <- results_combined$stdev_37pct[i]
  mean_value_44pct <- results_combined$avg_44pct[i]
  std_deviation_44pct <- results_combined$stdev_44pct[i]
  observed_value_37pct <- results_combined$sum_cna[i]
  observed_value_44pct <- results_combined$sum_carbon[i]
  # Calculate the t-statistic
  t_stat_37pct <- (observed_value_37pct - mean_value_37pct) / (std_deviation_37pct / sqrt(sample_size))
  t_stat_44pct <- (observed_value_44pct - mean_value_44pct) / (std_deviation_44pct / sqrt(sample_size))
  # Calculate the p-value for an observed value of 0.75 or greater
  p_value_37pct <- pt(abs(t_stat_37pct), df, lower.tail = FALSE)
  p_value_44pct <- pt(abs(t_stat_44pct), df, lower.tail = FALSE)
  p_value_df <- rbind(p_value_df, cbind(p_value_37pct, p_value_44pct))
}

results_combined_pval <- cbind(results_combined, p_value_df)


write.csv(results_combined_pval, "outputs/results_combined_pval.csv")

