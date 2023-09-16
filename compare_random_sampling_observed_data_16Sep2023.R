# compare random sampling results to actual results
# September 16 2023

library(tidyverse)

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
