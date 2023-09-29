# bar plots for bird species represented in CNA and high carbon areas
# Sept 25 2023

# create bar plots with CNA results ---------------------
library(tidyverse)

#load data
summary_pct_pop_guild_cna <- read_csv("outputs/summary_pct_pop_guild_cna_16Sept2023.csv")

# make mutually exclusive categories for stacked bar chart
summary_pct_pop_guild_cna_mutuallyexclusive <- summary_pct_pop_guild_cna %>%
  mutate(more37_only = more37 - more50, more50_only = more50 - more75)
summary_pct_pop_guild_cna_select <- summary_pct_pop_guild_cna_mutuallyexclusive %>%
  select(guild, n, more37_only, more50_only, more75)

summary_longer_cna <- pivot_longer(summary_pct_pop_guild_cna_select, cols=3:5, names_to="category", values_to="pct_spp")

#add a column for spp counts
summary_longer_cna <- summary_longer_cna %>%
  mutate(spp_counts = pct_spp*n) %>%
  mutate(spp_counts = as.integer(spp_counts))

#add a column for n_spp per guild
summary_longer_cna <- summary_longer_cna %>%
  mutate(guild_n = paste0(guild," (n=", n, ")"))

#plot CNA results (stacked bar plot)
plot_cna <- ggplot(summary_longer_cna, aes(x=guild_n, y=pct_spp, fill=category)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = spp_counts), size = 3, position = position_stack(vjust = 0.5)) +
  ggtitle("Percent of species represented within critical natural assets, by guild") +
  xlab("Guild") +
  ylab("Percent of species") +
  scale_fill_discrete(labels=c('More than 37%', 'More than 50%', 'More than 75%'), 
                      type=c("#DDCC77", "#88CCEE", "#44AA99")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.title=element_blank())
plot_cna


# create bar plots with high carbon (90pct of carbon) results

#load data
summary_pct_pop_guild_carbon_90pct <- read_csv("outputs/summary_pct_pop_guild_carbon_21Sep2023.csv")

#add column for n species
n_spp <- summary_pct_pop_guild_cna %>%
  select(guild, n)
summary_pct_pop_guild_carbon_90pct <- left_join(summary_pct_pop_guild_carbon_90pct, n_spp, by="guild")

# make mutually exclusive categories for stacked bar chart
summary_pct_pop_carbon_mutuallyexclusive <- summary_pct_pop_guild_carbon_90pct %>%
  mutate(more44_only = more44 - more50, more50_only = more50 - more75)
summary_pct_pop_carbon_select <- summary_pct_pop_carbon_mutuallyexclusive %>%
  select(guild, n, more44_only, more50_only, more75)

summary_longer_carbon <- pivot_longer(summary_pct_pop_carbon_select, cols=3:5, names_to="category", values_to="pct_spp") #check column numbers!

#add a column for spp counts
summary_longer_carbon <- summary_longer_carbon %>%
  mutate(spp_counts = pct_spp*n) %>%
  mutate(spp_counts = as.integer(spp_counts))

#add a column for n_spp per guild
summary_longer_carbon <- summary_longer_carbon %>%
  mutate(guild_n = paste0(guild," (n=", n, ")"))

# plot carbon results (stacked bar plot)
plot_carbon <- ggplot(summary_longer_carbon, aes(x=guild_n, y=pct_spp, fill=category)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = spp_counts), size = 3, position = position_stack(vjust = 0.5)) +
  ggtitle("Percent of species represented within high carbon areas, by guild") +
  xlab("Guild") +
  ylab("Percent of species") +
  scale_fill_discrete(labels=c('More than 44%', 'More than 50%', 'More than 75%'), 
                      type=c("#DDCC77", "#88CCEE", "#44AA99")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.title=element_blank())
plot_carbon
