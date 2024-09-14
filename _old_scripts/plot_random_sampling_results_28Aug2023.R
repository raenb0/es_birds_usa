# Plotting results of random sampling
# September 5 2023

library(tidyverse)

#load random sampling data, all species, all iterations
result_10pct <- read_csv("outputs/random_sampling_result_10pct_100runs.csv") #test, only 100 runs
result_44pct <- read_csv("outputs/random_sampling_result_44pct.csv") #1000 runs
result_37pct <- read_csv("outputs/random_sampling_result_37pct.csv") #1000 runs


#load random sampling data, summarized by species, mean and SD
result_10pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_10pct_mean_sd_biomes.csv")
result_44pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_44pct_mean_sd_biomes.csv")
result_37pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_37pct_mean_sd_biomes.csv")

#load data summarized by habitat group
result_10pct_habitat <- read_csv("outputs/random_sampling_result_10pct_habitat_group.csv")
result_44pct_habitat <- read_csv("outputs/random_sampling_result_44pct_habitat_group.csv")
result_37pct_habitat <- read_csv("outputs/random_sampling_result_37pct_habitat_group.csv")

#define color palette with 62 colors
colors_62 <- c("#6f8e30",
               "#855bdb",
               "#65c14c",
               "#9b40b5",
               "#a5ba2e",
               "#4a65de",
               "#cfb033",
               "#cb6ce2",
               "#41912e",
               "#d84ab1",
               "#39c685",
               "#e4478e",
               "#489758",
               "#a63b91",
               "#71bf82",
               "#e53f68",
               "#5dd1bb",
               "#cb3938",
               "#41c2d1",
               "#d34e21",
               "#4971d2",
               "#d69230",
               "#9779e1",
               "#abb857",
               "#724fab",
               "#e27e2e",
               "#6299eb",
               "#918329",
               "#d482db",
               "#35743b",
               "#ad346d",
               "#91ba70",
               "#8a4e8f",
               "#576614",
               "#bba1e6",
               "#ca9f50",
               "#535a99",
               "#beb471",
               "#7882c5",
               "#a15626",
               "#65beed",
               "#e9715d",
               "#4bad8e",
               "#b83750",
               "#328867",
               "#da79b7",
               "#1a6447",
               "#dd708c",
               "#288f88",
               "#aa5244",
               "#4c9dcd",
               "#db966a",
               "#3176ae",
               "#87672d",
               "#9f77b4",
               "#56642b",
               "#e898bd",
               "#7c8a4f",
               "#ac638a",
               "#de8784",
               "#8d4463",
               "#9d4b55")

## plot 10pct results ------------

plot_10pct_habitats <- ggplot(result_10pct_habitat, aes(x=habitat, y=avg_representation, fill=habitat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg_representation-(1.96*stdev_representation), #95% CI
                    ymax=avg_representation+(1.96*stdev_representation), 
                    width=0.2))+
  ggtitle("Results of random sampling of 10% of land area, 100 runs, by habitat group")+
  ylim(0, 0.5)
plot_10pct_habitats

#plot 10pct result, Tipping Point species only
result_10pct_tipping_pt <- result_10pct_mean_sd_biomes %>%
  filter(tipping_pt=="Tipping Point")

plot_10pct_tipping_pt <- ggplot(result_10pct_tipping_pt, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 10% of land area, 100 runs, Tipping Pt spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_10pct_tipping_pt

#plot 10pct result, Aridland species only
result_10pct_aridlands <- result_10pct_mean_sd_biomes %>%
  filter(habitat=="Aridlands")

plot_10pct_aridlands <- ggplot(result_10pct_aridlands, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 10% of land area, 100 runs, Aridlands spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_10pct_aridlands



## plot 44pct results ------------
plot_44pct_habitats <- ggplot(result_44pct_habitat, aes(x=habitat, y=avg_representation, fill=habitat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg_representation-(1.96*stdev_representation), 
                    ymax=avg_representation+(1.96*stdev_representation), 
                    width=0.2))+
  ggtitle("Results of random sampling of 44% of land area, by habitat group")+
  ylim(0, 0.5)
plot_44pct_habitats

#plot 44pct result, Tipping Point species only
result_44pct_tipping_pt <- result_44pct_mean_sd_biomes %>%
  filter(tipping_pt=="Tipping Point")

plot_44pct_tipping_pt <- ggplot(result_44pct_tipping_pt, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 44% of land area, Tipping Pt spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_44pct_tipping_pt

#plot 44pct result, aridlands species only
result_44pct_aridlands <- result_44pct_mean_sd_biomes %>%
  filter(habitat=="Aridlands")

plot_44pct_aridlands <- ggplot(result_44pct_aridlands, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 44% of land area, Aridland spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_44pct_aridlands

#plot 44pct result, wetland species only
result_44pct_wetlands <- result_44pct_mean_sd_biomes %>%
  filter(habitat=="Water/wetland")

plot_44pct_wetlands <- ggplot(result_44pct_wetlands, aes(x=species, y=avg))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 44% of land area, Wetland/Water spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_44pct_wetlands


#plot 37pct result
library(ggplot2)
plot_37pct_habitats <- ggplot(result_37pct_habitat, aes(x=habitat, y=avg_representation, fill=habitat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg_representation-(1.96*stdev_representation), 
                    ymax=avg_representation+(1.96*stdev_representation), 
                    width=0.2))+
  ggtitle("Results of random sampling of 37% of land area, by habitat group")+
  ylim(0, 0.5)
plot_37pct_habitats

#plot 37pct result, Tipping Point species only
result_37pct_tipping_pt <- result_37pct_mean_sd_biomes %>%
  filter(tipping_pt=="Tipping Point")

plot_37pct_tipping_pt <- ggplot(result_37pct_tipping_pt, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 37% of land area, Tipping Pt spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_37pct_tipping_pt

#plot 37pct result, aridlands species only
result_37pct_aridlands <- result_37pct_mean_sd_biomes %>%
  filter(habitat=="Aridlands")

plot_37pct_aridlands <- ggplot(result_37pct_aridlands, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 37% of land area, Aridland spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  scale_fill_manual(values=colors_62)
plot_37pct_aridlands

#plot 37pct result, wetland species only
result_37pct_wetlands <- result_37pct_mean_sd_biomes %>%
  filter(habitat=="Water/wetland")

plot_37pct_wetlands <- ggplot(result_37pct_wetlands, aes(x=species, y=avg))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-(1.96*stdev), 
                    ymax=avg+(1.96*stdev), 
                    width=0.2))+
  ggtitle("Results of random sampling of 37% of land area, Wetland/Water spp")+
  ylim(0, 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_37pct_wetlands
