
library(here)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(ggridges)


# Create long data frame for easy plotting
species_total_table <- read.csv(here("data/Species_Responsibility_Total_REFORMATTED_noSFW.csv"))
species_total_table_long_reformatted1 <- species_total_table[,c(1:4,6:10)]
species_total_table_long_reformatted2 <- na.omit(species_total_table[,c(1:3,5:10)])
colnames(species_total_table_long_reformatted1)[4] <- colnames(species_total_table_long_reformatted2)[4] <- "group"

species_total_table_long_reformatted_full <- rbind(species_total_table_long_reformatted1, species_total_table_long_reformatted2)


species_total_table_long <- melt(species_total_table_long_reformatted_full)
colnames(species_total_table_long)[c(5,6)] <- c("mngr","max_stewardship")

species_total_table_long2 <- species_total_table_long[!(species_total_table_long$mngr == "ANY"),]

species_total_table_long2_max <- species_total_table_long2 %>%
  group_by(group, mngr) %>%
  summarise(max = max(max_stewardship, na.rm=TRUE))

species_total_table_long2 <- na.omit(species_total_table_long2)
species_total_table_long2 <- species_total_table_long2[!species_total_table_long2$mngr == "SFW",]

for(i in 1:nrow(species_total_table_long2)){
  if (species_total_table_long2$max_stewardship[i] >= 0.25) {
    species_total_table_long2$percent25[i] <- 1
  } else {
    species_total_table_long2$percent25[i] <- 0}
}

species_total_table_long2_percent25 <- species_total_table_long2 %>%
  group_by(group, mngr) %>%
  summarise(percent.above.25 = (sum(percent25)/length(percent25))*100)


ggplot(species_total_table_long2, aes(x = max_stewardship*100, y = mngr, fill = group)) + 
  geom_density_ridges2(rel_min_height = 0.001) + 
  geom_vline(xintercept = 25, linetype = "dashed") +
  #geom_vline(xintercept = 0.50, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") + 
  xlim(0,100) +
  scale_y_discrete(expansion(add = c(0, 1))) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("lightyellow", "#117733","#999933","#DDDDDD","#CC6677","#88CCEE")) + xlab("Stewardship Responsibility - Maximum % of U.S. Population")

ggsave(here("figures","Stewardship_Summaries_Species-level.tiff"), width = 8, height = 7)