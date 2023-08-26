library(terra)
library(sf)
library(prioritizr)
library(Matrix)
library(tictoc)

setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa")

#load raster with bird pct population per species (479 species)
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

#load raster with bird pct population per group
pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

# load blank USA raster
usa_raster <- rast("outputs/rasters/new_usa_raster.tif")

FIRST <- FALSE

if(FIRST){
  rij <- rij_matrix(usa_raster, pct_pop_per_sp_rast)  
  rij %>% saveRDS("data/rij.rds", compress = FALSE) 
} else {
  rij <- readRDS("data/rij.rds")
}

# just checking that species information sums to 1
Matrix::rowSums(rij)

# vector from 1 to ncol rij
cvec <- 1:ncol(rij)


# Start sampling: this needs to go in a loop

# I want to randomly sample 44% of the pixels, this would be:
# 1084595*0.44 = 477221.8  or 477222 pixels per sample

#initialize a data frame to hold the results
result_44pct <- data.frame(matrix(nrow = 479, ncol = 1001))
colnames(result_44pct) <- c("species", paste0("iteration",c(1:1000))) #column names
result_44pct$species <- names(smpl_sums) # each row is a species

#for loop
tic()
for(i in 1:1000) {
  smpl <- sample(cvec, 477222) %>% sort()
  rij_red <- rij[,smpl]
  smpl_sums <- Matrix::rowSums(rij_red)
  result_44pct[, i+1] <- smpl_sums # assign smpl_sums to result matrix
}
toc()

library(tidyverse)
write_csv(result, "outputs/random_sampling_result_44pct.csv")

# To get 37% of the pixels
# 1084595*0.37 = 401300.15 or 401300 pixels per sample

#initialize a new data frame to hold the results
result_37pct <- data.frame(matrix(nrow = 479, ncol = 1001))
colnames(result_37pct) <- c("species", paste0("iteration",c(1:1000))) #column names
result_37pct$species <- names(smpl_sums) # each row is a species

#for loop
tic()
for(i in 1:1000) {
  smpl <- sample(cvec, 401300) %>% sort()
  rij_red <- rij[,smpl]
  smpl_sums <- Matrix::rowSums(rij_red)
  result_37pct[, i+1] <- smpl_sums # assign smpl_sums to result matrix
}
toc()

write_csv(result_37pct, "outputs/random_sampling_result_37pct.csv")

#calculate mean and SD for each species
#first, pivot 44pct results longer
result_44pct_longer <- pivot_longer(result_44pct, cols=iteration1:iteration1000, names_to="iteration", values_to="value")

result_44pct_mean_sd <- result_44pct_longer %>%
  group_by(species) %>%
  summarize(avg=mean(value, na.rm=T), stdev=sd(value, na.rm=T)) 

write_csv(result_44pct_mean_sd, "outputs/random_sampling_result_44pct_mean_sd.csv")

#repeat for 37pct
result_37pct_longer <- pivot_longer(result_37pct, cols=iteration1:iteration1000, names_to="iteration", values_to="value")

result_37pct_mean_sd <- result_37pct_longer %>%
  group_by(species) %>%
  summarize(avg=mean(value, na.rm=T), stdev=sd(value, na.rm=T)) 

write_csv(result_37pct_mean_sd, "outputs/random_sampling_result_37pct_mean_sd.csv")

# read in tables with habitat groups, tipping point spp
library(tidyverse)
biome_sps_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome groups
tp_sps_vars <- readRDS("data/tp_final_species_selection.rds") #updated with tipping point spp

#select only columns of interest
names(biome_sps_vars)
biome_sps <- biome_sps_vars %>%
  select(species_code, sps_groups)
tp_sps <- tp_sps_vars %>%
  select(species_code, sps_groups)

#join tables
result_44pct_mean_sd_biomes <- left_join(result_44pct_mean_sd, biome_sps, 
                                         by=join_by(species == species_code))
result_44pct_mean_sd_biomes_tp <- left_join(result_44pct_mean_sd_biomes, tp_sps, 
                                            by=join_by(species==species_code))
result_44pct_mean_sd_biomes_tp <- rename(result_44pct_mean_sd_biomes_tp, 
                                         habitat=sps_groups.x, 
                                         tipping_pt = sps_groups.y)

write_csv(result_44pct_mean_sd_biomes_tp, "outputs/random_sampling_result_44pct_mean_sd_biomes.csv")

#repeat for 37% results
result_37pct_mean_sd_biomes <- left_join(result_37pct_mean_sd, biome_sps, 
                                         by=join_by(species == species_code))
result_37pct_mean_sd_biomes_tp <- left_join(result_37pct_mean_sd_biomes, tp_sps, 
                                            by=join_by(species==species_code))
result_37pct_mean_sd_biomes_tp <- rename(result_37pct_mean_sd_biomes_tp, 
                                         habitat=sps_groups.x, 
                                         tipping_pt = sps_groups.y)

write_csv(result_37pct_mean_sd_biomes_tp, "outputs/random_sampling_result_37pct_mean_sd_biomes.csv")

#summarize by habitat
result_44pct_habitat <- result_44pct_mean_sd_biomes_tp %>%
  group_by(habitat) %>%
  summarize(avg_representation = mean(avg),
            stdev_representation = mean(stdev)) #ask Courtney: is this right? the mean of the standard deviations of the species in that habitat group?

#plot 44pct result
library(ggplot2)
plot_44pct_habitats <- ggplot(result_44pct_habitat, aes(x=habitat, y=avg_representation, fill=habitat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg_representation-stdev_representation, 
                    ymax=avg_representation+stdev_representation, 
                    width=0.2))
plot_44pct_habitats

#plot 44pct result, aridlands species only
result_44pct_aridlands <- result_44pct_mean_sd_biomes_tp %>%
  filter(habitat=="Aridlands")

plot_44pct_aridlands <- ggplot(result_44pct_aridlands, aes(x=species, y=avg, fill=species))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-stdev, 
                    ymax=avg+stdev, 
                    width=0.2))
plot_44pct_aridlands


#plot 44pct result, wetland species only
result_44pct_wetlands <- result_44pct_mean_sd_biomes_tp %>%
  filter(habitat=="Water/wetland")

plot_44pct_wetlands <- ggplot(result_44pct_wetlands, aes(x=species, y=avg))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=avg-stdev, 
                    ymax=avg+stdev, 
                    width=0.2))
plot_44pct_wetlands

plot(pct_pop_per_sp_rast, "abetow")
