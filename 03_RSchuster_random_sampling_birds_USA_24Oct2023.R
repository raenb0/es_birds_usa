# Random sampling, birds and ES in USA
# Richard Schuster and Rachel Neugarten
# October 24 2023

library(data.table)
library(stringr)
library(terra)
library(dplyr)
library(tictoc)
library(beepr)

# load the  birds (percent population per species) corrected rasters --------------------

# sps_sel_all_vars <- readRDS("data/final_species_selection.rds") #old RDS file
biome_sps_sel_all_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome groups
tp_sps_sel_all_vars <- readRDS("data/tp_final_species_selection.rds") #updated with tipping point spp
#bind these two tables
sps_sel_all_vars <- rbind(biome_sps_sel_all_vars, tp_sps_sel_all_vars)

#load corrected bird spp population abundance rasters
pct_pop_files <- list.files("data/pct_pop_corrected", #updated file path 
                            pattern = ".tif", full.names = T) #lists spp tiffs filenames

tiffs_sps <- str_extract(pct_pop_files, "(?<=pct_pop_corrected\\/)(.*)(?=_a)") #simplifies tiffs names, updated file path 
tifs_path_sel <- pct_pop_files[tiffs_sps %in% sps_sel_all_vars$species_code] #selects 479 files
sps_sel <- tiffs_sps[tiffs_sps %in% sps_sel_all_vars$species_code] #selects 479 spp

pct_pop_per_sp_rast <- rast(tifs_path_sel) #creates raster
names(pct_pop_per_sp_rast) <- sps_sel #names layers in raster by spp codes

#double check if spp abundances all add up to 1
# calculate sum of pixels (takes 1 minute)
tic()
pct_pop_per_spp_all_sum <- global(pct_pop_per_sp_rast, fun='sum', na.rm=T)
toc()

# Random sampling -------------------------------------

library(terra)
library(sf)
library(prioritizr)
library(Matrix)
library(tictoc) #for tracking how much time a process takes

setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa")

#load raster with bird pct population per group (6 groups, for testing)
# pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

#create a blank USA sampling raster that covers full extent of all bird species TIFs
# updated with corrected bird spp layers, new_USA raster, double checked Oct 25 2023
new_US <- c(pct_pop_per_sp_rast, new_USA)
new_US_added <- app(new_US, "sum", na.rm = T)
new_US_reclass <- ifel(new_US_added > 0, 1, NA)

#save raster as TIF file
writeRaster(new_US_reclass, "outputs/rasters/new_USA_raster_25Oct2023.tif", overwrite=TRUE)

ext(new_US_reclass)
ext(new_USA) #appear the same
freq_new_US_reclass <- freq(new_US_reclass) #1086943
freq_new_USA <- freq(new_USA) #1084595 not the same**
new_USA <- new_US_reclass #use updated version for below

#load raster with bird pct population per species, if needed (479 species)
# pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

# load blank USA raster if necessary
# new_USA <- rast("outputs/rasters/new_usa_raster_25Oct2023.tif")

#check raster for a single species
plot(pct_pop_per_sp_rast, "abetow")
plot(pct_pop_per_sp_rast, "arcwar1")

#check USA raster
plot(new_USA)

# create rij matrix to make bird spp data easier to work with #got an error msg
FIRST <- FALSE #update this to TRUE

tic()
if(FIRST){
  rij <- rij_matrix(new_USA, pct_pop_per_sp_rast)  #update with new_USA, error
  rij %>% saveRDS("data/rij.rds", compress = FALSE) 
} else {
  rij <- readRDS("data/rij.rds")
}
toc()

# just checking that species information sums to 1 
# something is wrong - ***arcwar1 sums to 0***
Matrix::rowSums(rij)

# vector from 1 to ncol rij
cvec <- 1:ncol(rij)


# Start sampling ----------------------

# Test 10% of the pixels, only 100 runs
# 1084595*0.1 = 108459.5 or 108460 pixels per sample

#initialize a new data frame to hold the results
names(pct_pop_per_sp_rast)
result_10pct <- data.frame(matrix(nrow = 479, ncol = 101))
colnames(result_10pct) <- c("species", paste0("iteration",c(1:100))) #column names
result_10pct$species <- names(pct_pop_per_sp_rast) # each row is a species

#for loop
tic()
for(i in 1:100) {
  smpl <- sample(cvec, 108460) %>% sort()
  rij_red <- rij[,smpl]
  smpl_sums <- Matrix::rowSums(rij_red)
  result_10pct[, i+1] <- smpl_sums # assign smpl_sums to result matrix
}
toc()

write_csv(result_10pct, "outputs/random_sampling_result_10pct_100runs.csv")

# check how many values are 0 for birds
library(dplyr)

tt <- read.csv("outputs/random_sampling_result_10pct_100runs.csv") %>%
  mutate(sum = Matrix::rowSums(rij),
         cnt_0 = Matrix::rowSums(rij == 0)) %>% #returns count of 0 values for each spp
  dplyr::select(species, sum, cnt_0) # results look OK

# I want to randomly sample 44% of the pixels in the USA, this would be:
# 1084595*0.44 = 477221.8  or 477222 pixels per sample

#initialize a data frame to hold the results
result_44pct <- data.frame(matrix(nrow = 479, ncol = 1001))
colnames(result_44pct) <- c("species", paste0("iteration",c(1:1000))) #column names
result_44pct$species <- names(pct_pop_per_sp_rast) # each row is a species

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
result_37pct$species <- names(pct_pop_per_sp_rast) # each row is a species

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


# calculate mean, SD, and confidence intervals

#load data if necessary
library(tidyverse)

#load random sampling data, all species, all iterations
result_10pct <- read_csv("outputs/random_sampling_result_10pct_100runs.csv") #test, only 100 runs
result_44pct <- read_csv("outputs/random_sampling_result_44pct.csv") #1000 runs
result_37pct <- read_csv("outputs/random_sampling_result_37pct.csv") #1000 runs

# calculate mean and SD for each species, 10pct results -------------------

#first, pivot 10pct results longer
result_10pct_longer <- pivot_longer(result_10pct, cols=iteration1:iteration100, names_to="iteration", values_to="value")

result_10pct_mean_sd <- result_10pct_longer %>%
  group_by(species) %>%
  summarize(avg=mean(value, na.rm=T), stdev=sd(value, na.rm=T)) 

# add confidence intervals
result_10pct_mean_sd_confint <- result_10pct_mean_sd %>%
  mutate(lower95 = avg - (1.96*stdev), upper95 = avg + (1.96*stdev))

write_csv(result_10pct_mean_sd_confint, "outputs/random_sampling_result_10pct_100runs_mean_sd_confint.csv")

# calculate mean and SD for each species, 44pct results -------------------
#first, pivot 44pct results longer
result_44pct_longer <- pivot_longer(result_44pct, cols=iteration1:iteration1000, names_to="iteration", values_to="value")

result_44pct_mean_sd <- result_44pct_longer %>%
  group_by(species) %>%
  summarize(avg=mean(value, na.rm=T), stdev=sd(value, na.rm=T)) 

# add confidence intervals
result_44pct_mean_sd_confint <- result_44pct_mean_sd %>%
  mutate(lower95 = avg - (1.96*stdev), upper95 = avg + (1.96*stdev))

write_csv(result_44pct_mean_sd_confint, "outputs/random_sampling_result_44pct_mean_sd_confint.csv")

#calculate mean and SD for each species, 37pct results ------------------
result_37pct_longer <- pivot_longer(result_37pct, cols=iteration1:iteration1000, names_to="iteration", values_to="value")

result_37pct_mean_sd <- result_37pct_longer %>%
  group_by(species) %>%
  summarize(avg=mean(value, na.rm=T), stdev=sd(value, na.rm=T)) 

# add confidence intervals
result_37pct_mean_sd_confint <- result_37pct_mean_sd %>%
  mutate(lower95 = avg - (1.96*stdev), upper95 = avg + (1.96*stdev))

write_csv(result_37pct_mean_sd_confint, "outputs/random_sampling_result_37pct_mean_sd_confint.csv")


# read in tables with habitat groups, tipping point spp
library(tidyverse)
biome_sps_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome groups
tp_sps_vars <- readRDS("data/tp_final_species_selection.rds") #updated with tipping point spp

#select only columns of interest
names(biome_sps_vars)
biome_sps <- biome_sps_vars %>%
  dplyr::select(species_code, sps_groups)
tp_sps <- tp_sps_vars %>%
  dplyr::select(species_code, sps_groups)

#join biome and tipping point columns to results tables

# 10% results
result_10pct_mean_sd_biomes <- left_join(result_10pct_mean_sd, biome_sps,  #add habitats
                                         by=join_by(species == species_code))
result_10pct_mean_sd_biomes <- left_join(result_10pct_mean_sd_biomes, tp_sps, #add TP species
                                         by=join_by(species==species_code))
result_10pct_mean_sd_biomes <- rename(result_10pct_mean_sd_biomes, 
                                      habitat=sps_groups.x, 
                                      tipping_pt = sps_groups.y)

write_csv(result_10pct_mean_sd_biomes, "outputs/random_sampling_result_10pct_mean_sd_biomes.csv")

# repeat for 44% results
result_44pct_mean_sd_biomes <- left_join(result_44pct_mean_sd, biome_sps,  #add habitats
                                         by=join_by(species == species_code))
result_44pct_mean_sd_biomes <- left_join(result_44pct_mean_sd_biomes, tp_sps,  #add TP species
                                         by=join_by(species==species_code))
result_44pct_mean_sd_biomes <- rename(result_44pct_mean_sd_biomes, 
                                      habitat=sps_groups.x, 
                                      tipping_pt = sps_groups.y)

write_csv(result_44pct_mean_sd_biomes, "outputs/random_sampling_result_44pct_mean_sd_biomes.csv")

#repeat for 37% results
result_37pct_mean_sd_biomes <- left_join(result_37pct_mean_sd, biome_sps,  #add habitats
                                         by=join_by(species == species_code))
result_37pct_mean_sd_biomes <- left_join(result_37pct_mean_sd_biomes, tp_sps, #add TP species
                                         by=join_by(species==species_code))
result_37pct_mean_sd_biomes <- rename(result_37pct_mean_sd_biomes, 
                                      habitat=sps_groups.x, 
                                      tipping_pt = sps_groups.y)

write_csv(result_37pct_mean_sd_biomes_tp, "outputs/random_sampling_result_37pct_mean_sd_biomes.csv")

#join species results with habitat data, confidence intervals --------------

#load data if necessary
result_37pct_mean_sd_confint <- read_csv("outputs/random_sampling_result_37pct_mean_sd_confint.csv")
result_44pct_mean_sd_confint <- read_csv("outputs/random_sampling_result_44pct_mean_sd_confint.csv")
result_37pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_37pct_mean_sd_biomes.csv")
result_44pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_44pct_mean_sd_biomes.csv")


# select columns of interest and join tables
names(result_37pct_mean_sd_biomes)
result_37pct_biomes <- result_37pct_mean_sd_biomes %>%
  select(species, habitat, tipping_pt)
result_37pct_join <- left_join(result_37pct_mean_sd_confint, result_37pct_biomes, by="species")

result_44pct_biomes <- result_44pct_mean_sd_biomes %>%
  select(species, habitat, tipping_pt)
result_44pct_join <- left_join(result_44pct_mean_sd_confint, result_44pct_biomes, by="species")

write_csv(result_37pct_join, "outputs/random_sampling_result_37pct_mean_sd_confint_biomes.csv")
write_csv(result_44pct_join, "outputs/random_sampling_result_44pct_mean_sd_confint_biomes.csv")

#summarize 10% results by habitat

#load data if necessary
result_10pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_10pct_mean_sd_biomes.csv")

result_10pct_habitat <- result_10pct_mean_sd_biomes %>%
  group_by(habitat) %>%
  summarize(avg_representation = mean(avg),
            stdev_representation = mean(stdev)) #ask Courtney: is this right? the mean of the standard deviations of the species in that habitat group?

write_csv(result_10pct_habitat, "outputs/random_sampling_result_10pct_habitat_group.csv")

#summarize 44% results by habitat

#load data if necessary
result_44pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_44pct_mean_sd_biomes.csv")

result_44pct_habitat <- result_44pct_mean_sd_biomes %>%
  group_by(habitat) %>%
  summarize(avg_representation = mean(avg),
            stdev_representation = mean(stdev)) #ask Courtney: is this right? the mean of the standard deviations of the species in that habitat group?

write_csv(result_44pct_habitat, "outputs/random_sampling_result_44pct_habitat_group.csv")

#summarize 37% results by habitat

#load data if necessary
result_37pct_mean_sd_biomes <- read_csv("outputs/random_sampling_result_37pct_mean_sd_biomes.csv")

result_37pct_habitat <- result_37pct_mean_sd_biomes %>%
  group_by(habitat) %>%
  summarize(avg_representation = mean(avg),
            stdev_representation = mean(stdev)) #ask Courtney: is this right? the mean of the standard deviations of the species in that habitat group?

write_csv(result_37pct_habitat, "outputs/random_sampling_result_37pct_habitat_group.csv")

