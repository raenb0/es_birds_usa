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

# create rij matrix to make bird spp data easier to work with
FIRST <- TRUE

tic()
if(FIRST){
  rij <- rij_matrix(new_USA, pct_pop_per_sp_rast)  #update with new_USA
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

