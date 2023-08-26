# Create a random sample of 37% of grid cells of USA 
# cleaned up code August 22 2023

library(terra)
library(sf)
library(parallel)
library(future.apply)
library(data.table)
library(tictoc) #calculates time to run
library(beepr)

#load raster with bird pct population per species (479 species)
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

#load raster with bird pct population per group
pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

# load blank USA raster
usa_raster <- rast("data/usa_raster.tif")

#compare counts of non-NA pixels
freq_usa <- freq(usa_raster) # 1,073,267 pixels, but misses some coastal areas
freq_birds_groups <- freq(pct_pop_per_group_all) #different numbers of non-NA pixels in each group
freq_birds_abetow <- freq(pct_pop_per_sp_rast$abetow) #889,119 pixels in a single bird raster

#create a blank USA sampling raster that covers full extent of all bird species TIFs
new_US <- c(pct_pop_per_group_all, usa_raster)
new_US_added <- app(new_US, "sum", na.rm = T)
new_US_reclass <- ifel(new_US_added > 0, 1, NA)
freq_new_USA <- freq(new_US_reclass) #1,084,595 pixels so added about 10,000 pixels

#save raster as TIF file
writeRaster(new_US_reclass, "outputs/rasters/new_USA_raster.tif", overwrite=FALSE)

#calculate 37% (and 44%) of new USA raster
count_pixels_USA <- freq_new_USA[1,3] #1,084,595 pixels
sample_size_37pct <- 0.37*count_pixels_USA
sample_size_37pct #401300.2 pixels represent 37% of USA land area
sample_size_44pct <- 0.44*count_pixels_USA
sample_size_44pct #477221.8 pixels represent 44% of USA land area

# try using raster algebra -------------------

# create sampling function

dir_path <- "outputs/sampling_data/"

sampling_function <- function(i, dir_path, input_raster_path, sample_size, usa_raster_path){
  input_raster <- rast(input_raster_path)
  usa_raster <- rast(usa_raster_path)
  
  sample_usa <- spatSample(
    usa_raster, size=sample_size, method="random", replace = F, na.rm = T,
    as.points = T, values = T, warn = T, exhaustive = T)
  
  sample_usa_raster <- rasterize(sample_usa, usa_raster)
  
  samples_raster <- sample_usa_raster * input_raster
  samples_summaries_dt <- data.table(
    sps = names(input_raster),
    values = global(samples_raster, "sum", na.rm = T))
  
  samples_summaries_dt$iteration_n <- i
  fwrite(samples_summaries_dt,
         paste0(dir_path, "iteration_", i, ".csv"))
  return(samples_summaries_dt)
}

#try one iteration
tic()
sampling_function(
  i = 1, 
  dir_path = "outputs/sampling_data/", 
  input_raster_path = "outputs/rasters/pct_pop_per_sp_rast.tif",
  sample_size = 397108.8,
  usa_raster_path = "data/usa_raster.tif")
toc()
#453.54 sec elapsed

library(future.apply)
plan(multisession) ## Run in parallel on local computer

# try 30 iterations
tic()
test_list <- future_lapply(
  1:30, sampling_function, 
  dir_path = "outputs/sampling_data/", 
  input_raster_path = "outputs/rasters/pct_pop_per_sp_rast.tif",
  sample_size = 397109,
  usa_raster_path = "data/usa_raster.tif",
  future.seed=TRUE
)
toc()
#2001.72 sec elapsed to run 30 simulations (33.362 minutes)

# load results

temp = list.files(path = "outputs/sampling_data/_new_all/", pattern="*.csv", full.names = T)
myfiles = lapply(temp, data.table::fread)
myfiles[[1]] #check if worked
myfiles_df <- data.table::rbindlist(myfiles, idcol=T) #to put them into a single dataframe
# using idcol = T in rbindlist  will create a sequential number for each element of the list, so it should work
myfiles_mean_sd <- myfiles_df[, .(mean_var = mean(values.sum, na.rm = T), stdev_var = sd(values.sum, na.rm = T)), by = sps] 
#the group by is for species, not for each iteration... so instead of the by = column_with_teration_unique_id, it will be with the column of species_code

# read in tables with habitat groups, tipping point spp
library(tidyverse)
biome_sps_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome groups
tp_sps_vars <- readRDS("data/tp_final_species_selection.rds") #updated with tipping point spp
#bind these two tables
#sps_sel_all_vars <- rbind(biome_sps_sel_all_vars, tp_sps_sel_all_vars)

#select only columns of interest
names(biome_sps_vars)
biome_sps <- biome_sps_vars %>%
  select(species_code, sps_groups)
tp_sps <- tp_sps_vars %>%
  select(species_code, sps_groups)

#join tables
sampling_mean_sd_habitat <- left_join(myfiles_mean_sd, biome_sps, by=join_by(sps == species_code))
sampling_mean_sd_habitat_tp <- left_join(sampling_mean_sd_habitat, tp_sps, by=join_by(sps==species_code))

#write to csv
write.csv(sampling_mean_sd_habitat_tp, "outputs/sampling_mean_sd_habitat_tp.csv")

#bring in results from CNA analysis
pct_pop_tipping_pt_spp_cna <- read_csv("outputs/pct_pop_tipping_pt_spp_cna.csv")
sampling_cna_comparison_tp <- left_join(sampling_mean_sd_habitat_tp, pct_pop_tipping_pt_spp_cna, by=join_by(sps==species_code))
tp_sps_comparison <- sampling_cna_comparison_tp %>%
  filter(sps_groups.y=="Tipping Point")

#write to csv
write.csv(tp_sps_comparison, "outputs/tp_sps_sampling_cna_comparison.csv")

#sampling_cna_comparison_habitat <- left_join()