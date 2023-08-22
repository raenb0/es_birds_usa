# Create a random sample of 37% of grid cells of USA 
# cleaned up code August 22 2023

library(terra)
library(sf)
library(parallel)
library(future.apply)
library(data.table)
library(tictoc) #calculates time to run
library(beepr)

# try using raster algebra -------------------

#load raster with bird pct population per species (479 species)
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

# load blank USA raster
usa_raster <- rast("data/usa_raster.tif")

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

tic()
test_list <- future_lapply(
  1:20, sampling_function, 
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





