# Create a random sample of 37% of grid cells of USA 
library(terra)
library(sf)
library(tictoc) #calculates time to run
library(beepr) #alerts you when run is complete

#links to data:

# USA raster
# https://cornell.box.com/s/mvsefr2rk261z37lqfznkl2fqipy0z7n

# single species TIF (for testing)
# https://cornell.box.com/s/jlaawl0jajeul2kwpuvlupc8h0024p50

# bird pct population per group (6 groups, for testing)
# https://cornell.box.com/s/x0d6xkbsclpvgbw89e8wwtbfzv2hjsqr

# bird pct population per species (479 species, LARGE FILE)
# https://cornell.box.com/s/4tun1x451b2f96phq0o2acvjk3eyq524

#load raster with bird pct population per group (6 groups)
pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

#load raster with bird pct population per species (479 species)
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

# load blank USA raster
usa_raster <- rast("data/usa_raster.tif")

#count pixels in USA raster
count_pixels_usa <- global(usa_raster, fun="notNA") #1073267
sample_size_37pct <- count_pixels_usa[1,1]*0.37 #397108.8 this is equivalent to high NCP areas
sample_size_44pct <- count_pixels_usa[1,1]*0.44 #472237.5 this is equivalent to high carbon areas

# # create a spatial sample from the blank USA raster equivalent to 37% of land area
# sample1_usa <- spatSample(usa_raster, size=397109, method="random", replace=FALSE, na.rm=TRUE,
#                           as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
#                           xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
# 
# # convert USA sample to vector
# sample1_usa_vect <- sample1_usa |> 
#   st_as_sf(coords = c("x", "y"), crs = crs(usa_raster)) |> 
#   vect()
# 
# #extract values from bird guilds data for sample points (6 guilds)
# tic()
# test_df <- extract(pct_pop_per_group_all, sample1_usa_vect)
# toc() #2 seconds
# head(test_df, 10)
# 
# #extract values from bird species data for sample points (479 species)
# tic()
# test_df_sp <- extract(pct_pop_per_sp_rast, sample1_usa_vect) 
# toc() #73 seconds
# beep()
# head(test_df_sp, 10)

# code from Guillermo
library(terra)
library(sf)
library(parallel)
library(future.apply)
library(data.table)
library(tictoc) #calculates time to run
library(beepr)

#load raster with bird pct population per group (6 groups)
# pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

#load raster with bird pct population per species (479 species)
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

# load blank USA raster
usa_raster <- rast("data/usa_raster.tif")

dir_path <- "outputs/sampling_data"

sampling_function <- function(i, dir_path, input_raster, sample_size, usa_raster){
  sample_usa <- spatSample(
    usa_raster, size=sample_size, method="random", replace=FALSE, na.rm=TRUE,
    as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
    xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
  
  sample_usa_vect <- vect(sample_usa, geom=c("x", "y"),
                          crs = crs(usa_raster))
  
  samples_dt <-  extract(input_raster, sample_usa_vect) |> 
    as.data.table()
  
  cols <- colnames(samples_dt)
  cols_sel <- cols[2:length(cols)]
  
  samples_dt_summary <- samples_dt[
    , lapply(.SD, sum, na.rm = T), .SDcols = cols_sel
  ] 
  
  samples_dt_long <- melt(
    samples_dt_summary, measure.vars = cols_sel, 
    variable.name = "species", value.name = "summary")
  
  samples_dt_long$iteration_n <- i
  data.table::fwrite(samples_dt_long,
                     paste0(dir_path, "/iteration_", i, ".csv"))
  return(samples_dt_long)
}

plan(multisession) ## Run in parallel on local computer
tic()
test_list <- future_lapply(
  1:3, sampling_function,
  dir_path = dir_path,
  input_raster = pct_pop_per_sp_rast,
  usa_raster = usa_raster,
  future.seed = TRUE
)
toc()
beep()

#test outside parallelization
sampling_function(i = 1, 
                  dir_path = dir_path, 
                  input_raster = pct_pop_per_sp_rast,
                  sample_size = 397109,
                  usa_raster = usa_raster)

# try old version ------------------------

sampling_function <- function(i, input_raster, sample_size){
  sample_usa <- spatSample(
    usa_raster, size=sample_size, method="random", replace=FALSE, na.rm=TRUE,
    as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
    xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
  
  sample_usa_vect <- vect(sample_usa, geom=c("x", "y"),
                          crs = crs(usa_raster))
  
  samples_dt <-  terra::extract(input_raster, sample_usa_vect) |> 
    as.data.table()
  
  cols <- colnames(samples_dt)
  cols_sel <- cols[2:length(cols)]
  
  samples_dt_summary <- samples_dt[
    , lapply(.SD, sum, na.rm = T), .SDcols = cols_sel
  ] 
  
  samples_dt_long <- melt(
    samples_dt_summary, measure.vars = cols_sel, 
    variable.name = "species", value.name = "summary")
  
  samples_dt_long$iteration_n <- i
  data.table::fwrite(samples_dt_long,
                     paste0(dir_path, "iteration_", i, ".csv"))
  return(samples_dt_long)
}

plan(multisession) ## Run in parallel on local computer
tic()
test_list <- future_lapply(
  1:3, sampling_function, 
  input_raster = pct_pop_per_sp_rast,
  sample_size = 397109
)
toc()
beep()
