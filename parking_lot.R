# parking lot code

# Create an empty dataframe to store the results
guild_results_cna <- data.frame(Guild = character(), sum = numeric(), stringsAsFactors = FALSE)

# Loop through each SpatRaster object in the list
for (i in seq_along(pct_pop_per_group_list_cna)) {
  raster <- pct_pop_per_group_list_cna[[i]]
  
  # Calculate the sum of all pixels from all layers in the SpatRaster
  raster_sum <- global(raster, fun="sum", na.rm=TRUE)
  
  # Add the results to the dataframe
  guild_results_cna <- rbind(guild_results_cna, data.frame(Guild = names(pct_pop_per_group_list_cna)[i], sum = raster_sum))
}
# Reset row names
row.names(guild_results_cna) <- NULL

# Print the resulting dataframe
print(guild_results_cna)

# repeat calculating sums by guild, unmasked version
guild_results_all <- data.frame(Guild = character(), sum = numeric(), stringsAsFactors = FALSE)

# Loop through each SpatRaster object in the list
for (i in seq_along(pct_pop_per_group_list_all)) {
  raster <- pct_pop_per_group_list_all[[i]]
  
  # Calculate the sum of all pixels from all layers in the SpatRaster
  raster_sum <- global(raster, fun="sum", na.rm=TRUE)
  
  # Add the results to the dataframe
  guild_results_all <- rbind(guild_results_all, data.frame(Guild = names(pct_pop_per_group_list_all)[i], sum = raster_sum))
}

row.names(guild_results_all) <- NULL
print(guild_results_all) #these appear to be counts of species, not percentages of populations

#identify threshold value to define top 30% by land area ---------------
#threshold_top30_usa <- as_tibble(vuln_carbon_usa, na.rm = TRUE) %>%
#  slice_max(order_by = vuln_carbon_global, prop = .3) %>% #note layer name is still vuln_carbon_global
#  min()
#top30 threshold in USA: 66

# filter carbon data to only include top 30% #takes a long time, terminated
# tic()
# vuln_carbon_top30 <- vuln_carbon_usa %>% tidyterra::filter(vuln_carbon_global > threshold_top30_usa)
# toc()
# beep()

# check area
# area_usa <- expanse(vuln_carbon_usa, unit = "km")
# area_30perc <- expanse(vuln_carbon_top30, unit = "km")
# area_30perc / area_usa
# beep()

# save resulting raster
# writeRaster(vuln_carbon_top30, "outputs/rasters/vuln_carbon_top30_landarea.tif", overwrite=TRUE)

#trying to create a blank USA raster template
library(sf)
library(rnaturalearth)

# load USA boundary shapefile --------------
usa_boundary <- ne_download(
  scale = 50, type = "admin_1_states_provinces", returnclass = "sf") |>
  subset(iso_a2 == "US" & name != "Hawaii") |> st_union()
plot(usa_boundary)
st_crs(usa_boundary) #epsg 4326  WGS84

# re-project USA boundary to match ebird
usa_ebird <- st_transform(usa_boundary, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
plot(usa_ebird)

#   Transforming sf class to SpatVector class from terra
usa_vect = vect(usa_ebird)
plot(usa_vect)

# save transformed vector
writeVector(usa_vect, "outputs/rasters/usa_vect_ebird.shp", filetype="ESRI Shapefile", overwrite=TRUE)

# attempt to convert to a raster
# first make an empty raster
usa_grid = rast(nc, nrow=200, ncol=200)
# then rasterize usa_vect, use same nrows and ncols as above
usa_raster <- rast(usa_vect, nrows=1759, ncols=2414, vals=1) #not right
#try again, specifying the resolution instead of the nrows and ncols, try values 1 and 0
usa_raster <- rast(usa_vect, resolution=2962.808, vals=c(1,0))
plot(usa_raster) #not right

#try another method, using raster from Courtney
library(terra)
global_raster <- rast("data/srd_raster_template.tif")
crs(global_raster, describe=F, proj=T) #undefined
crs(global_raster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs" #set projection
crs(global_raster, describe=F, proj=T)
plot(global_raster)
ext(global_raster) #extent: 0, 1502, 0, 626 (xmin, xmax, ymin, ymax)

#crop to USA
crs(usa_vect, describe=F, proj=T) #double check CRS matches
ext(usa_vect) #extent: -12284348.7183238, -5134141.77866928, 2728987.81049671, 7940181.44761433 (xmin, xmax, ymin, ymax)
usa_rast <- mask(global_raster, usa_vect, updatevalue=1)
plot(usa_rast) #not right
ext(usa_rast)

#old version of Guillermo's code (doesn't sum by spp)
# sampling_function <- function(i, input_raster, sample_size){
#   sample_usa <- spatSample(
#     usa_raster, size=sample_size, method="random", replace=FALSE, na.rm=TRUE,
#     as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
#     xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
#   
#   sample_usa_vect <- vect(sample_usa, geom=c("x", "y"),
#                           crs = crs(usa_raster))
#   
#   sample_usa_df <-  extract(input_raster, sample_usa_vect)
#   
#   sample_usa_df$iteration_n <- i
#   data.table::fwrite(sample_usa_df,
#                      paste0(dir_path, "iteration_", i, ".csv"))
#   return(sample_usa_df)
# }

# library(tidyverse)
# iteration_1 <- read_csv("outputs/sampling_data/iteration_1.csv")
# names(iteration_1)
# ncol(iteration_1)
# 
# iteration_1_sum <- iteration_1 %>%
#   pivot_longer(cols = 2:480, names_to = "spp_code", values_to = "val") %>%
#   group_by(spp_code) %>%
#   summarise(sum = sum(val, na.rm=T))

write_csv(iteration_1_sum, "outputs/iteration_1_sum.csv")

#next old version:
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