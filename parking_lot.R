# parking lot code



# # identify top 30% of USA land areas, by carbon value ------------------
# 
# # calculate how much land area is 30% of area of USA
# area_usa_30pct <- 0.3*area_usa
# area_usa_30pct # 2,831,240 sq km
# 
# # calculate how many 2km x 2km pixels this is
# area_usa_30pct / 4 #707810 2km pixels
# 
# #identify which carbon value corresponds to the 707810th pixel
# vuln_carbon_usa_df_sort$vuln_carbon_usa[707810] #51 tonnes (threshold for top30)
# 
# # reclassify USA vulnerable carbon to include only pixels with value greater than 51 (threshold)
# range(vuln_carbon_usa) #0, 535
# m30 <- c(0, 51, 0) #reclassify values from 0 to 51 to be 0, all other values left alone
# rcl_matrix30 <- matrix(m30, ncol=3, byrow=TRUE) #convert to a matrix
# vuln_carbon_usa_top30 <- classify(vuln_carbon_usa, rcl_matrix30, include.lowest=TRUE)
# 
# #look at result
# plot(vuln_carbon_usa, axes=F, main="Vulnerable carbon in USA (tonnes/ha)")
# plot(vuln_carbon_usa_top30, axes=F, main="Top 30% of land area for vulnerable carbon (tonnes / ha)")
# 
# # save raster representing top 30% of land area for USA vulnerable carbon
# writeRaster(vuln_carbon_usa_top30, "outputs/rasters/vuln_carbon_usa_top30.tif", overwrite=T)
# 
# # calculate how much carbon this represents
# sum_vuln_carbon_usa_top30 <- global(vuln_carbon_usa_top30, fun="sum",  na.rm=TRUE)
# sum_vuln_carbon_usa_top30 <- sum_vuln_carbon_usa_top30[1,1]
# sum_vuln_carbon_usa_top30 #60461519 tonnes
# sum_vuln_carbon_usa_top30 / sum_vuln_carbon_usa #0.7246803 so 72.5% of vulnerable caron
# 
# # reclassify top 30% of land areas for vulnerable carbon to 0/1 to use for masking bird data
# range(vuln_carbon_usa_top30) #0, 535
# m2 <- c(1, 535, 1) #reclassify values from 1 to 535 to be 1, leave other values alone
# rcl_matrix2 <- matrix(m2, ncol=3, byrow=TRUE) #convert to a matrix
# vuln_carbon_usa_top30_binary <- classify(vuln_carbon_usa_top30, rcl_matrix2, include.lowest=TRUE)
# plot(vuln_carbon_usa_top30_binary, axes=F, main="Top 30% of land areas for vulnerable carbon") #looks good
# 
# # save raster representing top30 of USA vulnerable carbon (0/1 version)
# writeRaster(vuln_carbon_usa_top30_binary, "outputs/rasters/vuln_carbon_usa_top30_binary.tif", overwrite=T)
# 
# #check area
# #area_usa <- expanse(usa_vect, unit = "km")
# #area_usa #9,437,467 sq km  Google says: 9.147 to 9.834 million km2 so close enough
# area_vuln_carbon_top30_binary <- expanse(vuln_carbon_usa_top30_binary, byValue=TRUE, unit = "km") # takes a minute, byValue gives you area of 0, 1
# beep()
# area_vuln_carbon_top30 <- area_vuln_carbon_top30_binary[2,3]
# area_vuln_carbon_top30 <- unname(area_vuln_carbon_top30) #removes name
# area_vuln_carbon_top30 # 2800423
# area_vuln_carbon_top30 / area_usa #0.2967347 or 29.7% close enough



# ## group bird spp by habitat groups within top 30% of land area for carbon
# sps_groups <- unique(sps_sel_all_vars$sps_groups)
# 
# tic()
# pct_pop_per_group_list_carbon_top30 <- lapply(sps_groups,
#                                               function(group_name){
#                                                 group_sps <- subset(
#                                                   sps_sel_all_vars, sps_groups == group_name)$species_code |> 
#                                                   sort()
#                                                 raster_names <- names(pct_pop_carbon_top30_mask) #replace this raster
#                                                 sel_species <- raster_names[raster_names %in% group_sps]
#                                                 group_raster <- subset(pct_pop_carbon_top30_mask, sel_species) #replace this raster
#                                                 pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
#                                                 return(pct_sps_per_group_cell)
#                                               })
# names(pct_pop_per_group_list_carbon_top30) <- sps_groups
# toc()
# beep()
# 
# # rasterize
# pct_pop_per_group_carbon_top30 <- rast(pct_pop_per_group_list_carbon_top30)
# 
# # save resulting raster
# writeRaster(pct_pop_per_group_carbon_top30, "outputs/rasters/pct_pop_per_guild_carbon_top30.tif", overwrite=TRUE)





# # repeat for top 30% of land areas for carbon ---------------
# 
# # pct pop of tipping point spp within top 30% carbon areas
# 
# tipping_pt_spp <- unique(sps_vars_tipping_point$species_code) #unique tipping pt spp codes
# raster_names <- names(pct_pop_carbon_top30_mask) #masked to 90% carbon areas
# sel_species <- raster_names[raster_names %in% tipping_pt_spp] #select tipping pt spp
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species) #subset tipping pt spp rasters only
# pct_pop_tipping_pt_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for tipping pt spp
# pct_pop_tipping_pt_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_tipping_pt_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_tipping_pt_spp_carbon_top30, "outputs/pct_pop_tipping_pt_spp_carbon_top30.csv")
# 
# # pct pop of forest spp within top 30% carbon areas #NOTE overwrites objects
# 
# forest_spp <- unique(sps_vars_forest$species_code) #unique forest spp codes
# raster_names <- names(pct_pop_carbon_top30_mask) #masked to 90% carbon areas
# sel_species <- raster_names[raster_names %in% forest_spp] #select forest spp
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species) #subset forest spp rasters only
# pct_pop_forest_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for forest spp
# pct_pop_forest_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_forest_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_forest_spp_carbon_top30, "outputs/pct_pop_forest_spp_carbon_top30.csv")
# 
# # pct pop of grassland spp within top 30% carbon areas #NOTE overwrites objects
# 
# grassland_spp <- unique(sps_vars_grassland$species_code)
# raster_names <- names(pct_pop_carbon_top30_mask)
# sel_species <- raster_names[raster_names %in% grassland_spp]
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species)
# pct_pop_grassland_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T)
# pct_pop_grassland_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_grassland_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_grassland_spp_carbon_top30, "outputs/pct_pop_grassland_spp_carbon_top30.csv")
# 
# # pct pop of aridland spp within top 30% carbon areas #NOTE overwrites objects
# 
# aridland_spp <- unique(sps_vars_aridland$species_code)
# raster_names <- names(pct_pop_carbon_top30_mask)
# sel_species <- raster_names[raster_names %in% aridland_spp]
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species)
# pct_pop_aridland_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T)
# pct_pop_aridland_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_aridland_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_aridland_spp_carbon_top30, "outputs/pct_pop_aridland_spp_carbon_top30.csv")
# 
# # pct pop of wetland spp within top 30% carbon areas #NOTE overwrites objects
# 
# wetland_spp <- unique(sps_vars_wetland$species_code)
# raster_names <- names(pct_pop_carbon_top30_mask)
# sel_species <- raster_names[raster_names %in% wetland_spp]
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species)
# pct_pop_wetland_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T)
# pct_pop_wetland_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_wetland_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_wetland_spp_carbon_top30, "outputs/pct_pop_wetland_spp_carbon_top30.csv")
# 
# # pct pop of generalist spp within top 30% carbon areas #NOTE overwrites objects
# 
# generalist_spp <- unique(sps_vars_generalist$species_code)
# raster_names <- names(pct_pop_carbon_top30_mask)
# sel_species <- raster_names[raster_names %in% generalist_spp]
# spp_raster <- subset(pct_pop_carbon_top30_mask, sel_species)
# pct_pop_generalist_spp_carbon_top30 <- global(spp_raster, fun='sum', na.rm=T)
# pct_pop_generalist_spp_carbon_top30 <- tibble::rownames_to_column(pct_pop_generalist_spp_carbon_top30, "species_code")
# 
# write_csv(pct_pop_generalist_spp_carbon_top30, "outputs/pct_pop_generalist_spp_carbon_top30.csv")


# # repeat for top 30% of areas for carbon -----------------
# # calculate percent of spp that are >90 >75 >50 pct represented
# 
# # tipping point spp
# pct_pop_tipping_pt_spp_carbon_top30 <- pct_pop_tipping_pt_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_tipping_pt_spp_carbon_top30 <- pct_pop_tipping_pt_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
#   mutate(guild="tipping_point")
# 
# # forest spp
# pct_pop_forest_spp_carbon_top30 <- pct_pop_forest_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_forest_spp_carbon_top30 <- pct_pop_forest_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
#   mutate(guild="forest")
# 
# # grassland spp
# pct_pop_grassland_spp_carbon_top30 <- pct_pop_grassland_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_grassland_spp_carbon_top30 <- pct_pop_grassland_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
#   mutate(guild="grassland")
# 
# # aridland spp
# pct_pop_aridland_spp_carbon_top30 <- pct_pop_aridland_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_aridland_spp_carbon_top30 <- pct_pop_aridland_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
#   mutate(guild="aridland")
# 
# # wetland spp
# pct_pop_wetland_spp_carbon_top30 <- pct_pop_wetland_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_wetland_spp_carbon_top30 <- pct_pop_wetland_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90, na.rm=T), more75 = mean(more75, na.rm=T), more50 = mean(more50, na.rm=T)) %>%
#   mutate(guild="wetland")
# 
# # generalist spp
# pct_pop_generalist_spp_carbon_top30 <- pct_pop_generalist_spp_carbon_top30 %>%
#   mutate(more90 = ifelse(sum>0.9,1,0)) %>%
#   mutate(more75 = ifelse(sum>0.75,1,0)) %>%
#   mutate(more50 = ifelse(sum>0.5,1,0))
# 
# summary_generalist_spp_carbon_top30 <- pct_pop_generalist_spp_carbon_top30 %>%
#   summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
#   mutate(guild="generalist")
# 
# # combine rows into single table
# summary_pct_pop_guild_carbon_top30 <- rbind(summary_tipping_pt_spp_carbon_top30, summary_forest_spp_carbon_top30, summary_grassland_spp_carbon_top30, summary_aridland_spp_carbon_top30, summary_wetland_spp_carbon_top30, summary_generalist_spp_carbon_top30)
# 
# write_csv(summary_pct_pop_guild_carbon_top30, "outputs/summary_pct_pop_guild_carbon_top30.csv")
# 
# # pivot to make tidy and plot top30 results
# library(ggplot2)
# 
# #load data if necessary
# summary_pct_pop_guild_carbon_top30 <- read_csv("outputs/summary_pct_pop_guild_carbon_top30.csv")
# 
# # make mutually exclusive categories for stacked bar chart
# summary_pct_pop_top30_mutuallyexclusive <- summary_pct_pop_guild_carbon_top30 %>%
#   mutate(more50_only = more50 - more75, more75_only = more75 - more90)
# summary_pct_pop_top30_select <- summary_pct_pop_top30_mutuallyexclusive %>%
#   select(guild, more50_only, more75_only, more90)
# 
# summary_longer_top30 <- pivot_longer(summary_pct_pop_top30_select, cols=2:4, names_to="category", values_to="pct_spp") #check column numbers!
# 
# plot_top30 <- ggplot(summary_longer_top30, aes(x=guild, y=pct_spp, fill=category)) +
#   geom_bar(stat="identity", position="stack") +
#   ggtitle("Percent of species represented within top 30% of areas for vulnerable carbon") +
#   xlab("Guild") +
#   ylab("Percent of species") +
#   scale_fill_discrete(labels=c('More than 50%', 'More than 75%', 'More than 90%')) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   theme(legend.title=element_blank())
# plot_top30



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

# third old version:
sampling_function <- function(i, input_raster, sample_size){
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

# revert to original code, doesn't summarize by species ----------------------
library(parallel)
library(future.apply)

plan(multisession) ## Run in parallel on local computer

dir_path <- "outputs/sampling_data/"

sampling_function <- function(i, input_raster, sample_size){
  sample_usa <- spatSample(
    usa_raster, size=sample_size, method="random", replace=FALSE, na.rm=TRUE,
    as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
    xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
  
  sample_usa_vect <- vect(sample_usa, geom=c("x", "y"),
                          crs = crs(usa_raster))
  
  sample_usa_df <-  extract(input_raster, sample_usa_vect)
  
  sample_usa_df$iteration_n <- i
  data.table::fwrite(sample_usa_df,
                     paste0(dir_path, "iteration_", i, ".csv"))
  return(sample_usa_df)
}

tic()
test_list <- future_lapply(
  1:3, sampling_function, 
  input_raster = pct_pop_per_sp_rast,
  sample_size = 397109,
  future.seed=TRUE
)
toc()
beep()

# random sampling code ----------------------

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
# pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

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

dir_path <- "outputs/sampling_data/"

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
                     paste0(dir_path, "iteration_", i, ".csv"))
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
tic()
sampling_function(i = 1, 
                  dir_path = dir_path, 
                  input_raster = pct_pop_per_sp_rast,
                  sample_size = 397109,
                  usa_raster = usa_raster)
toc()
beep()

# try old version ------------------------
plan(multisession) ## Run in parallel on local computer
dir_path <- "outputs/sampling_data/"

sampling_function <- function(i, input_raster, sample_size){
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
                     paste0(dir_path, "iteration_", i, ".csv"))
  return(samples_dt_long)
}

tic()
test_list <- future_lapply(
  1:3, sampling_function, 
  input_raster = pct_pop_per_sp_rast,
  sample_size = sample_size_37pct
)
toc()
beep()

# plot density curves (old version) -----------------

#load data if needed
result_37_pct_full <- read_csv("outputs/random_sampling_result_37pct_full.csv")
result_44_pct_full <- read_csv("outputs/random_sampling_result_44pct_full.csv")

#calculate group-level mean, SD of random sampling results (37%)
result_37_pct_group_summary <- result_37_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_37_pct_group_summary_confint <- result_37_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

#calculate group-level mean, SD of random sampling results (44%)
result_44_pct_group_summary <- result_44_pct_full %>%
  group_by(group) %>%
  summarize(sample_avg = mean(sample_result), sample_sd = sd(sample_result))

# add confidence intervals
result_44_pct_group_summary_confint <- result_44_pct_group_summary %>%
  mutate(lower95 = sample_avg - (1.96*sample_sd), upper95 = sample_avg + (1.96*sample_sd))

# 37% data

#plot random sampling results only, 37% data
ggplot(result_37_pct_full) +
  geom_density(aes(x = sample_result*100, fill = group))+
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(35,40) +
  ylim(0,5)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77", "#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by a random sample of 37% of USA land area")

#plot observed results only, 37% pct data
ggplot(result_37_pct_full) + 
  geom_density(aes(x=sum_cna*100, fill=group)) +
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by critical natural assets")

#plot both, 37% data
ggplot(result_37_pct_full) + 
  geom_density(aes(x=sum_cna*100, fill=group)) +
  geom_density(aes(x=sample_result*100)) +
  geom_vline(xintercept = 37, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high NCP areas, overlaid with random sampling result")

#plot 44% data

#plot random sampling results only, 44% data
ggplot(result_44_pct_full) +
  geom_density(aes(x = sample_result*100, fill = group))+
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(40,50) +
  ylim(0,5)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by a random sample of 44% of USA land area")

#plot observed results only, 44% pct data
ggplot(result_44_pct_full) + 
  geom_density(aes(x=sum_carbon*100, fill=group)) +
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high carbon areas")

#plot both, 44% data
ggplot(result_44_pct_full) + 
  geom_density(aes(x=sum_carbon*100, fill=group)) +
  geom_density(aes(x=sample_result*100)) +
  geom_vline(xintercept = 44, linetype = "dashed") +
  facet_wrap(~group, scales = "free_x") +
  xlim(0,100) +
  ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 5, l = 0))) + 
  scale_fill_manual(values = c("#CC6677", "#117733","#DDCC77","#AB9DEF","#882255","#88CCEE")) + 
  xlab("Percent of bird species represented by high carbon areas, overlaid with random sampling result")




