# Identify top 30% of areas in USA for vulnerable carbon (30% of land area)
# Also, identify areas containing 90% of USA's vulnerable carbon (not by land area)
# June 15 2023

library(terra)
library(tidyterra)
library(dplyr)
library(beepr)
library(tictoc)

#load vulnerable carbon (global) ---------------------------
setwd("C:/Users/raenb/Box/Documents/GIS/EcosystemServices_ChaplinKramer/reprojected_resampled_Eckert2km")
vuln_carbon_global <- rast("Vulnerable_C_Total_2018_WARPED_average_MASKED_md5_3233d4cc77cd819670e69881b5db50d4.tif")
setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa") #reset wd

#check resolution, projection
res(vuln_carbon_global) # 2km
crs(vuln_carbon_global, describe=F, proj=T) #"+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
names(vuln_carbon_global) <- "vuln_carbon_global" #rename layer

#identify threshold value (30%) #note this is global 30% threshold
min_value <- as_tibble(vuln_carbon_global, na.rm = TRUE) %>%
  slice_max(order_by = vuln_carbon_global, prop = .3) %>%
  min()

# mask vulnerable carbon to USA ------------------------

library(sf)
library(rnaturalearth)

# load USA boundary shapefile --------------
usa_boundary <- ne_download(
  scale = 50, type = "admin_1_states_provinces", returnclass = "sf") |>
  subset(iso_a2 == "US" & name != "Hawaii") |> st_union()
plot(usa_boundary)
class(usa_boundary)
st_bbox(usa_boundary)

st_crs(usa_boundary) #epsg 4326  WGS84

# re-project USA boundary to match carbon raster
usa_transform <- st_transform(usa_boundary, "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(usa_transform)
st_bbox(usa_transform)
# ebird projection
# usa_ebird <- st_transform(usa_boundary, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
# plot(usa_ebird)

#   Transforming sf class to SpatVector class from terra
usa_vect = vect(usa_transform)
plot(usa_vect)
ext(usa_vect)

#   Masking in terra (use crop instead of mask to get correct spatial extent)
vuln_carbon_usa = crop(vuln_carbon_global, usa_vect, mask=TRUE)
plot(vuln_carbon_global)
plot(vuln_carbon_usa, axes=F, main = "Vulnerable carbon in USA (tonnes / ha)")

# save USA vulnerable carbon raster
writeRaster(vuln_carbon_usa, "outputs/rasters/vuln_carbon_usa.tif", overwrite=FALSE)


#identify areas containing 90% of total USA vulnerable carbon ---------------------
sum_vuln_carbon_usa <- global(vuln_carbon_usa, fun="sum",  na.rm=TRUE)
sum_vuln_carbon_usa <- sum_vuln_carbon_usa[1,1]
sum_vuln_carbon_usa # 83431990
sum_vuln_carbon_usa_90pct <- 0.9*sum_vuln_carbon_usa
sum_vuln_carbon_usa_90pct #75088791

# convert the vulnerable carbon raster into a dataframe
vuln_carbon_usa_df <- as.data.frame(vuln_carbon_usa)

# sort the dataframe by pixel value
vuln_carbon_usa_df_sort <- vuln_carbon_usa_df %>%
  arrange(desc(vuln_carbon_global))

#create a new column with a cumulative sum of the pixel values
vuln_carbon_usa_df_sort <- vuln_carbon_usa_df_sort %>%
  mutate(cum_sum = cumsum(vuln_carbon_global))

#find the pixel value where you reach the 90% of the overall sum
which(vuln_carbon_usa_df_sort$cum_sum > sum_vuln_carbon_usa_90pct) #first position 1055054
threshold_90pct_usa <- vuln_carbon_usa_df_sort$vuln_carbon_global[1055054]
threshold_90pct_usa #31 tonnes per ha is threshold

# reclassify USA vulnerable carbon to include only pixels with value greater than 31 (threshold)
range(vuln_carbon_usa) #0, 535
m <- c(0, 31, 0) #reclassify values from 0 to 31 to be 0, all other values left alone
rcl_matrix <- matrix(m, ncol=3, byrow=TRUE) #convert to a matrix
vuln_carbon_usa_90pct <- classify(vuln_carbon_usa, rcl_matrix, include.lowest=TRUE)

#look at result
plot(vuln_carbon_global, axes=F, main="Vulnerable carbon (tonnes/ha)")
plot(vuln_carbon_usa, axes=F, main="Vulnerable carbon in USA (tonnes/ha)")
plot(vuln_carbon_usa_90pct, axes=F, main="Areas containing 90% of vulnerable carbon (tonnes / ha)")
ext(vuln_carbon_usa_90pct)

# save raster representing 90pct of USA vulnerable carbon
writeRaster(vuln_carbon_usa_90pct, "outputs/rasters/vuln_carbon_usa_90pct.tif", overwrite=T)

# check if sum adds up to 90 pct
sum_vuln_carbon_usa_90pct_check <- global(vuln_carbon_usa_90pct, fun="sum",  na.rm=TRUE)
sum_vuln_carbon_usa_90pct_check <- sum_vuln_carbon_usa_90pct_check[1,1]
sum_vuln_carbon_usa_90pct - sum_vuln_carbon_usa_90pct_check #difference: 406,183 (>90% but OK)

# reclassify areas with 90% of vulnerable carbon to 0/1 to use for masking bird data
range(vuln_carbon_usa) #0, 535
m2 <- c(1, 535, 1) #reclassify values from 1 to 535 to be 1, leave other values alone
rcl_matrix2 <- matrix(m2, ncol=3, byrow=TRUE) #convert to a matrix
vuln_carbon_usa_90pct_binary <- classify(vuln_carbon_usa_90pct, rcl_matrix2, include.lowest=TRUE)
plot(vuln_carbon_usa_90pct_binary, axes=F, main="Areas containing 90% of vulnerable carbon") #looks good

# save raster representing 90pct of USA vulnerable carbon
writeRaster(vuln_carbon_usa_90pct_binary, "outputs/rasters/vuln_carbon_usa_90pct_binary.tif", overwrite=T)

#check area
area_usa <- expanse(usa_vect, unit = "km")
area_usa #9,437,467 sq km  Google says: 9.147 to 9.834 million km2 so close enough
area_vuln_carbon_90pct_binary <- expanse(vuln_carbon_usa_90pct_binary, byValue=TRUE, unit = "km") # takes a minute, byValue gives you area of 0, 1
beep()
area_vuln_carbon_90pct <- area_vuln_carbon_90pct_binary[2,3]
area_vuln_carbon_90pct <- unname(area_vuln_carbon_90pct) #removes name
area_vuln_carbon_90pct / area_usa #0.4439892 or 44% of the land area

# identify top 30% of USA land areas, by carbon value ------------------

# calculate how much land area is 30% of area of USA
area_usa_30pct <- 0.3*area_usa
area_usa_30pct # 2,831,240 sq km

# calculate how many 2km x 2km pixels this is
area_usa_30pct / 4 #707810 2km pixels

#identify which carbon value corresponds to the 707810th pixel
vuln_carbon_usa_df_sort$vuln_carbon_global[707810] #51 tonnes (threshold for top30)

# reclassify USA vulnerable carbon to include only pixels with value greater than 51 (threshold)
range(vuln_carbon_usa) #0, 535
m30 <- c(0, 51, 0) #reclassify values from 0 to 51 to be 0, all other values left alone
rcl_matrix30 <- matrix(m30, ncol=3, byrow=TRUE) #convert to a matrix
vuln_carbon_usa_top30 <- classify(vuln_carbon_usa, rcl_matrix30, include.lowest=TRUE)

#look at result
plot(vuln_carbon_usa, axes=F, main="Vulnerable carbon in USA (tonnes/ha)")
plot(vuln_carbon_usa_top30, axes=F, main="Top 30% of land area for vulnerable carbon (tonnes / ha)")

# save raster representing top 30% of land area for USA vulnerable carbon
writeRaster(vuln_carbon_usa_top30, "outputs/rasters/vuln_carbon_usa_top30.tif", overwrite=T)

# calculate how much carbon this represents
sum_vuln_carbon_usa_top30 <- global(vuln_carbon_usa_top30, fun="sum",  na.rm=TRUE)
sum_vuln_carbon_usa_top30 <- sum_vuln_carbon_usa_top30[1,1]
sum_vuln_carbon_usa_top30 #60461519 tonnes
sum_vuln_carbon_usa_top30 / sum_vuln_carbon_usa #0.7246803 so 72.5% of vulnerable caron

# reclassify top 30% of land areas for vulnerable carbon to 0/1 to use for masking bird data
range(vuln_carbon_usa_top30) #0, 535
m2 <- c(1, 535, 1) #reclassify values from 1 to 535 to be 1, leave other values alone
rcl_matrix2 <- matrix(m2, ncol=3, byrow=TRUE) #convert to a matrix
vuln_carbon_usa_top30_binary <- classify(vuln_carbon_usa_top30, rcl_matrix2, include.lowest=TRUE)
plot(vuln_carbon_usa_top30_binary, axes=F, main="Top 30% of land areas for vulnerable carbon") #looks good

# save raster representing top30 of USA vulnerable carbon
writeRaster(vuln_carbon_usa_top30_binary, "outputs/rasters/vuln_carbon_usa_top30_binary.tif", overwrite=T)

#check area
#area_usa <- expanse(usa_vect, unit = "km")
#area_usa #9,437,467 sq km  Google says: 9.147 to 9.834 million km2 so close enough
area_vuln_carbon_top30_binary <- expanse(vuln_carbon_usa_top30_binary, byValue=TRUE, unit = "km") # takes a minute, byValue gives you area of 0, 1
beep()
area_vuln_carbon_top30 <- area_vuln_carbon_top30_binary[2,3]
area_vuln_carbon_top30 <- unname(area_vuln_carbon_top30) #removes name
area_vuln_carbon_top30 # 2800423
area_vuln_carbon_top30 / area_usa #0.2967347 or 29.7% close enough

# Calculate contribution of these areas to bird populations

# load the percent population per species raster --------------------
library(data.table)
library(stringr)
library(terra)
library(dplyr)
library(tictoc)
library(beepr)

sps_sel_all_vars <- readRDS("data/final_species_selection.rds") #update filepath
pct_pop_files <- list.files("data/pct_pop_sps",  #update filepath
                            pattern = ".tif", full.names = T)

tiffs_sps <- str_extract(pct_pop_files, "(?<=pct_pop_sps\\/)(.*)(?=_a)") #update filepath
tifs_path_sel <- pct_pop_files[tiffs_sps %in% sps_sel_all_vars$species_code]
sps_sel <- tiffs_sps[tiffs_sps %in% sps_sel_all_vars$species_code]

pct_pop_per_sp_rast <- rast(tifs_path_sel)
names(pct_pop_per_sp_rast) <- sps_sel

#re-project and resample to get carbon layers to match birds data ------------------

# re-project and resample 90pct carbon binary layer
carbon_90pct_reproject <- project(vuln_carbon_usa_90pct_binary, pct_pop_per_sp_rast, method="near")
carbon_90pct_3km <- resample(carbon_90pct_reproject, pct_pop_per_sp_rast, method="near")
crs(carbon_90pct_3km, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
res(carbon_90pct_3km) #2962.809 good
plot(carbon_90pct_3km) #distorted but matches birds data

# re-project and re-sample top30 carbon binary layer
carbon_top30_reproject <- project(vuln_carbon_usa_top30_binary, pct_pop_per_sp_rast, method="near")
carbon_top30_3km <- resample(carbon_top30_reproject, pct_pop_per_sp_rast, method="near")
crs(carbon_top30_3km, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
res(carbon_top30_3km) #2962.809 good
plot(carbon_top30_3km) #distorted but matches birds data

# mask bird data with 90pct carbon data (takes 5 minutes) -------------------
tic()
pct_pop_carbon_90pct_mask <- mask(pct_pop_per_sp_rast, carbon_90pct_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
toc() #5 min
beep()

# mask bird data with top30 carbon data (takes a few minutes) -------------------
tic()
pct_pop_carbon_top30_mask <- mask(pct_pop_per_sp_rast, carbon_top30_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
toc()
beep()

#save masked versions (takes a few min, only do this once)
tic()
writeRaster(pct_pop_carbon_90pct_mask, "outputs/rasters/pct_pop_carbon_90pct_mask.tif", overwrite=F)
toc() #2.3 min
beep()

tic()
writeRaster(pct_pop_carbon_top30_mask, "outputs/rasters/pct_pop_carbon_top30_mask.tif", overwrite=TRUE)
toc() #2 min
beep()

# group bird tifs, masked to important carbon areas, by guild -----------------------

## 90% of carbon
sps_groups <- unique(sps_sel_all_vars$sps_groups)

tic()
pct_pop_per_group_list_carbon_90pct <- lapply(sps_groups,
                                     function(group_name){
                                       group_sps <- subset(
                                         sps_sel_all_vars, sps_groups == group_name)$species_code |> 
                                         sort()
                                       raster_names <- names(pct_pop_carbon_90pct_mask) #replace this raster
                                       sel_species <- raster_names[raster_names %in% group_sps]
                                       group_raster <- subset(pct_pop_carbon_90pct_mask, sel_species) #replace this raster
                                       pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
                                       return(pct_sps_per_group_cell)
                                     })
names(pct_pop_per_group_list_carbon_90pct) <- sps_groups
toc()
beep()

# rasterize
pct_pop_per_group_carbon_90pct <- rast(pct_pop_per_group_list_carbon_90pct)

# save resulting raster
writeRaster(pct_pop_per_group_carbon_90pct, "outputs/rasters/pct_pop_per_guild_carbon_90pct.tif", overwrite=TRUE)

## top 30% of land area for carbon
sps_groups <- unique(sps_sel_all_vars$sps_groups)

tic()
pct_pop_per_group_list_carbon_top30 <- lapply(sps_groups,
                                              function(group_name){
                                                group_sps <- subset(
                                                  sps_sel_all_vars, sps_groups == group_name)$species_code |> 
                                                  sort()
                                                raster_names <- names(pct_pop_carbon_top30_mask) #replace this raster
                                                sel_species <- raster_names[raster_names %in% group_sps]
                                                group_raster <- subset(pct_pop_carbon_top30_mask, sel_species) #replace this raster
                                                pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
                                                return(pct_sps_per_group_cell)
                                              })
names(pct_pop_per_group_list_carbon_top30) <- sps_groups
toc()
beep()

# rasterize
pct_pop_per_group_carbon_top30 <- rast(pct_pop_per_group_list_carbon_top30)

# save resulting raster
writeRaster(pct_pop_per_group_carbon_top30, "outputs/rasters/pct_pop_per_guild_carbon_top30.tif", overwrite=TRUE)

# look at outputs (carbon 90% areas)
plot(pct_pop_per_group_carbon_90pct, "Water/wetland", main="Sum of Water/wetland bird populations % within carbon 90% areas")
plot(pct_pop_per_group_carbon_90pct, "Forest", main="Sum of Forest bird populations % within carbon 90% areas")
plot(pct_pop_per_group_carbon_90pct, "Aridlands", main="Sum of Aridlands bird populations % within carbon 90% areas")
plot(pct_pop_per_group_carbon_90pct, "Grasslands", main="Sum of Grasslands bird populations % within carbon 90% areas")
plot(pct_pop_per_group_carbon_90pct, "Habitat Generalist", main="Sum of Habitat Generalist bird populations % within carbon 90% areas")
plot(pct_pop_per_group_cna, "Tipping Point", main="Sum of Tipping Point bird populations % within carbon 90% areas")