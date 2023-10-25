# Identify top 30% of areas in USA for vulnerable carbon (30% of land area)
# Also, identify areas containing 90% of USA's vulnerable carbon (not by land area)
# Bird populations represented in high carbon areas
# Oct 25 2023

library(terra)
library(tidyterra)
library(dplyr)
library(beepr)
library(tictoc)

# below code only needs to be run once -----------------------
#load vulnerable carbon (global) ---------------------------
vuln_carbon_global <- rast("data/carbon/Vulnerable_C_Total_2018.tif")

#check resolution, projection
res(vuln_carbon_global) # 2km
crs(vuln_carbon_global, describe=F, proj=T) #"+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
names(vuln_carbon_global) <- "vuln_carbon_global" #rename layer

# mask vulnerable carbon to USA ------------------------
# NOTE this might cut off coastal carbon (mangroves), see below

# library(sf)
# library(rnaturalearth)
# 
# # load USA boundary shapefile --------------
# usa_boundary <- ne_download(
#   scale = 50, type = "admin_1_states_provinces", returnclass = "sf") |>
#   subset(iso_a2 == "US" & name != "Hawaii") |> st_union()
# plot(usa_boundary)
# class(usa_boundary)
# st_bbox(usa_boundary)
# 
# st_crs(usa_boundary) #epsg 4326  WGS84
# 
# # re-project USA boundary to match carbon raster
# usa_transform <- st_transform(usa_boundary, "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# plot(usa_transform)
# st_bbox(usa_transform)
# 
# # ebird projection
# # usa_ebird <- st_transform(usa_boundary, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
# # plot(usa_ebird)
# 
# #   Transforming sf class to SpatVector class from terra
# usa_vect = vect(usa_transform)
# plot(usa_vect)
# ext(usa_vect)
# crs(usa_vect)
# crs(vuln_carbon_global)
# 
# # save vector
# writeVector(usa_vect, "outputs/rasters/usa_vect_eck4.shp", filetype="ESRI Shapefile", overwrite=TRUE)
# 
# #  Crop global vulnerable carbon to USA (use crop instead of mask to get correct spatial extent)
# vuln_carbon_usa = crop(vuln_carbon_global, usa_vect, mask=TRUE)
# names(vuln_carbon_usa) <- "vuln_carbon_usa" #rename layer
# ext(vuln_carbon_usa) #-14025202.923, -5517202.923, 3175398.539, 7831398.539 (xmin, xmax, ymin, ymax)
# plot(vuln_carbon_global)
# plot(vuln_carbon_usa, axes=F, main = "Vulnerable carbon in USA (tonnes / ha)")
# 
# # save USA vulnerable carbon raster
# writeRaster(vuln_carbon_usa, "outputs/rasters/vuln_carbon_usa.tif", overwrite=TRUE)

# crop global vulnerable carbon using bird rasters, to make sure we aren't missing coastal areas --------

#load data if necessary
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")
ext(pct_pop_per_sp_rast)

#re-project and resample carbon layer to match birds data ------------------
carbon_3km <- project(vuln_carbon_global, pct_pop_per_sp_rast, method="near")
plot(carbon_3km)
carbon_3km <- resample(carbon_3km, pct_pop_per_sp_rast, method="near")
plot(carbon_3km)

#mask carbon data with bird data to get vulnerable carbon within USA (takes 3 min)
#results in multiple layers, doesn't work
# tic()
# carbon_usa <- mask(carbon_3km, pct_pop_per_sp_rast) 
# toc()
# beep()
#
# plot(carbon_usa)


# try again, using clean USA raster developed from birds data
# load new USA raster
new_USA <- rast("outputs/rasters/new_USA_raster.tif")

#mask carbon data with new USA data to get vulnerable carbon within USA (takes 3 min)
carbon_usa <- crop(carbon_3km, new_USA, mask=TRUE)
names(carbon_usa) <- "carbon_usa" #rename layer

plot(carbon_usa)

#save raster as TIF file
writeRaster(carbon_usa, "outputs/rasters/carbon_all_usa.tif", overwrite=TRUE)




#identify areas containing 90% of total USA vulnerable carbon ---------------------
sum_vuln_carbon_usa <- global(carbon_usa, fun="sum",  na.rm=TRUE) #updated
sum_vuln_carbon_usa <- sum_vuln_carbon_usa[1,1]
sum_vuln_carbon_usa # 83,431,990 originally, now 37,996,532, diff: 45,435,458
sum_vuln_carbon_usa_90pct <- 0.9*sum_vuln_carbon_usa
sum_vuln_carbon_usa_90pct #75,088,791 originally, now 34,196,879

# convert the vulnerable carbon raster into a dataframe
vuln_carbon_usa_df <- as.data.frame(carbon_usa)

# sort the dataframe by pixel value
vuln_carbon_usa_df_sort <- vuln_carbon_usa_df %>%
  arrange(desc(carbon_usa))

#create a new column with a cumulative sum of the pixel values
vuln_carbon_usa_df_sort <- vuln_carbon_usa_df_sort %>%
  mutate(cum_sum = cumsum(carbon_usa))

#plot curve #takes time
#plot(vuln_carbon_usa_df_sort$cum_sum, main="Cumulative sum of vulnerable carbon value")

#find the pixel value where you reach the 90% of the overall sum
which(vuln_carbon_usa_df_sort$cum_sum > sum_vuln_carbon_usa_90pct) #first position 1055054 originally, now 480101
threshold_90pct_usa <- vuln_carbon_usa_df_sort$carbon_usa[480101]
threshold_90pct_usa #31 tonnes per ha is still the threshold

# reclassify USA vulnerable carbon to include only pixels with value greater than 31 (threshold)
range(carbon_usa) #0, 535 originally, now 0 to 509
m <- c(0, 31, 0) #reclassify values from 0 to 31 to be 0, all other values left alone
rcl_matrix <- matrix(m, ncol=3, byrow=TRUE) #convert to a matrix
carbon_usa_90pct <- classify(carbon_usa, rcl_matrix, include.lowest=TRUE)

#look at result
plot(vuln_carbon_global, axes=F, main="Vulnerable carbon (tonnes/ha)")
plot(carbon_usa, axes=F, main="Vulnerable carbon in USA (tonnes/ha)")
plot(carbon_usa_90pct, axes=F, main="Areas containing 90% of vulnerable carbon (tonnes / ha)")
ext(carbon_usa_90pct)

# save raster representing 90pct of USA vulnerable carbon
writeRaster(carbon_usa_90pct, "outputs/rasters/carbon_usa_90pct.tif", overwrite=T)

# check if sum adds up to 90 pct
sum_vuln_carbon_usa_90pct_check <- global(carbon_usa_90pct, fun="sum",  na.rm=TRUE)
sum_vuln_carbon_usa_90pct_check <- sum_vuln_carbon_usa_90pct_check[1,1]
sum_vuln_carbon_usa_90pct - sum_vuln_carbon_usa_90pct_check #difference: 406,183 (>90% but OK) originally, now 184,086.8 so still >90% but diff is smaller

# reclassify areas with 90% of vulnerable carbon to 0/1 to use for masking bird data
range(carbon_usa) #0, 535 now 0, 509
m2 <- c(1, 509, 1) #reclassify values from 1 to 509 to be 1, leave other values alone
rcl_matrix2 <- matrix(m2, ncol=3, byrow=TRUE) #convert to a matrix
carbon_usa_90pct_binary <- classify(carbon_usa_90pct, rcl_matrix2, include.lowest=TRUE)
plot(carbon_usa_90pct_binary, axes=F, main="Areas containing 90% of vulnerable carbon") #looks good

# save raster representing 90pct of USA vulnerable carbon (0/1 version)
writeRaster(carbon_usa_90pct_binary, "outputs/rasters/carbon_usa_90pct_binary.tif", overwrite=T)

#check area of USA and area of high carbon areas
#area_usa <- expanse(usa_vect, unit = "km")
# area_usa <- 9437467 #9,437,467 sq km  Google says: 9.147 to 9.834 million km2 so close enough
area_usa <- expanse(new_USA, unit = "km")
area_usa #9593197  or 9,593,197 sq km
area_vuln_carbon_90pct_binary <- expanse(carbon_usa_90pct_binary, byValue=TRUE, unit = "km") # takes a minute, byValue gives you area of 0, 1
beep()
area_vuln_carbon_90pct <- area_vuln_carbon_90pct_binary[2,3]
area_vuln_carbon_90pct <- unname(area_vuln_carbon_90pct) #removes name
area_vuln_carbon_90pct / area_usa #0.4439892 or 44% of the land area originally, updated: 0.437179  or 44% with updated USA raster (close enough)


# mask bird data with 90pct carbon data (takes 4 minutes) -------------------
# updated with new carbon_usa_90pct_binary (updated to full extent of birds data)
tic()
pct_pop_carbon_mask <- mask(pct_pop_per_sp_rast, carbon_usa_90pct_binary, maskvalues=c(0,NA)) #trying to mask in values=1 only
toc() #5 min
beep()

plot(pct_pop_carbon_mask$abetow)
plot(pct_pop_carbon_mask$arcwar1)

#save masked versions (takes a few min, only do this once)
tic()
writeRaster(pct_pop_carbon_mask, "outputs/rasters/pct_pop_carbon_mask.tif", overwrite=TRUE)
toc() #2 min
beep()



# Calculate contribution of these areas to bird populations --------------------------

# load the percent population per species raster --------------------
library(data.table)
library(stringr)
library(terra)
library(dplyr)
library(tictoc)
library(beepr)

#sps_sel_all_vars <- readRDS("data/final_species_selection.rds") #update filepath
biome_sps_sel_all_vars <- readRDS("data/biome_final_species_selection.rds") #updated with biome spp
tp_sps_sel_all_vars <- readRDS("data/tp_final_species_selection.rds") #updated with tipping point spp
#bind these two tables
sps_sel_all_vars <- rbind(biome_sps_sel_all_vars, tp_sps_sel_all_vars)

pct_pop_files <- list.files("data/pct_pop_corrected",  #update filepath
                            pattern = ".tif", full.names = T)

tiffs_sps <- str_extract(pct_pop_files, "(?<=pct_pop_corrected\\/)(.*)(?=_a)") #update filepath
tifs_path_sel <- pct_pop_files[tiffs_sps %in% sps_sel_all_vars$species_code]
sps_sel <- tiffs_sps[tiffs_sps %in% sps_sel_all_vars$species_code]

#create raster
pct_pop_per_sp_rast <- rast(tifs_path_sel)
names(pct_pop_per_sp_rast) <- sps_sel

# #re-project and resample to get carbon layers to match birds data ------------------
# # (only run this once)
# 
# #load data if necessary
# vuln_carbon_usa_90pct_binary <- rast("outputs/rasters/vuln_carbon_usa_90pct_binary.tif") #focus on 90%
# #vuln_carbon_usa_top30_binary <- rast("outputs/rasters/vuln_carbon_usa_top30_binary.tif")
# 
# # re-project and resample 90pct carbon binary layer
# carbon_90pct_reproject <- project(vuln_carbon_usa_90pct_binary, pct_pop_per_sp_rast, method="near")
# carbon_90pct_3km <- resample(carbon_90pct_reproject, pct_pop_per_sp_rast, method="near")
# crs(carbon_90pct_3km, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
# res(carbon_90pct_3km) #2962.809 good
# plot(carbon_90pct_3km) #distorted but matches birds data
# 
# #save to raster
# writeRaster(carbon_90pct_3km, "outputs/rasters/vuln_carbon_usa_90pct_binary_3km.tif", overwrite=F)

# re-project and re-sample top30 carbon binary layer
# carbon_top30_reproject <- project(vuln_carbon_usa_top30_binary, pct_pop_per_sp_rast, method="near")
# carbon_top30_3km <- resample(carbon_top30_reproject, pct_pop_per_sp_rast, method="near")
# crs(carbon_top30_3km, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
# res(carbon_top30_3km) #2962.809 good
# plot(carbon_top30_3km) #distorted but matches birds data

# # mask bird data with 90pct carbon data (takes 3 minutes) -------------------
# tic()
# pct_pop_carbon_90pct_mask <- mask(pct_pop_per_sp_rast, carbon_90pct_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
# toc() #5 min
# beep()

# # mask bird data with top30 carbon data (takes a few minutes) -------------------
# tic()
# pct_pop_carbon_top30_mask <- mask(pct_pop_per_sp_rast, carbon_top30_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
# toc()
# beep()

# #save masked versions (takes a few min, only do this once)
# tic()
# writeRaster(pct_pop_carbon_90pct_mask, "outputs/rasters/pct_pop_carbon_90pct_mask.tif", overwrite=T)
# toc() #2 min
# beep()

# tic()
# writeRaster(pct_pop_carbon_top30_mask, "outputs/rasters/pct_pop_carbon_top30_mask.tif", overwrite=TRUE)
# toc() #2 min
# beep()

# mask bird data with high carbon areas 90% (takes a few minutes) -------------------
#note this includes the non-cropped version to ensure we don't lose coastal areas
tic()
pct_pop_carbon_mask <- mask(pct_pop_per_sp_rast, carbon_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
toc()
beep()


# group bird tifs, masked to important carbon areas, by guild -----------------------

# load data if necessary
pct_pop_carbon_mask <- rast("outputs/rasters/pct_pop_carbon_mask.tif")

## 90% of carbon
sps_groups <- unique(sps_sel_all_vars$sps_groups)
#tp_sps_groups <- unique(tp_sps_sel_all_vars$sps_groups) #tipping point spp

tic()
pct_pop_per_group_list_carbon <- lapply(sps_groups,
                                     function(group_name){
                                       group_sps <- subset(
                                         sps_sel_all_vars, sps_groups == group_name)$species_code |> 
                                         sort()
                                       raster_names <- names(pct_pop_carbon_mask) #replace
                                       sel_species <- raster_names[raster_names %in% group_sps]
                                       group_raster <- subset(pct_pop_carbon_mask, sel_species) #replace 
                                       pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
                                       return(pct_sps_per_group_cell)
                                     })
names(pct_pop_per_group_list_carbon) <- sps_groups
toc()
beep()

# rasterize
pct_pop_per_group_carbon <- rast(pct_pop_per_group_list_carbon)

# save resulting raster
writeRaster(pct_pop_per_group_carbon, "outputs/rasters/pct_pop_per_group_carbon.tif", overwrite=TRUE)

# look at outputs (carbon 90% areas)

# load data if necessary
# pct_pop_per_group_carbon  <- rast("outputs/rasters/pct_pop_per_group_carbon.tif")

plot(pct_pop_per_group_carbon, "Water/wetland", main="Sum of Water/wetland birds within high carbon areas", axes=F)
plot(pct_pop_per_group_carbon, "Forest", main="Sum of Forest birds within high carbon areas", axes=F)
plot(pct_pop_per_group_carbon, "Aridlands", main="Sum of Aridlands bird populations % within high carbon areas", axes=F)
plot(pct_pop_per_group_carbon, "Grasslands", main="Sum of Grasslands bird populations % within high carbon areas", axes=F)
plot(pct_pop_per_group_carbon, "Habitat Generalist", main="Sum of Habitat Generalist bird populations % within high carbon areas", axes=F)
plot(pct_pop_per_group_carbon, "Tipping Point", main="Sum of Tipping Point bird populations % within high carbon areas", axes=F)


# re-project bird guild data, masked to carbon 90%, to Eckert IV (for visualization) -----------

# load data if necessary
pct_pop_per_group_carbon_90pct <- rast("outputs/rasters/pct_pop_per_group_carbon_90pct.tif")
crs(pct_pop_per_group_carbon_90pct, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
ext(pct_pop_per_group_carbon_90pct)

#project to Eckert IV "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
pct_pop_per_group_carbon_90pct_eck4 <- project(pct_pop_per_group_carbon_90pct, "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", method="near")
names(pct_pop_per_group_carbon_90pct_eck4) <- sps_groups #name layers
ext(pct_pop_per_group_carbon_90pct_eck4)

#save re-projected raster
writeRaster(pct_pop_per_group_carbon_90pct_eck4, "outputs/rasters/pct_pop_per_group_carbon_90pct_eck4.tif", overwrite=TRUE)
pct_pop_per_group_carbon_90pct_eck4




# calculate pct of population within high carbon areas for all spp -------------------
#updated with corrected bird spp layers, corrected carbon 90pct areas

# load bird tifs raster, masked to high carbon areas, if necessary
pct_pop_carbon_mask <- rast("outputs/rasters/pct_pop_carbon_mask.tif")

# calculate sum of pixels (takes 1 minute)
tic()
pct_pop_per_spp_carbon_sum <- global(pct_pop_carbon_mask, fun='sum', na.rm=T)
toc()

#rowname to column
pct_pop_per_spp_carbon_sum <- tibble::rownames_to_column(pct_pop_per_spp_carbon_sum, "species")
pct_pop_per_spp_carbon_sum <- pct_pop_per_spp_carbon_sum %>%
  rename("sum_carbon" = "sum") #rename "sum" to "sum_cna" for clarity

library(tidyverse)
write_csv(pct_pop_per_spp_carbon_sum, "outputs/pct_pop_per_spp_carbon_sum_25Oct2023.csv") #save to csv


# calculate pct of population within high carbon areas for spp in each group ------------------
library(tidyverse)

#filter tipping point species, forest spp, etc.
sps_vars_tipping_point <- sps_sel_all_vars %>%
  filter(sps_groups=="Tipping Point")
sps_vars_forest <- sps_sel_all_vars %>%
  filter(sps_groups=="Forest")
sps_vars_grassland <- sps_sel_all_vars %>%
  filter(sps_groups=="Grasslands")
sps_vars_aridland <- sps_sel_all_vars %>%
  filter(sps_groups=="Aridlands")
sps_vars_wetland <- sps_sel_all_vars %>%
  filter(sps_groups=="Water/wetland")
sps_vars_generalist <- sps_sel_all_vars %>%
  filter(sps_groups=="Habitat Generalist")

# # pct pop of tipping point spp within carbon 90% areas

tipping_pt_spp <- unique(sps_vars_tipping_point$species_code) #unique tipping pt spp codes
raster_names <- names(pct_pop_carbon_mask) #masked to 90% carbon areas
sel_species <- raster_names[raster_names %in% tipping_pt_spp] #select tipping pt spp
spp_raster <- subset(pct_pop_carbon_mask, sel_species) #subset tipping pt spp rasters only
pct_pop_tipping_pt_spp_carbon <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for tipping pt spp
pct_pop_tipping_pt_spp_carbon <- tibble::rownames_to_column(pct_pop_tipping_pt_spp_carbon, "species_code")

write_csv(pct_pop_tipping_pt_spp_carbon, "outputs/pct_pop_tipping_pt_spp_carbon_25Oct2023.csv")

# skip the above step and load original tipping pt csv
# pct_pop_tipping_pt_spp_carbon_90pct <- read_csv("outputs/pct_pop_tipping_pt_spp_carbon_90pct.csv")

# pct pop of forest spp within carbon 90% areas #NOTE overwrites objects

forest_spp <- unique(sps_vars_forest$species_code) #unique forest spp codes
raster_names <- names(pct_pop_carbon_mask) #masked to 90% carbon areas
sel_species <- raster_names[raster_names %in% forest_spp] #select forest spp
spp_raster <- subset(pct_pop_carbon_mask, sel_species) #subset forest spp rasters only
pct_pop_forest_spp_carbon <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for forest spp
pct_pop_forest_spp_carbon <- tibble::rownames_to_column(pct_pop_forest_spp_carbon, "species_code")

write_csv(pct_pop_forest_spp_carbon, "outputs/pct_pop_forest_spp_carbon_25Oct2023.csv")

# pct pop of grassland spp within carbon 90% areas #NOTE overwrites objects

grassland_spp <- unique(sps_vars_grassland$species_code)
raster_names <- names(pct_pop_carbon_mask)
sel_species <- raster_names[raster_names %in% grassland_spp]
spp_raster <- subset(pct_pop_carbon_mask, sel_species)
pct_pop_grassland_spp_carbon <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_grassland_spp_carbon <- tibble::rownames_to_column(pct_pop_grassland_spp_carbon, "species_code")

write_csv(pct_pop_grassland_spp_carbon, "outputs/pct_pop_grassland_spp_carbon_25Oct2023.csv")

# pct pop of aridland spp within carbon 90% areas #NOTE overwrites objects

aridland_spp <- unique(sps_vars_aridland$species_code)
raster_names <- names(pct_pop_carbon_mask)
sel_species <- raster_names[raster_names %in% aridland_spp]
spp_raster <- subset(pct_pop_carbon_mask, sel_species)
pct_pop_aridland_spp_carbon <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_aridland_spp_carbon <- tibble::rownames_to_column(pct_pop_aridland_spp_carbon, "species_code")

write_csv(pct_pop_aridland_spp_carbon, "outputs/pct_pop_aridland_spp_carbon_25Oct2023.csv")

# pct pop of wetland spp within carbon 90% areas #NOTE overwrites objects

wetland_spp <- unique(sps_vars_wetland$species_code)
raster_names <- names(pct_pop_carbon_mask)
sel_species <- raster_names[raster_names %in% wetland_spp]
spp_raster <- subset(pct_pop_carbon_mask, sel_species)
pct_pop_wetland_spp_carbon <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_wetland_spp_carbon <- tibble::rownames_to_column(pct_pop_wetland_spp_carbon, "species_code")

write_csv(pct_pop_wetland_spp_carbon, "outputs/pct_pop_wetland_spp_carbon_25Oct2023.csv")

# pct pop of generalist spp within carbon 90% areas #NOTE overwrites objects

generalist_spp <- unique(sps_vars_generalist$species_code)
raster_names <- names(pct_pop_carbon_mask)
sel_species <- raster_names[raster_names %in% generalist_spp]
spp_raster <- subset(pct_pop_carbon_mask, sel_species)
pct_pop_generalist_spp_carbon <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_generalist_spp_carbon <- tibble::rownames_to_column(pct_pop_generalist_spp_carbon, "species_code")

write_csv(pct_pop_generalist_spp_carbon, "outputs/pct_pop_generalist_spp_carbon_25Oct2023.csv")
beep()



# calculate percent of spp that are >44 >50 >75 pct represented -----------------
library(tidyverse)

# load data if necessary
pct_pop_tipping_pt_spp_carbon_90pct <- read_csv("outputs/pct_pop_tipping_pt_spp_carbon_90pct.csv")
pct_pop_forest_spp_carbon_90pct <- read_csv("outputs/pct_pop_forest_spp_carbon_90pct.csv")
pct_pop_grassland_spp_carbon_90pct <- read_csv("outputs/pct_pop_grassland_spp_carbon_90pct.csv")
pct_pop_aridland_spp_carbon_90pct <- read_csv("outputs/pct_pop_aridland_spp_carbon_90pct.csv")
pct_pop_wetland_spp_carbon_90pct <- read_csv("outputs/pct_pop_wetland_spp_carbon_90pct.csv")
pct_pop_generalist_spp_carbon_90pct <- read_csv("outputs/pct_pop_generalist_spp_carbon_90pct.csv")

# tipping point spp
pct_pop_tipping_pt_spp_carbon_90pct <- pct_pop_tipping_pt_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_tipping_pt_spp_carbon_90pct <- pct_pop_tipping_pt_spp_carbon_90pct %>%
  summarize(more75 = mean(more75), more50 = mean(more50), more44 = mean(more44)) %>%
  mutate(guild="tipping_point")

# forest spp
pct_pop_forest_spp_carbon_90pct <- pct_pop_forest_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_forest_spp_carbon_90pct <- pct_pop_forest_spp_carbon_90pct %>%
  summarize(more75 = mean(more75), more50 = mean(more50), more44 = mean(more44)) %>%
  mutate(guild="forest")

# grassland spp
pct_pop_grassland_spp_carbon_90pct <- pct_pop_grassland_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_grassland_spp_carbon_90pct <- pct_pop_grassland_spp_carbon_90pct %>%
  summarize(more75 = mean(more75), more50 = mean(more50), more44 = mean(more44)) %>%
  mutate(guild="grassland")

# aridland spp
pct_pop_aridland_spp_carbon_90pct <- pct_pop_aridland_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_aridland_spp_carbon_90pct <- pct_pop_aridland_spp_carbon_90pct %>%
  summarize(more75 = mean(more75), more50 = mean(more50), more44 = mean(more44)) %>%
  mutate(guild="aridland")

# wetland spp
pct_pop_wetland_spp_carbon_90pct <- pct_pop_wetland_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_wetland_spp_carbon_90pct <- pct_pop_wetland_spp_carbon_90pct %>%
  summarize(more44 = mean(more44, na.rm=T), more50 = mean(more50, na.rm=T), more75 = mean(more75, na.rm=T)) %>%
  mutate(guild="wetland")

# generalist spp
pct_pop_generalist_spp_carbon_90pct <- pct_pop_generalist_spp_carbon_90pct %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0)) %>%
  mutate(more44 = ifelse(sum>0.44,1,0))

summary_generalist_spp_carbon_90pct <- pct_pop_generalist_spp_carbon_90pct %>%
  summarize(more75 = mean(more75), more50 = mean(more50), more44 = mean(more44)) %>%
  mutate(guild="generalist")

# combine rows into single table
summary_pct_pop_guild_carbon_90pct <- rbind(summary_tipping_pt_spp_carbon_90pct, summary_forest_spp_carbon_90pct, summary_grassland_spp_carbon_90pct, summary_aridland_spp_carbon_90pct, summary_wetland_spp_carbon_90pct, summary_generalist_spp_carbon_90pct)

write_csv(summary_pct_pop_guild_carbon_90pct, "outputs/summary_pct_pop_guild_carbon_21Sep2023.csv")