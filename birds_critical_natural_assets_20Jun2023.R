# Bird populations represented within Critical Natural Assets (CNA) areas within USA
# June 13 2023
# Rachel Neugarten

# Notes from Courtney:
# The "final_species_selection.rds" table has a bunch of columns for the 479 species of the analysis. It has all the columns of ebirdst_runs plus our guild groups. 
# Our guilds groups are in a column named "sps_groups" (the last column of the table).
# The tiff files in the zip are for all the species with abundnace > 0 in our study area (more than our 479 species). 
# You can filter them by their file name using the final_species_slection table and the "species_code" column.

library(data.table)
library(stringr)
library(terra)
library(dplyr)
library(tictoc)
library(beepr)

# load the percent population per species raster --------------------

sps_sel_all_vars <- readRDS("data/final_species_selection.rds") #update filepath
pct_pop_files <- list.files("data/pct_pop_sps",  #update filepath
                            pattern = ".tif", full.names = T)

tiffs_sps <- str_extract(pct_pop_files, "(?<=pct_pop_sps\\/)(.*)(?=_a)") #update filepath
tifs_path_sel <- pct_pop_files[tiffs_sps %in% sps_sel_all_vars$species_code]
sps_sel <- tiffs_sps[tiffs_sps %in% sps_sel_all_vars$species_code]

pct_pop_per_sp_rast <- rast(tifs_path_sel)
names(pct_pop_per_sp_rast) <- sps_sel

# check projection, resolution
crs(pct_pop_per_sp_rast, describe=F, proj=T) #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
res(pct_pop_per_sp_rast) #2962.807 (3 km)

# plot one layer
plot(pct_pop_per_sp_rast, "acafly") #Acadian flycatcher?

# load the CNA layer (local NCP, 90% target, prioritized within USA) ----------------
cna <- rast("data/critical_natural_assets/solution_scenario-A_usa_target-90.tif")

# check projection
crs(cna, describe=F, proj=T) # "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
res(cna) #2 km

# look at it
plot(cna) #note it includes offshore territories so might need to mask the extent to match the birds data

#re-project and resample to get CNA layer to match birds data ------------------
cna_3km <- project(cna, pct_pop_per_sp_rast, method="near")
cna_3km <- resample(cna_3km, pct_pop_per_sp_rast, method="near")
crs(cna_3km, describe=F, proj=T) #good
res(cna_3km) #2962.809 good

plot(cna_3km) #looks distorted but matches bird data

# mask bird data with CNA data (takes a few minutes) -------------------
pct_pop_mask <- mask(pct_pop_per_sp_rast, cna_3km, maskvalues=c(0,NA)) #trying to mask in values=1 only
beep()

plot(pct_pop_mask, "acafly")

#save masked version (takes 20 min, only do this once)
#tic()
#writeRaster(pct_pop_mask, "outputs/rasters/pct_pop_mask_cna.tif", overwrite=F)
#toc()
#beep()

# group bird tifs by guild, unmasked version ----------------

# # Guillermo: A code example of how I did the grouping is below. Result is a single "multi layers" raster where each layer is a species, and each layer is named with its species code.

sps_groups <- unique(sps_sel_all_vars$sps_groups)

pct_pop_per_group_list_all <- lapply(sps_groups,
                                     function(group_name){
                                       group_sps <- subset(
                                         sps_sel_all_vars, sps_groups == group_name)$species_code |> 
                                         sort()
                                       raster_names <- names(pct_pop_per_sp_rast) #replaced this raster
                                       sel_species <- raster_names[raster_names %in% group_sps]
                                       group_raster <- subset(pct_pop_per_sp_rast, sel_species)
                                       pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
                                       return(pct_sps_per_group_cell)
                                     })
names(pct_pop_per_group_list_all) <- sps_groups

#rasterize
pct_pop_per_group_all <- rast(pct_pop_per_group_list_all)
#pct_pop_tipping_point <- rast(pct_pop_per_group_list_all$`Tipping Point`) #raster for just tipping pt spp

#save resulting raster
writeRaster(pct_pop_per_group_all, "outputs/rasters/pct_pop_per_guild_all.tif", overwrite=TRUE)

# look at outputs
plot(pct_pop_per_group_all, "Water/wetland", main="Sum of Water/wetland bird populations % per pixel")
plot(pct_pop_per_group_all, "Forest", main="Sum of Forest bird populations % per pixel")
plot(pct_pop_per_group_all, "Aridlands", main="Sum of Aridlands bird populations % per pixel")
plot(pct_pop_per_group_all, "Grasslands", main="Sum of Grasslands bird populations % per pixel")
plot(pct_pop_per_group_all, "Habitat Generalist", main="Sum of Habitat Generalist bird populations % per pixel")
plot(pct_pop_per_group_all, "Tipping Point", main="Percent of Tipping Point bird populations per pixel")

#calculate the pct of each guild population  -----------
pct_pop_per_group_all_sum <- global(pct_pop_per_group_all, fun='sum', na.rm=T)
pct_pop_per_group_all_sum <- tibble::rownames_to_column(pct_pop_per_group_all_sum, "Guild")


# group bird tifs, masked to CNA, by guild -----------------------

sps_groups <- unique(sps_sel_all_vars$sps_groups)

tic()
pct_pop_per_group_list_cna <- lapply(sps_groups,
                                  function(group_name){
                                    group_sps <- subset(
                                      sps_sel_all_vars, sps_groups == group_name)$species_code |> 
                                      sort()
                                    raster_names <- names(pct_pop_mask) #replaced this raster
                                    sel_species <- raster_names[raster_names %in% group_sps]
                                    group_raster <- subset(pct_pop_mask, sel_species)
                                    pct_sps_per_group_cell <- app(group_raster, "sum", na.rm = T)
                                    return(pct_sps_per_group_cell)
                                  })
names(pct_pop_per_group_list_cna) <- sps_groups
toc()
beep()

# rasterize
pct_pop_per_group_cna <- rast(pct_pop_per_group_list_cna)

# save resulting raster
writeRaster(pct_pop_per_group_cna, "outputs/rasters/pct_pop_per_guild_cna.tif", overwrite=TRUE)

# look at outputs
plot(pct_pop_per_group_cna, "Water/wetland", main="Sum of Water/wetland bird populations % within CNA")
plot(pct_pop_per_group_cna, "Forest", main="Sum of Forest bird populations % within CNA")
plot(pct_pop_per_group_cna, "Aridlands", main="Sum of Aridlands bird populations % within CNA")
plot(pct_pop_per_group_cna, "Grasslands", main="Sum of Grasslands bird populations % within CNA")
plot(pct_pop_per_group_cna, "Habitat Generalist", main="Sum of Habitat Generalist bird populations % within CNA")
plot(pct_pop_per_group_cna, "Tipping Point", main="Sum of Tipping Point bird populations % within CNA")

#calculate the pct of each guild population contained with CNA areas -----------

# load data if necessary
pct_pop_per_group_cna <- rast("outputs/rasters/pct_pop_per_guild_cna.tif")

pct_pop_per_group_cna_sum <- global(pct_pop_per_group_cna, fun='sum', na.rm=T)
pct_pop_per_group_cna_sum <- tibble::rownames_to_column(pct_pop_per_group_cna_sum, "Guild")
pct_pop_per_group_cna_sum <- pct_pop_per_group_cna_sum %>%
  rename("sum_cna" = "sum") #rename "sum" to "sum_cna" for clarity

# calculate the sum of pct_pop within CNA divided by the sum of pct_pop total
pct_pop_per_group_cna_sum$sum_all <- pct_pop_per_group_all_sum$sum #total pct_pop by group
pct_pop_per_group_cna_sum$pct_cna <- pct_pop_per_group_cna_sum$sum_cna / pct_pop_per_group_all_sum$sum


#plot individual tipping point spp (examples)
plot(pct_pop_per_sp_rast$snoowl1, main="Snowy owl % population per pixel")
plot(pct_pop_per_sp_rast$cerwar, main="Cerulean warbler % population per pixel")

#load spp data masked to CNA
pct_pop_mask <- rast("outputs/rasters/pct_pop_mask_cna.tif")

#plot tipping point spp masked to CNA
plot(pct_pop_mask$snoowl1, main="Snowy owl % population within CNA")
plot(pct_pop_mask$cerwar, main="Cerulean warbler % population wihtin CNA")

#sum tipping point spp masked to CNA
snowy_owl_sum <- global(pct_pop_per_sp_rast$snoowl1, fun="sum", na.rm=TRUE) #1
snowy_owl_sum_cna <- global(pct_pop_mask$snoowl1, fun="sum", na.rm=TRUE)
snowy_owl_sum_cna #0.1492723 so 15%
cerulean_warbler_sum_cna <- global(pct_pop_mask$cerwar, fun="sum", na.rm=TRUE)
cerulean_warbler_sum_cna #0.9057386

# calculate pct of population within CNA for individual spp ------------------
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

# pct pop of tipping point spp within CNA
tipping_pt_spp <- unique(sps_vars_tipping_point$species_code) #unique tipping pt spp codes
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% tipping_pt_spp] #select tipping pt spp
spp_raster <- subset(pct_pop_mask, sel_species) #subset tipping pt spp rasters only
pct_pop_tipping_pt_spp_cna <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for tipping pt spp
pct_pop_tipping_pt_spp_cna <- tibble::rownames_to_column(pct_pop_tipping_pt_spp_cna, "species_code")

write_csv(pct_pop_tipping_pt_spp_cna, "outputs/pct_pop_tipping_pt_spp_cna.csv")

# calculate pct of population within CNA for all forest spp ------------------
#NOTE overwrites objects with same names

forest_spp <- unique(sps_vars_forest$species_code) #unique forest spp codes
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% forest_spp] #select forest spp
spp_raster <- subset(pct_pop_mask, sel_species) #subset forest spp rasters only
pct_pop_forest_spp_cna <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for forest spp
pct_pop_forest_spp_cna <- tibble::rownames_to_column(pct_pop_forest_spp_cna, "species_code")

write_csv(pct_pop_forest_spp_cna, "outputs/pct_pop_forest_spp_cna.csv")

# calculate pct of population within CNA for all Grassland spp ------------------
#NOTE overwrites objects with same names

grassland_spp <- unique(sps_vars_grassland$species_code)
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% grassland_spp] #select grassland spp
spp_raster <- subset(pct_pop_mask, sel_species)
pct_pop_grassland_spp_cna <- global(spp_raster, fun='sum', na.rm=T) #calculate sum for grassland spp
pct_pop_grassland_spp_cna <- tibble::rownames_to_column(pct_pop_grassland_spp_cna, "species_code")

write_csv(pct_pop_grassland_spp_cna, "outputs/pct_pop_grassland_spp_cna.csv")

# calculate pct of population within CNA for all Aridland spp ------------------
#NOTE overwrites objects with same names

aridland_spp <- unique(sps_vars_aridland$species_code)
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% aridland_spp]
spp_raster <- subset(pct_pop_mask, sel_species)
pct_pop_aridland_spp_cna <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_aridland_spp_cna <- tibble::rownames_to_column(pct_pop_aridland_spp_cna, "species_code")

write_csv(pct_pop_aridland_spp_cna, "outputs/pct_pop_aridland_spp_cna.csv")

# calculate pct of population within CNA for all Wetland spp ------------------
#NOTE overwrites objects with same names

wetland_spp <- unique(sps_vars_wetland$species_code)
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% wetland_spp]
spp_raster <- subset(pct_pop_mask, sel_species)
pct_pop_wetland_spp_cna <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_wetland_spp_cna <- tibble::rownames_to_column(pct_pop_wetland_spp_cna, "species_code")

write_csv(pct_pop_wetland_spp_cna, "outputs/pct_pop_wetland_spp_cna.csv")

# calculate pct of population within CNA for all Habitat Generalist spp ------------------
#NOTE overwrites objects with same names

generalist_spp <- unique(sps_vars_generalist$species_code)
raster_names <- names(pct_pop_mask) #masked to CNA
sel_species <- raster_names[raster_names %in% generalist_spp]
spp_raster <- subset(pct_pop_mask, sel_species)
pct_pop_generalist_spp_cna <- global(spp_raster, fun='sum', na.rm=T)
pct_pop_generalist_spp_cna <- tibble::rownames_to_column(pct_pop_generalist_spp_cna, "species_code")

write_csv(pct_pop_generalist_spp_cna, "outputs/pct_pop_generalist_spp_cna.csv")


# calculate percent of spp that are >90 >75 >50 pct represented ---------------
library(tidyverse)

# load data if necessary
pct_pop_tipping_pt_spp_cna <- read_csv("outputs/pct_pop_tipping_pt_spp_cna.csv")
pct_pop_forest_spp_cna <- read_csv("outputs/pct_pop_forest_spp_cna.csv")
pct_pop_grassland_spp_cna <- read_csv("outputs/pct_pop_grassland_spp_cna.csv")
pct_pop_aridland_spp_cna <- read_csv("outputs/pct_pop_aridland_spp_cna.csv")
pct_pop_wetland_spp_cna <- read_csv("outputs/pct_pop_wetland_spp_cna.csv")
pct_pop_generalist_spp_cna <- read_csv("outputs/pct_pop_generalist_spp_cna.csv")

# tipping point spp
pct_pop_tipping_pt_spp_cna <- pct_pop_tipping_pt_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_tipping_pt_spp_cna <- pct_pop_tipping_pt_spp_cna %>%
  summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
  mutate(guild="tipping_point") %>%
  mutate(n=nrow(pct_pop_tipping_pt_spp_cna))

# forest spp
pct_pop_forest_spp_cna <- pct_pop_forest_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_forest_spp_cna <- pct_pop_forest_spp_cna %>%
  summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
  mutate(guild="forest") %>%
  mutate(n=nrow(pct_pop_forest_spp_cna))

# grassland spp
pct_pop_grassland_spp_cna <- pct_pop_grassland_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_grassland_spp_cna <- pct_pop_grassland_spp_cna %>%
  summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
  mutate(guild="grassland") %>%
  mutate(n=nrow(pct_pop_grassland_spp_cna))

# aridland spp
pct_pop_aridland_spp_cna <- pct_pop_aridland_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_aridland_spp_cna <- pct_pop_aridland_spp_cna %>%
  summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
  mutate(guild="aridland")%>%
  mutate(n=nrow(pct_pop_aridland_spp_cna))

# wetland spp
pct_pop_wetland_spp_cna <- pct_pop_wetland_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_wetland_spp_cna <- pct_pop_wetland_spp_cna %>%
  summarize(more90 = mean(more90, na.rm=T), more75 = mean(more75, na.rm=T), more50 = mean(more50, na.rm=T)) %>%
  mutate(guild="wetland")%>%
  mutate(n=nrow(pct_pop_wetland_spp_cna))

# generalist spp
pct_pop_generalist_spp_cna <- pct_pop_generalist_spp_cna %>%
  mutate(more90 = ifelse(sum>0.9,1,0)) %>%
  mutate(more75 = ifelse(sum>0.75,1,0)) %>%
  mutate(more50 = ifelse(sum>0.5,1,0))

summary_generalist_spp_cna <- pct_pop_generalist_spp_cna %>%
  summarize(more90 = mean(more90), more75 = mean(more75), more50 = mean(more50)) %>%
  mutate(guild="generalist") %>%
  mutate(n=nrow(pct_pop_generalist_spp_cna))

# combine rows into single table
summary_pct_pop_guild_cna <- rbind(summary_tipping_pt_spp_cna, summary_forest_spp_cna, summary_grassland_spp_cna, summary_aridland_spp_cna, summary_wetland_spp_cna, summary_generalist_spp_cna)

write_csv(summary_pct_pop_guild_cna, "outputs/summary_pct_pop_guild_cna.csv")

# pivot to make tidy and plot CNA results
library(ggplot2)

summary_longer_cna <- pivot_longer(summary_pct_pop_guild_cna, cols=1:3, names_to="category", values_to="pct_spp")

plot_cna <- ggplot(summary_longer_cna, aes(x=guild, y=pct_spp, fill=category)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Percent of spp represented within Critical Natural Assets") +
  xlab("Guild") +
  ylab("Percent of spp")
plot_cna

