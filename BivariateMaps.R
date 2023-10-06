library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(reshape2)
library(viridis)
library(dplyr)
library(tidyverse)
library(tigris)

# Load raster files
cna_usa <- rast("cna_usa_sum_crop.tif")
vuln_carbon_usa <- rast("vuln_carbon_usa_90pct.tif")
pct_pop_group <- rast("pct_pop_per_group_all.tif")
pct_pop_species <- rast("pct_pop_per_sp_rast.tif")


dataset1 <- pct_pop_group$Forest
dataset2 <- pct_pop_group$Aridlands
quant_length <- 4

 # create 4 quantile buckets for each dataset
  quantiles_dataset1 <- global(dataset1, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset2 <- global(dataset2, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)

  # create color scale that encodes two variables
  bivariate_color_scale <- tibble(
    "3_3" = "#2a5a5b",
    "2_3" = "#567994",
    "1_3" = "#6c83b5",
    "3_2" = "#5a9178",
    "2_2" = "#90b2b3",
    "1_2" = "#b5c0da",
    "3_1" = "#73ae80",
    "2_1" = "#b8d6be",
    "1_1" = "#e8e8e8"
  ) %>%
    gather("group", "fill")
  
  # cut into groups defined above and join fill
  rc1 <- classify(dataset1, matrix(c(quantiles_dataset1[[1]], quantiles_dataset1[[2]], 1,
         quantiles_dataset1[[2]], quantiles_dataset1[[3]], 2,
         quantiles_dataset1[[3]], quantiles_dataset1[[4]], 3), ncol = 3, byrow = TRUE),
         include.lowest=TRUE) %>% project("EPSG:5070")
  

  rc2 <- classify(dataset2, matrix(c(quantiles_dataset2[[1]], quantiles_dataset2[[2]], 1,
                                     quantiles_dataset2[[2]], quantiles_dataset2[[3]], 2,
                                     quantiles_dataset2[[3]], quantiles_dataset2[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  study_area_sf <- ne_download(
    scale = 50, type = "admin_1_states_provinces", returnclass = "sf") |> 
    subset(iso_a2 == "US" & name != "Hawaii") %>%
    st_transform(crs = 5070)
  
  # combine into a single data frame and join the group-level "fill" (color bins for bivariate map)
  geo <- c(rc1, rc2) %>%
    classify(cbind(NA, 1))

  # GD - as the dataset is huge and this will be used for a printed/online map 
  # I'm aggregating (changing the resolution) of the raster by a factor of 3.. 
  # you can change it if you want but in any case a similar aggregation is going 
  # to happen when the final map is rendered 
  #geo_agg_clip <- aggregate(geo, fact = 3, fun = "mean") |> 
  geo_agg_clip <- geo |>
  # cropping the raster for only including the study area
    crop(study_area_sf, mask = T)
  
  geo_df <- as.data.frame(geo_agg_clip, xy = TRUE)
  geo_df$group <- paste0(geo_df[,3],"_",geo_df[,4])

  geo_df <- geo_df %>%
    left_join(bivariate_color_scale, by = "group")

ggplot() +
  geom_sf(data = study_area_sf, col = "black", fill = NA, lwd = 0.1) +
  geom_tile(data = geo_df, mapping = aes(x = x, y = y, fill = fill)) +
  scale_fill_identity() +
  theme(panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = "grey99"),
        plot.background = element_rect(fill = "grey99"),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("BivariatePlot.png", width = 6, height = 6)



############# IF YOU WANT TO SHIFT THE DATASET TO HAVE ALASKA AT THE BOTTOM - 
############# NOTE, THIS WILL AGGREGATE THE DATA AND YOU WILL LOSE SPATIAL RESOLUTION
# Shifting the dataset with tigris::shift_geometry()
study_area_sf_shifted <- shift_geometry(study_area_sf)
# st_write(study_area_sf_shifted, "rachel_data_shifted.gpkg", layer = "study_area_shifted", append = T)
# study_area_sf_shifted <- st_read("rachel_data_shifted.gpkg", layer = "study_area_shifted")

geo_sf <- geo_df |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = T)

# This function takes a long time to run (we can aggregate the dataset if needed to make it run faster)
geo_sf_shifted <- shift_geometry(geo_sf)

ggplot(geo_sf_shifted) +
  geom_sf(aes(col = fill))

# st_write(geo_sf_shifted, "rachel_data_shifted.gpkg", layer = "geo_sf_shifted",
#          append=TRUE)
# geo_sf_shifted <- st_read("rachel_data_shifted.gpkg", layer = "geo_sf_shifted")

geo_sf_shifted$x <- st_coordinates(geo_sf_shifted)[,1]
geo_sf_shifted$y <- st_coordinates(geo_sf_shifted)[,2]
geo_df_shifted <- st_drop_geometry(geo_sf_shifted)
head(geo_df_shifted)

ggplot() +
  geom_sf(data = geo_sf_shifted, mapping = aes(col = fill), size = 0.0001) +
  scale_colour_identity() + 
  geom_sf(data = study_area_sf_shifted, col = "black", fill = NA, lwd = 0.1) +
  theme(panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = "grey99"),
        plot.background = element_rect(fill = "grey99"),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("BivariatePlot_shifted.png", width = 6, height = 6)

