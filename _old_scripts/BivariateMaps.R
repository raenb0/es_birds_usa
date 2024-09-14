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
cna_usa <- rast("outputs/rasters/cna_usa_sum_crop.tif")
#vuln_carbon_usa <- rast("outputs/rasters/vuln_carbon_usa_90pct.tif") #this is top90pct
vuln_carbon_usa <- rast("outputs/rasters/vuln_carbon_usa.tif") #this is all vulnerable carbon
pct_pop_group <- rast("outputs/rasters/pct_pop_per_group_all.tif")
pct_pop_species <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")


dataset1 <- pct_pop_group$Forest
dataset2 <- pct_pop_group$Aridlands
dataset3 <- pct_pop_group$Grasslands
dataset4 <- pct_pop_group$`Water/wetland`
dataset5 <- pct_pop_group$`Habitat Generalist`
dataset6 <- pct_pop_group$`Tipping Point`

quant_length <- 4

 # create 4 quantile buckets for each dataset
  quantiles_dataset1 <- global(dataset1, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset2 <- global(dataset2, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset3 <- global(dataset3, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset4 <- global(dataset4, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset5 <- global(dataset5, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_dataset6 <- global(dataset6, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  
  #repeat for carbon, CNA
  quantiles_carbon <- global(vuln_carbon_usa, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)
  quantiles_cna <- global(cna_usa, fun = quantile, probs = seq(0, 1, length.out = quant_length), na.rm = T)

  # create color scale that encodes two variables
  # https://kwstat.github.io/pals/reference/bivariate.html
  
  # bivariate_color_scale <- tibble(
  #   "3_3" = "#2a5a5b",
  #   "2_3" = "#567994",
  #   "1_3" = "#6c83b5",
  #   "3_2" = "#5a9178",
  #   "2_2" = "#90b2b3",
  #   "1_2" = "#b5c0da",
  #   "3_1" = "#73ae80",
  #   "2_1" = "#b8d6be",
  #   "1_1" = "#e8e8e8"
  # ) %>%
  #   gather("group", "fill")
  
  bivariate_color_scale <- tibble(
    "3_3" = "#3b4994",
    "2_3" = "#8c62aa",
    "1_3" = "#be64ac",
    "3_2" = "#5698b9",
    "2_2" = "#a5add3",
    "1_2" = "#dfb0d6",
    "3_1" = "#5ac8c8",
    "2_1" = "#ace4e4",
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
  
  rc3 <- classify(dataset3, matrix(c(quantiles_dataset3[[1]], quantiles_dataset3[[2]], 1,
                                     quantiles_dataset3[[2]], quantiles_dataset3[[3]], 2,
                                     quantiles_dataset3[[3]], quantiles_dataset3[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  rc4 <- classify(dataset4, matrix(c(quantiles_dataset4[[1]], quantiles_dataset4[[2]], 1,
                                     quantiles_dataset4[[2]], quantiles_dataset4[[3]], 2,
                                     quantiles_dataset4[[3]], quantiles_dataset4[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  rc5 <- classify(dataset5, matrix(c(quantiles_dataset5[[1]], quantiles_dataset5[[2]], 1,
                                     quantiles_dataset5[[2]], quantiles_dataset5[[3]], 2,
                                     quantiles_dataset5[[3]], quantiles_dataset5[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  rc6 <- classify(dataset6, matrix(c(quantiles_dataset6[[1]], quantiles_dataset6[[2]], 1,
                                     quantiles_dataset6[[2]], quantiles_dataset6[[3]], 2,
                                     quantiles_dataset6[[3]], quantiles_dataset6[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  rc_carbon <- classify(vuln_carbon_usa, matrix(c(quantiles_carbon[[1]], quantiles_carbon[[2]], 1,
                                                  quantiles_carbon[[2]], quantiles_carbon[[3]], 2,
                                                  quantiles_carbon[[3]], quantiles_carbon[[4]], 3), ncol = 3, byrow = TRUE),
                  include.lowest=TRUE) %>% project("EPSG:5070")
  
  rc_cna <- classify(cna_usa, matrix(c(quantiles_cna[[1]], quantiles_cna[[2]], 1,
                                                  quantiles_cna[[2]], quantiles_cna[[3]], 2,
                                                  quantiles_cna[[3]], quantiles_cna[[4]], 3), ncol = 3, byrow = TRUE),
                        include.lowest=TRUE) %>% project("EPSG:5070")
 
  
  # Rachel's interlude - saving rasters, harmonizing rasters
  
  terra::plot(rc1) #look at output, why is there a weird blank area at the top?
  terra::plot(rc_carbon)
  #save reclassified rasters
  writeRaster(rc1, "outputs/rasters/forest_birds_reclass.tif") #save forest birds reclassified
  writeRaster(rc2, "outputs/rasters/aridland_birds_reclass.tif")
  writeRaster(rc3, "outputs/rasters/grassland_birds_reclass.tif")
  writeRaster(rc4, "outputs/rasters/water_birds_reclass.tif")
  writeRaster(rc5, "outputs/rasters/generalist_birds_reclass.tif")
  writeRaster(rc6, "outputs/rasters/tipping_point_birds_reclass.tif")
  writeRaster(rc_carbon, "outputs/rasters/vuln_carbon_reclass.tif")
  writeRaster(rc_cna, "outputs/rasters/cna_reclass.tif")
  
 
  #check extent, projection, resolution
  ext(rc1)
  ext(rc_carbon)
  ext(rc_cna)
  res(rc1)
  res(rc_carbon)
  crs(rc1, describe=F, proj=T)
  crs(rc_carbon, describe=F, proj=T) #matches
  
  #resample carbon and cna data to match bird data
  rc_carbon_resample <- terra::resample(rc_carbon, rc1, method="near")
  rc_cna_resample <- terra::resample(rc_cna, rc1, method="near")
    
# back to Courtney's code
  
  study_area_sf <- ne_download(
    scale = 50, type = "admin_1_states_provinces", returnclass = "sf") |> 
    subset(iso_a2 == "US" & name != "Hawaii") %>%
    st_transform(crs = 5070)
  
  # combine into a single data frame and join the group-level "fill" (color bins for bivariate map)
  geo <- c(rc1, rc_carbon_resample) %>% #replaced with carbon
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

# create legend manually

library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
bvColors=c("#be64ac","#8c62aa","#3b4994","#dfb0d6","#a5add3","#5698b9","#e8e8e8","#ace4e4","#5ac8c8")
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3)) %>%
  rename("CNA" = "Var1",
         "Abundance" = "Var2")
test<-ggplot(legendGoal, aes(Abundance,CNA,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=bvColors)
test<-test + theme(legend.position="none")
#test<-ggdraw(test) + draw_text(text = "Abundance -->",x=0.91,y=0.58)
#test<-ggdraw(test) + draw_text(text = "Critical natural assets -->",x=0.84,y=0.5,angle=270)
test

#create a plot that will be the legend itself
lg<- test #would not be true when making a map
lg<-lg + theme(axis.title.x=element_text(size=rel(1),color=bvColors[3])) + xlab("Abundance -->")
lg<-lg + theme(axis.title.y=element_text(size=rel(1),color=bvColors[3])) + ylab("CNA -->")
lg<-lg+theme(axis.text=element_blank())
lg<-lg+theme(line=element_blank())
#put both plots on a grid
ggdraw()+ draw_plot(lg,0.1,0.7,width=0.2,height=0.2) +draw_plot(test,0.3,0,width=.7,height=.7)

# yellow-blue scale (test)
library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
bvColors=c("#E8E8E8","#00ABDD","#009195","#E4D9AC","#a5add3","#5698b9","#c8b35a","#009195","#00784E")
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3)) %>%
  rename("CNA" = "Var1",
         "Abundance" = "Var2")
test<-ggplot(legendGoal, aes(Abundance,CNA,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=bvColors)
test<-test + theme(legend.position="none")
#test<-ggdraw(test) + draw_text(text = "Abundance -->",x=0.91,y=0.58)
#test<-ggdraw(test) + draw_text(text = "Critical natural assets -->",x=0.84,y=0.5,angle=270)
test

#create a plot that will be the legend itself
lg<- test #would not be true when making a map
lg<-lg + theme(axis.title.x=element_text(size=rel(1),color=bvColors[3])) + xlab("Abundance -->")
lg<-lg + theme(axis.title.y=element_text(size=rel(1),color=bvColors[3])) + ylab("CNA -->")
lg<-lg+theme(axis.text=element_blank())
lg<-lg+theme(line=element_blank())
#put both plots on a grid
ggdraw()+ draw_plot(lg,0.1,0.7,width=0.2,height=0.2) +draw_plot(test,0.3,0,width=.7,height=.7)

# yellow-blue scale (2 by 3)
library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
bvColors2=c("#E8E8E8","#00ABDD","#E4D9AC","#009195","#c8b35a","#00784E")
melt(matrix(1:6,nrow=2))
legendGoal=melt(matrix(1:6,nrow=2)) %>%
  rename("CNA" = "Var1",
         "Abundance" = "Var2")
test<-ggplot(legendGoal, aes(Abundance,CNA,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=bvColors2)
test<-test + theme(legend.position="none")
#test<-ggdraw(test) + draw_text(text = "Abundance -->",x=0.91,y=0.58)
#test<-ggdraw(test) + draw_text(text = "Critical natural assets -->",x=0.84,y=0.5,angle=270)
test

#create a plot that will be the legend itself
lg<- test #would not be true when making a map
lg<-lg + theme(axis.title.x=element_text(size=rel(1),color="black")) + xlab("Abundance -->")
lg<-lg + theme(axis.title.y=element_text(size=rel(1),color="black")) + ylab("Carbon -->")
lg<-lg+theme(axis.text=element_blank())
lg<-lg+theme(line=element_blank())
#put both plots on a grid
ggdraw()+ draw_plot(lg,0.1,0.7,width=0.2,height=0.2) +draw_plot(test,0.3,0,width=.7,height=.7)
