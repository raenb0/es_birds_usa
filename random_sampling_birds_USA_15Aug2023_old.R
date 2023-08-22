# Create a random sample of 37% of grid cells of USA 
library(terra)
library(RColorBrewer)

# calculate how many pixels makes up 37% (and 44%) of USA land area

#load raster with bird pct population per species
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

#load raster with bird pct population per group
pct_pop_per_group_all <- rast("outputs/rasters/pct_pop_per_group_all.tif")

#count total pixels in birds rasters
count_pixels_birds <- global(pct_pop_per_group_all, fun="notNA") #bird TIFs have different counts of non-NA pixels, so taking 37% of these values won't work

#create color dataframe
color_df <- data.frame(value=1, color="blue")

# load blank USA raster
usa_raster <- rast("data/usa_raster.tif")
usa_raster
plot(usa_raster, col=color_df, main="USA")

#count pixels in USA raster
count_pixels_usa <- global(usa_raster, fun="notNA") #1073267
sample_size_37pct <- count_pixels_usa[1,1]*0.37 #397108.8 this is equivalent to high NCP areas
sample_size_44pct <- count_pixels_usa[1,1]*0.44 #472237.5 this is equivalent to high carbon areas

## create raster samples of 37% (doesn't work)
# sample1 <- spatSample(usa_raster, size=3, method="random", replace=FALSE, na.rm=TRUE, 
#                        as.raster=TRUE, as.df=FALSE, as.points=FALSE, values=TRUE, cells=FALSE, 
#                        xy=FALSE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=TRUE)
# 
# plot(sample1, col=color_df, main="Sample size of 3, method=random")
# 
# sample2 <- spatSample(usa_raster, size=397108, method="random", replace=FALSE, na.rm=TRUE, 
#                        as.raster=TRUE, as.df=FALSE, as.points=FALSE, values=TRUE, cells=FALSE, 
#                        xy=FALSE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=TRUE)
# 
# plot(sample2, col=color_df, main="Sample size of 397108, method=random")
# 
# sample3 <- spatSample(usa_raster, size=397108, method="regular", replace=FALSE, na.rm=TRUE, 
#                            as.raster=TRUE, as.df=FALSE, as.points=FALSE, values=TRUE, cells=FALSE, 
#                            xy=FALSE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=TRUE)
# 
# plot(sample3, col=color_df, main="Sample size of 397108, method=regular")
#
##try spatSample on the birds (groups) raster directly (works but not exactly 37%)
# sample1_birds <- spatSample(pct_pop_per_group_all, size=397109, method="random", replace=FALSE, na.rm=TRUE,
#                       as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
#                       xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)
# 
# sample1_birds_rast <- rast(sample1_birds, type="xyz")
# 
##look at outputs
# plot(sample1_birds_rast, "Tipping Point", main="Tipping Point birds in sample", axes=F, col=(brewer.pal(3, "Blues")))
# plot(sample1_birds_rast, "Water/wetland", main="Water/wetland birds in sample", axes=F, col=(brewer.pal(3, "Blues")))
# plot(sample1_birds_rast, "Forest", main="Forest birds in sample", axes=F, col=(brewer.pal(3, "Blues"))) 

# create a spatial sample from the blank USA raster equivalent to 37% of land area
sample1_usa <- spatSample(usa_raster, size=397109, method="random", replace=FALSE, na.rm=TRUE,
                            as.raster=FALSE, as.df=TRUE, as.points=FALSE, values=TRUE, cells=FALSE, 
                            xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=1, exhaustive=FALSE)

# #convert to raster (not necessary, we can extract raster values using the sample dataframe)
# sample1_usa_rast <- rast(sample1_usa, type="xyz")
# sample1_usa_rast #resolution looks good
# crs(sample1_usa_rast, describe=F, proj=T) #missing projection
# crs(sample1_usa_rast) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
# 
# plot(sample1_usa_rast, axes=F, col=(brewer.pal(3, "Blues")))
# 
# #multiply sample raster by birds data
# 
# #check extent and resolutions match
# ext(sample1_usa_rast)
# ext(pct_pop_per_group_all) #close but not exactly the same
# res(sample1_usa_rast)
# res(pct_pop_per_group_all) #match
# 
# #resample birds data to match sample raster data
# pct_pop_per_group_resample <- resample(pct_pop_per_group_all, sample1_usa_rast, method="near")
# ext(sample1_usa_rast)
# ext(pct_pop_per_group_resample) #matches now
# 
# sample1_bird_groups_rast <- sample1_usa_rast*pct_pop_per_group_resample
# names(sample1_bird_groups_rast) <- names(pct_pop_per_group_all) #set names of sample dataset
# 
# plot(sample1_bird_groups_rast, "Tipping Point", main="Tipping Point birds in sample", axes=F, col=(brewer.pal(3, "Blues")))

#load raster with bird pct population per species, if necessary
pct_pop_per_sp_rast <- rast("outputs/rasters/pct_pop_per_sp_rast.tif")

# convert USA sample to vector
library(sf)
library(tictoc)
library(beepr)

sample1_usa_vect <- sample1_usa |> 
  st_as_sf(coords = c("x", "y"), crs = crs(usa_raster)) |> 
  vect()

#extract values from bird guilds data for sample points
tic()
test_df <- extract(pct_pop_per_group_all, sample1_usa_vect) #2 seconds
toc()
beep()
head(test_df, 10)

#extract values from bird species data for sample points
tic()
test_df_sp <- extract(pct_pop_per_sp_rast, sample1_usa_vect) #73 seconds
toc()
beep()
head(test_df_sp, 10)
