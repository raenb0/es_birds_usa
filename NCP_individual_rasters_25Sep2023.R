# Individual NCP maps
# September 25 2023

library(terra)
library(tictoc)
library(beepr)

#load NCP layers (global)  ---------------------------
rastlist <- list.files(path = "C:/Users/raenb/Box/Documents/GIS/EcosystemServices_ChaplinKramer/reprojected_resampled_Eckert2km", 
                       pattern='.tif$', 
                       all.files= T, 
                       full.names= T)
tic()
ncp_stack <- terra::rast(rastlist) #takes a few min
toc()
beep()

#clean up list of raster names
print(rastlist)
#function to remove long pathnames from raster layer names
rastnames <-  lapply(rastlist, function(x){
    gsub("C:/Users/raenb/Box/Documents/GIS/EcosystemServices_ChaplinKramer/reprojected_resampled_Eckert2km/","",x)
  })
#function to remove ".tif" from raster layer names
rastnames2 <-  lapply(rastnames, function(x){
  gsub(".tif","",x)
})
#rename raster layers using cleaned up names
names(ncp_stack) <- rastnames2
names(ncp_stack) #looks good

# load USA vector, re-projected to match global carbon layer 
usa_vect <- terra::vect("outputs/rasters/usa_vect_eck4.shp")

#check if CRS matches
terra::crs(usa_vect, describe=F, proj=T) #eck4
terra::crs(ncp_stack, describe=F, proj=T) #eck4

#  Crop global NCP rasters to USA (use crop to get correct spatial extent) #only do this once
ncp_stack_usa = terra::crop(ncp_stack, usa_vect, mask=TRUE)

names(ncp_stack_usa) # check layer names, looks good

#plot some examples
terra::plot(ncp_stack_usa$realized_domestictimber_forest, axes=F, main = "Timber (domestic), scale 0-1)")
terra::plot(ncp_stack_usa$realized_commercialtimber_forest, axes=F, main = "Timber (commercial), scale 0-1")
terra::plot(ncp_stack_usa$realized_coastalprotection_norm_onshore, axes=F, main = "Coastal protection (onshore)") #not visible
terra::plot(ncp_stack_usa$realized_fuelwood_forest, axes=F, main = "Fuelwood (scale 0-1)")
terra::plot(ncp_stack_usa$realized_fwfish_per_km2, axes=F, main = "Riverine fish harvest per km2")
terra::plot(ncp_stack_usa$realized_grazing_natnotforest, axes=F, main = "Grazing (scale 0-1)")
terra::plot(ncp_stack_usa$realized_pollination_norm_nathab, axes=F, main = "Pollination")
terra::plot(ncp_stack_usa$realized_nitrogenretention_attn_500km, axes=F, main = "Water quality (nitrogen retention)")
terra::plot(ncp_stack_usa$realized_sedimentdeposition_attn_500km, axes=F, main = "Water quality (sediment retention)")
terra::plot(ncp_stack_usa$realized_floodmitigation_attn_500km_nathab, axes=F, main = "Flood mitigation")

#global NCP (not included in national scale prioritization)
terra::plot(ncp_stack_usa$realized_moisturerecycling_nathab30s, axes=F, main = "Atmospheric moisture recycling")
terra::plot(ncp_stack_usa$Vulnerable_C_Total_2018, axes=F, main = "Vulnerable carbon storage (tonnes/ha)")


# save USA NCP raster stack
terra::writeRaster(ncp_stack_usa, "outputs/rasters/ncp_stack_usa.tif", overwrite=TRUE)

#write individual NCP layers, cropped to USA, as TIF files #only do this once
library(terra)
fnames <- paste0(rastnames2, ".tif")
setwd("outputs/rasters/ncp_usa/")
terra::writeRaster(ncp_stack_USA, fnames, overwrite=FALSE) #takes a few min


