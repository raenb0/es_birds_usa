# USA PAD database
# Calculate percent of CNA areas that are public, private, federal/state, protected/unprotected, etc.
# June 22 2023

library(sf)
library(rgdal)
library(terra)
library(tidyverse)
library(tictoc)
library(beepr)

#set wd
setwd("C:/Users/raenb/Box/Documents/GIS/CriticalNaturalCapital/USA_protected_areas")

#load PAD database (sf)
pad_usa <- st_read("PADUS3_0Geopackage/PADUS3_0Geopackage.gpkg") #takes 1 min

#load attribute table
pad_usa_df <- read_csv("PADUS3_0Geopackage_attribute_table.csv")

#reset wd
setwd("C:/Users/raenb/Documents/GitHub/es_birds_usa") #reset wd

str(pad_usa_df)
unique(pad_usa_df$Mang_Type) #"FED"  "TRIB" "STAT" "NGO"  "TERR" "LOC"  "UNK"  "DIST" "PVT"  "JNT" 
unique(pad_usa_df$d_Mang_Typ) #easier to interpret

# dissolve by management type
pad_usa_mang_typ <- dplyr::group_by(pad_usa, d_Mang_Typ) #group data (returns a df)

# summarize the data
tic()
pad_usa_mang_typ_dslv <- dplyr::summarise(pad_usa_mang_typ)
toc()
beep()
