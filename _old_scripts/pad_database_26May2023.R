# protected area database, USA
# May 26 2023

library(tidyverse)

setwd("C:/Users/raenb/Box/Documents/GIS/CriticalNaturalCapital/USA_protected_areas")

pad_attributes <- read_csv("PADUS3_0Geopackage_attribute_table.csv")
str(pad_attributes)
unique(pad_attributes$d_Mang_Nam)

#summarize mean, min, max area of properties by manager
area_management <- pad_attributes %>%
  group_by(d_Mang_Nam) %>%
  summarise(mean_area=mean(GIS_Acres),
            min(GIS_Acres),
            max(GIS_Acres))

#count properties smaller than 180,000 acres (27 km x 27 km)
count_small_properties <- pad_attributes %>% 
  filter(GIS_Acres < 180000) %>% 
  nrow()
#percent of properties #99.8% of properties are smaller than 180k acres
count_small_properties  / nrow(pad_attributes)
