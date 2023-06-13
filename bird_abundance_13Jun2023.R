# Bird abundance within CNA areas within USA
# June 13 2023
# Rachel Neugarten

# Notes from Courtney:
# The "final_species_selection.rds" table has a bunch of columns for the 479 species of the analysis. It has all the columns of ebirdst_runs plus our guild groups. 
# Our guilds groups are in a column named "sps_groups" (the last column of the table).
# The tiff files in the zip are for all the species with abundnace > 0 in our study area (more than our 479 species). 
# You can filter them by their file name using the final_species_slection table and the "species_code" column.
# The code I used for this filtering of the tif files is:
library(data.table)
sps_sel_all_vars <- readRDS("data/final_species_selection.rds")
pct_pop_files <- list.files("data/SnT_sp_rasters/pct_pop", 
                            pattern = ".tif", full.names = T)

tiffs_sps <- str_extract(pct_pop_files, "(?<=pct_pop\\/)(.*)(?=_a)")
tifs_path_sel <- pct_pop_files[tiffs_sps %in% sps_sel_all_vars$species_code]
sps_sel <- tiffs_sps[tiffs_sps %in% sps_sel_all_vars$species_code]

pct_pop_per_sp_rast <- rast(tifs_path_sel)
names(pct_pop_per_sp_rast) <- sps_sel

