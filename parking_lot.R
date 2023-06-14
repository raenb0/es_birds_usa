# parking lot code

# Create an empty dataframe to store the results
guild_results_cna <- data.frame(Guild = character(), sum = numeric(), stringsAsFactors = FALSE)

# Loop through each SpatRaster object in the list
for (i in seq_along(pct_pop_per_group_list_cna)) {
  raster <- pct_pop_per_group_list_cna[[i]]
  
  # Calculate the sum of all pixels from all layers in the SpatRaster
  raster_sum <- global(raster, fun="sum", na.rm=TRUE)
  
  # Add the results to the dataframe
  guild_results_cna <- rbind(guild_results_cna, data.frame(Guild = names(pct_pop_per_group_list_cna)[i], sum = raster_sum))
}
# Reset row names
row.names(guild_results_cna) <- NULL

# Print the resulting dataframe
print(guild_results_cna)

# repeat calculating sums by guild, unmasked version
guild_results_all <- data.frame(Guild = character(), sum = numeric(), stringsAsFactors = FALSE)

# Loop through each SpatRaster object in the list
for (i in seq_along(pct_pop_per_group_list_all)) {
  raster <- pct_pop_per_group_list_all[[i]]
  
  # Calculate the sum of all pixels from all layers in the SpatRaster
  raster_sum <- global(raster, fun="sum", na.rm=TRUE)
  
  # Add the results to the dataframe
  guild_results_all <- rbind(guild_results_all, data.frame(Guild = names(pct_pop_per_group_list_all)[i], sum = raster_sum))
}

row.names(guild_results_all) <- NULL
print(guild_results_all) #these appear to be counts of species, not percentages of populations
