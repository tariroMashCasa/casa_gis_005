# load all the libraries to use

library(sf)
library(here)
library(terra)
library(dplyr)
library(ggplot2)
library(tmap)
library(tidyverse)
#install.packages('countrycode')
library(countrycode)


#------------------------------------------------------------------------------
# Part 1: Load the datasets 
#------------------------------------------------------------------------------

# load the gdf file for the world 
world_gdf <- sf::st_read('World_Countries_(Generalized)_9029012925078512962.geojson')

# load the GII table using reader
index_data_df <- read_csv('HDR23-24_Composite_indices_complete_time_series.csv')

#------------------------------------------------------------------------------
# Part 2: Tidy and filter the index data
#------------------------------------------------------------------------------

columns_to_keep <- c('iso3','country','hdicode','gii_2010',	'gii_2019')

# filter the index data down to the subset of columns we're interested in 

index_data_df <- index_data_df %>% dplyr::select(all_of(columns_to_keep))

# some data has no values - we're going to drop these from the dataset 
non_null_index_data_df <- index_data_df %>% filter(!is.na(gii_2010)) %>% filter(!is.na(gii_2019))

# next drop some of the rows at the bottom of the data that are regional aggregations
country_level_index_data_df <- non_null_index_data_df %>% filter(!is.na(hdicode))

# use the countrycode package to get the iso3 code for the world_gdf file 
?countrycode

world_gdf["iso3"] <- countrycode(world_gdf$ISO, origin="iso2c", destination="iso3c")

#------------------------------------------------------------------------------
# Part 3: Join the data to the country file and calculate the difference in GII
#------------------------------------------------------------------------------

# Join the data together using the ISO3 code

world_w_gii_gdf <- world_gdf %>% left_join(country_level_index_data_df, by = c("iso3"))
world_w_gii_gdf["gii_diff_2010_and_2019"] <- world_w_gii_gdf$gii_2019 - world_w_gii_gdf$gii_2010

#------------------------------------------------------------------------------
# Part 4: Visualise the gii data 
#------------------------------------------------------------------------------
 
# make a simple plot to show the gii_2010 data 
plot(world_w_gii_gdf %>% select(all_of(c("gii_2010","geometry"))))

# make a simple plot to show the gii_2019 data 
plot(world_w_gii_gdf %>% select(all_of(c("gii_2019","geometry"))))
 
# make a simple plot to show the difference in gii data 
plot(world_w_gii_gdf %>% select(all_of(c(gii_diff_2010_and_2019","geometry"))))

