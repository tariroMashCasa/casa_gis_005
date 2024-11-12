
# this is the source of the location for the data for the plaques in GB:
# url: https://openplaques.org/data

# setup the libraries 
#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(tidyverse)
library(janitor)

#------------------------------------------------------------------------------
# Load datasets
#------------------------------------------------------------------------------

london_gdf <- st_read("week_6_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

# convert the london_gdf to WGS84

# london_gdf <- london_gdf %>% sf::st_transform(., 4326 
london_gdf <- st_transform(london_gdf, 4326) 

?st_transform()


# load the blue plaques data 
blue_plaques_df <- read_csv("week_6_data/open-plaques-gb-2021-05-14.csv")

# clean up the column names 
london_gdf <- london_gdf %>% janitor::clean_names(.,"lower_camel")

blue_plaques_df <- blue_plaques_df %>% janitor::clean_names(.,"lower_camel")
# looks like there are some nulls for this dataset
# we will first filter for London
london_blue_plaques_df <- dplyr::filter(blue_plaques_df, area == "London")

# there are still some cases with nas in the latitude and longitude columns
# so we will filter these out explicitly

london_blue_plaques_df <- london_blue_plaques_df %>% filter(!if_any(c(longitude, latitude), is.na))


# convert this csv into a sf object 
blue_plaques_gdf <- st_as_sf(x = london_blue_plaques_df,                         
               coords = c("longitude", "latitude"),
               crs = "4326")
blue_plaques_gdf <-  sf::st_set_crs(blue_plaques_gdf,4326)

columns(blue_plaques_df)
# to quickly plot these datasets on top of each other you can do this
tmap_mode("plot")

tmap::tm_shape(london_gdf) +
  tm_polygons(col = NA, alpha = 0.5) 
  
tmap::tm_shape(blue_plaques_gdf) + 
  tm_dots(col = "blue")

# so the reason the images don't line up is there are clearly some incorrect ones
# to adjust for this we'll do a spatial join to get the inscope blue plaques
# i.e. avoid the ones in the middle of the atlantic ocean

terra::crs(london_gdf)
terra::crs(blue_plaques_gdf)

london_blue_plaques_gdf <- st_join(blue_plaques_gdf,london_gdf)

# filter out anything that didn't join 
london_blue_plaques_gdf_2 <- london_blue_plaques_gdf %>% filter(!if_any(c(name), is.na))

# let's try to replot the data agin

# to quickly plot these datasets on top of each other you can do this
tmap_mode("plot")

tmap::tm_shape(london_gdf) +
  tm_polygons(col = NA, alpha = 0.5) +
tmap::tm_shape(london_blue_plaques_gdf_2) + 
    tm_dots(col = "blue")


#------------------------------------------------------------------------------
# Preprocess datasets
#------------------------------------------------------------------------------

# we will need to select the borough of Harrow for our analysis

Harrow <- london_gdf %>% dplyr::filter(name == "Harrow")

# now also filter the blue plaques in Harrow
blue_plaques_in_harrow_gdf <- london_blue_plaques_gdf_2 %>% dplyr::filter(name == "Harrow")

# plot the data again to check everything is fine
tmap_mode("plot")

tmap::tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tmap::tm_shape(blue_plaques_in_harrow_gdf) + 
  tm_dots(col = "blue")

# the data is now ready for the analysis usins spatstats

#------------------------------------------------------------------------------
# Carry out point pattern analyses
#------------------------------------------------------------------------------





#------------------------------------------------------------------------------
# Visualise your results
#------------------------------------------------------------------------------

