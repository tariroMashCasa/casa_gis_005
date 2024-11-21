# -----------------------------------------------------------------------------
# Import libraries
# -----------------------------------------------------------------------------

library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(tidyverse)
library(janitor)




# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

london_gdf <- st_read("week_7_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

# convert the london_gdf to WGS84

# london_gdf <- london_gdf %>% sf::st_transform(., 4326 
# london_gdf <- st_transform(london_gdf, 4326) 

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

blue_plaques_gdf <- st_transform(blue_plaques_gdf, st_crs(london_gdf)) 


st_crs(london_gdf)
columns(blue_plaques_df)
# to quickly plot these datasets on top of each other you can do this
tmap_mode("plot")

tmap::tm_shape(london_gdf) +
  tm_polygons(col = NA, alpha = 0.5) 

tmap::tm_shape(blue_plaques_gdf) + 
  tm_dots(col = "blue")




# -----------------------------------------------------------------------------
# Preprocess 
# -----------------------------------------------------------------------------








# -----------------------------------------------------------------------------
# Analyse
# -----------------------------------------------------------------------------







# -----------------------------------------------------------------------------
# Visualisation
# -----------------------------------------------------------------------------






# -----------------------------------------------------------------------------
# Save files
# -----------------------------------------------------------------------------


