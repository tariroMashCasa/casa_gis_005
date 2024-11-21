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

london_gdf <- st_read("week_7_data/London-wards-2014/London-wards-2014_ESRI/London_Ward_CityMerged.shp")

# convert the london_gdf to WGS84

# london_gdf <- london_gdf %>% sf::st_transform(., 4326 
# london_gdf <- st_transform(london_gdf, 4326) 

?st_transform()


# load the blue plaques data 
blue_plaques_df <- read_csv("week_7_data/open-plaques-gb-2021-05-14.csv")

# clean up the column names 
london_gdf <- london_gdf %>% janitor::clean_names(.,"lower_camel")

blue_plaques_df <- blue_plaques_df %>% janitor::clean_names(.,"lower_camel")
# looks like there are some nulls for this dataset
# we will first filter for London
london_blue_plaques_df <- dplyr::filter(blue_plaques_df, area == "London")

# there are still some cases with nas in the latitude and longitude columns
# so we will filter these out explicitly

london_blue_plaques_df <- london_blue_plaques_df %>% filter(!if_any(c(longitude, latitude), is.na))
london_blue_plaques_df <- london_blue_plaques_df %>% filter(colour=="blue")

# convert this csv into a sf object 
blue_plaques_gdf <- st_as_sf(x = london_blue_plaques_df,                         
                             coords = c("longitude", "latitude"),
                             crs = "4326")
blue_plaques_gdf <-  sf::st_set_crs(blue_plaques_gdf,4326)

blue_plaques_gdf <- st_transform(blue_plaques_gdf, st_crs(london_gdf)) 

terra::crs(london_gdf)
terra::crs(blue_plaques_gdf)

st_crs(london_gdf)

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



# -----------------------------------------------------------------------------
# Preprocess 
# -----------------------------------------------------------------------------

# we're going to convert this point data to a rate - density of blue plaques 
# (number of blue plaques / area of polygon)

# first we do a spatial join of the borough names onto the points
# calculate the number of plaques per gssCode
london_blue_plaques_gdf_2$id <- as.character(london_blue_plaques_gdf_2$id)
total_number_of_blue_plaques_per_gss_code <- london_blue_plaques_gdf_2 %>% group_by(gssCode) %>% summarize(number_of_plaques = n_distinct(id))

total_number_of_blue_plaques_per_gss_code %>% as.data.frame() %>% select(c(-geometry))

# join these onto the london_gdf by converting them both to dataframes and doing a string join
# then convert the result back to a sf object / geodataframe

london_gdf_w_num_blue_plaques <- st_as_sf(left_join(london_gdf %>% as.data.frame(),
          total_number_of_blue_plaques_per_gss_code %>% as.data.frame() %>% select(c(-geometry)),
          by="gssCode"))



london_gdf_w_num_blue_plaques["blue_plaque_density_per_hectares"] = london_gdf_w_num_blue_plaques$number_of_plaques / london_gdf_w_num_blue_plaques$hectares

# to quickly plot these datasets on top of each other you can do this
tmap_mode("plot")

tm_shape(london_gdf_w_num_blue_plaques) +
  tm_polygons("blue_plaque_density_per_hectares",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

# -----------------------------------------------------------------------------
# Analyse
# -----------------------------------------------------------------------------







# -----------------------------------------------------------------------------
# Visualisation
# -----------------------------------------------------------------------------






# -----------------------------------------------------------------------------
# Save files
# -----------------------------------------------------------------------------


