# -----------------------------------------------------------------------------
# Part 0: Load the libraries you're going to use
# -----------------------------------------------------------------------------

library(sf)
library(here)
library(tidyverse)
library(tmap)
library(janitor)

# -----------------------------------------------------------------------------
# Part 1: Load your datasets
# -----------------------------------------------------------------------------

# uk_outline_gdf <- st_read("data/gadm41_GBR.gpkg")

# load the osm data 

osm_poi_gdf <- st_read("data/greater-london-latest-free.shp/gis_osm_pois_free_1.shp")
osm_poi_gdf <- osm_poi_gdf %>% janitor::clean_names(.,'lower_camel')

# filter this data only for the hotels using the column fclass

hotels_gdf <- osm_poi_gdf %>% filter(fclass=="hotel")
hotels_gdf['counter'] = 1
hotels_gdf$counter <- as.numeric(hotels_gdf$counter)

# load the london polygon file 

london_gdf <- st_read('data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
london_gdf <- london_gdf %>% janitor::clean_names(.,'lower_camel')


# load the airbnb data listings
airbnb_listings_df <- read_csv('data/listings.csv')
airbnb_listings_df <- airbnb_listings_df %>% janitor::clean_names(.,'lower_camel')

# load the number of bedrooms data from the ONS

ons_num_bedrooms_df <- read_csv('data/ons_nomis_extract_num_bedrooms.csv')
ons_num_bedrooms_df <- ons_num_bedrooms_df %>% janitor::clean_names(.,'lower_camel')
ons_num_bedrooms_df['total_bedrooms'] <- ons_num_bedrooms_df$bedroomsAllCategoriesNumberOfBedroomsMeasuresValue
ons_num_bedrooms_df['gssCode'] <- ons_num_bedrooms_df$geographyCode
ons_num_bedrooms_df <- ons_num_bedrooms_df %>% select(c(gssCode,total_bedrooms))

# load the world cities data from esri / ArcMap
world_cities <- st_read('data/World_Cities.geojson')
world_cities <- world_cities %>% janitor::clean_names(.,'lower_camel')


# -----------------------------------------------------------------------------
# Part 2: Transform and tidy the music
# -----------------------------------------------------------------------------

# first we're going to get the counts of the hotels joined onto the the london_gdf variable
# the london data has a different crs to the hotels data 
# change the london data to WGS84

# to assign a crs you can do the following 

london_gdf <- sf::st_transform(london_gdf, 4326 )

london_gdf_w_hotels <- st_join(london_gdf, hotels_gdf) 

# aggregate the number of hotels per gssCode and name

lad_gdf_w_num_hotels <- london_gdf_w_hotels %>% group_by(gssCode) %>% summarise(numHotels = sum(counter))

# next we need to add the number of airbnb listings in each local authority district
# first convert the data frame to a sf geodataframe - this is the same as using 
# gp.points_from_xy() in python

airbnb_listings_gdf <- st_as_sf(airbnb_listings_df, coords = c("longitude", "latitude"), crs = 4326)
airbnb_listings_gdf['counter'] = 1
airbnb_listings_gdf$counter <- as.numeric(airbnb_listings_gdf$counter)
# next you will need to do a spatial join to get each listing matched against each LAD

colnames(airbnb_listings_gdf)






london_gdf_w_airbnb_listings <- st_join(london_gdf, airbnb_listings_gdf %>% select(c(id, counter, counter))) 

london_gdf_w_airbnb_listings <- london_gdf_w_airbnb_listings %>% st_drop_geometry() %>% select(c(gssCode, counter))
# aggregate the number of listings per gssCode 
lad_gdf_w_num_airbnbs <- london_gdf_w_airbnb_listings %>% group_by(gssCode) %>% summarise(numAirbnbs = sum(counter))

# now join the hotel and airbnb gdfs together to be a single gdf with the original london_gdf
lad_gdf_w_num_airbnbs <- lad_gdf_w_num_airbnbs %>% st_drop_geometry()
lad_gdf_w_num_hotels <- lad_gdf_w_num_hotels %>% st_drop_geometry()

lad_gdf_w_hotels_and_airbnb_volumes <- london_gdf %>% left_join(lad_gdf_w_num_hotels ,by=c("gssCode"))  %>% left_join(lad_gdf_w_num_airbnbs ,by=c("gssCode")) %>% left_join(ons_num_bedrooms_df ,by=c("gssCode")) 
# now create two columns for the normalised values 
lad_gdf_w_hotels_and_airbnb_volumes["numHotelsPerBedroom"] <- lad_gdf_w_hotels_and_airbnb_volumes$numHotels / lad_gdf_w_hotels_and_airbnb_volumes$total_bedrooms
lad_gdf_w_hotels_and_airbnb_volumes["numAirbnbsPerBedroom"] <- lad_gdf_w_hotels_and_airbnb_volumes$numAirbnbs / lad_gdf_w_hotels_and_airbnb_volumes$total_bedrooms

plot(lad_gdf_w_hotels_and_airbnb_volumes %>% select(c(numHotels, numAirbnbs,total_bedrooms, numHotelsPerBedroom,numAirbnbsPerBedroom, geometry)))





colnames(airbnb_listings_df)


# -----------------------------------------------------------------------------
# Part 3: Visualise your analysis
# -----------------------------------------------------------------------------

tmap_mode("plot")
tm_shape(lad_gdf_w_hotels_and_airbnb_volumes) + 
  tm_polygons("numHotels", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Number of hotels",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Number of Hotels in London")

tmap_mode("plot")
tm_shape(lad_gdf_w_hotels_and_airbnb_volumes) + 
  tm_polygons("numAirbnbs", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Number of hotels",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Number of Hotels in London")

tmap_mode("plot")
tm_shape(lad_gdf_w_hotels_and_airbnb_volumes) + 
  tm_polygons("numHotelsPerBedroom", 
              style="pretty",
              palette="YlOrBr",
              midpoint=NA,
              title="Number of hotels per bedroom",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Number of Hotels per bedroom in London")

tmap_mode("plot")
tm_shape(lad_gdf_w_hotels_and_airbnb_volumes) + 
  tm_polygons("numAirbnbsPerBedroom", 
              style="pretty",
              palette="YlOrBr",
              midpoint=NA,
              title="Number of Airbnbs per bedroom",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Number of Airbnbs per bedroom in London")
