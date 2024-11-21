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

# Replace NA in column with no plaques only
london_gdf_w_num_blue_plaques$number_of_plaques[is.na(london_gdf_w_num_blue_plaques$number_of_plaques)] <- 0

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

# 1. Calculate a weights list from your gdf
library(spdep)

#First calculate the centroids of all Wards in London

coordsW <- london_gdf_w_num_blue_plaques%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- london_gdf_w_num_blue_plaques %>%
  poly2nb(., queen=T)

# to get a quick overview about the neighbourhood relationships you can call
# the summary code below - note links basically means neighbours

summary(LWard_nb)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(london_gdf_w_num_blue_plaques$geometry, add=T)

# 2. Next you make the weights matrix 

#create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

sum(Lward.lw)
# this is a row standardised version of the code above
sum(Lward.lw[1,])


# 3. Spatial Autocorrelation


Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

# 3.1 Moran's I - Global assessment of spatial autocorrelation
# to interpret the results form the Moran's I statistic value that are close 
# 1 indicate higher degrees of clustering and -1 indicate dispersion (separation)

I_LWard_Global_Density <- london_gdf_w_num_blue_plaques %>%
  pull(blue_plaque_density_per_hectares) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

# 3.2 Geary's C 
# to interpret the results form the Geary's C statistic
# This aims to tell us if similar values are clustering or dissimilar values are
# clustering. A value of 1 or higher typically indicates clustering of dissimilar
# values and a value closer to 0 indicates clustering of similar values).
# Max value is 2 and min value is 0.

C_LWard_Global_Density <- 
  london_gdf_w_num_blue_plaques %>%
  pull(blue_plaque_density_per_hectares) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

# 3.3 Getis Ord General G
# to interpret the results form the Getis Ord General G statistic
# This aims to tell us if high values are clustering or low values are
# clustering. A value greater than the expected indicates high values are clustering
# and a value lower than the expected indicates that lower values are clustering

G_LWard_Global_Density <- 
  london_gdf_w_num_blue_plaques %>%
  pull(blue_plaque_density_per_hectares) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density
#------------------------------------------------------------------------------
# 4. Understanding local clustering 
#------------------------------------------------------------------------------

# 4.1 Local Moran's I
# Once you know there is some clustering going on you want to know where this is 
# happening. To do this you can use Local Moran's I statistic

#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- london_gdf_w_num_blue_plaques %>%
  pull(number_of_plaques) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- london_gdf_w_num_blue_plaques %>%
  pull(blue_plaque_density_per_hectares) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

# to pull these values back into our gdf we do this:
london_gdf_w_num_blue_plaques <- london_gdf_w_num_blue_plaques %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

# when plotting the z scores we should set the breaks manually as we know what
# certain deviations from the mean mean.

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

# create a diverging color palette 
library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))

# plot an interactive map
tmap_mode("view")

tm_shape(london_gdf_w_num_blue_plaques) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              alpha=0.75,
              title="Local Moran's I, Blue Plaques in London")

# 4.1 Local Getis Ord Gi statistic
# this statistics is useful in identifying hotspots, coldspots, and positive 
# (diamonds) and negative outliers (donuts)

Gi_LWard_Local_Density <- london_gdf_w_num_blue_plaques %>%
  pull(blue_plaque_density_per_hectares) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

# let's add the z scores from the Getis Ord Gi statistic test back to our gdf 
# of the london wards

london_gdf_w_num_blue_plaques <- london_gdf_w_num_blue_plaques %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

# and plot the results

library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(london_gdf_w_num_blue_plaques) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")






# -----------------------------------------------------------------------------
# Save files
# -----------------------------------------------------------------------------


