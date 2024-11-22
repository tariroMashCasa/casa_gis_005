# this is a spatial autocorrelation analysis of thefts in London
# in 2022 & 2023

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
library(spdep)
library(spdep)
library(RColorBrewer)




# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------


london_gdf <- st_read("/Users/tariromashongamhende/Documents/Documents - Tariro’s MacBook Pro/casa_masters/term_1/Quant_Methods/group_assignment/h3_index_l8_thefts_2022_2023.geojson")

# clean up the column names 
london_gdf_w_thefts <- london_gdf %>% janitor::clean_names(.,"lower_camel")

# to quickly plot these datasets on top of each other you can do this
plot(london_gdf_w_thefts)

# -----------------------------------------------------------------------------
# Analysis
# -----------------------------------------------------------------------------

# 1. Calculate a weights list from your gdf

#First calculate the centroids of all Wards in London

coordsW <- london_gdf_w_thefts%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- london_gdf_w_thefts %>%
  poly2nb(., queen=T)

# to get a quick overview about the neighbourhood relationships you can call
# the summary code below - note links basically means neighbours

summary(LWard_nb)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(london_gdf_w_thefts$geometry, add=T)

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

I_LWard_Global_Density <- london_gdf_w_thefts %>%
  pull(theft_density) %>%
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
  london_gdf_w_thefts %>%
  pull(theft_density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

# 3.3 Getis Ord General G
# to interpret the results form the Getis Ord General G statistic
# This aims to tell us if high values are clustering or low values are
# clustering. A value greater than the expected indicates high values are clustering
# and a value lower than the expected indicates that lower values are clustering

G_LWard_Global_Density <- 
  london_gdf_w_thefts %>%
  pull(theft_density) %>%
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

I_LWard_Local_count <- london_gdf_w_thefts %>%
  pull(number_of_thefts) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- london_gdf_w_thefts %>%
  pull(theft_density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

# to pull these values back into our gdf we do this:
london_gdf_w_thefts <- london_gdf_w_thefts %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

# when plotting the z scores we should set the breaks manually as we know what
# certain deviations from the mean mean.

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

# create a diverging color palette 
MoranColours<- rev(brewer.pal(8, "RdGy"))

# plot an interactive map
tmap_mode("view")

tm_shape(london_gdf_w_thefts) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              alpha=0.75,
              title="Local Moran's I, Thefts in London")

# 4.1 Local Getis Ord Gi statistic
# this statistics is useful in identifying hotspots, coldspots, and positive 
# (diamonds) and negative outliers (donuts)

Gi_LWard_Local_Density <- london_gdf_w_thefts %>%
  pull(theft_density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

# let's add the z scores from the Getis Ord Gi statistic test back to our gdf 
# of the london wards

london_gdf_w_thefts <- london_gdf_w_thefts %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

# and plot the results

library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(london_gdf_w_thefts) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")

