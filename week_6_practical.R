
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

# so the reason the images don't line up is there are clearly some incorrect ones
# to adjust for this we'll do a spatial join to get the inscope blue plaques
# i.e. avoid the ones in the middle of the atlantic ocean

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

# last thing before the data is now ready for the analysis using spatstats

# you need to set up an observation window

window <- as.owin(Harrow)
plot(window)

# create a sp object (for spatstats)

blue_plaques_sub <- blue_plaques_in_harrow_gdf %>%
                    as(., "Spatial")

# create a ppp object 
blue_plaques_sub_ppp <- ppp(x=blue_plaques_sub@coords[,1],
                            y=blue_plaques_sub@coords[,2],
                            window=window)

# visualise this to confirm that everything has worked as planned

blue_plaques_sub_ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")

#------------------------------------------------------------------------------
# Carry out point pattern analyses
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Analysis part 1: Kernel Density Estimation
#------------------------------------------------------------------------------

blue_plaques_sub_ppp %>%
  density(., sigma=500) %>%
  plot()

# raising the value of sigma basically has the effect of looking over a greater
# area from a metric like count of x within y km

blue_plaques_sub_ppp %>%
  density(., sigma=5000) %>%
  plot()

# reducing the value of sigma basically has the effect of looking over a smaller
# area from a metric like count of x within y km

blue_plaques_sub_ppp %>%
  density(., sigma=5) %>%
  plot()



#------------------------------------------------------------------------------
# Analysis part 2: Quadrat Analysis
#------------------------------------------------------------------------------

#First plot the points
plot(blue_plaques_sub_ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
blue_plaques_sub_ppp %>%
  quadratcount(.,nx = 10, ny = 10)%>%
  plot(., add=T, col="red")

# next we will compute what the quadrat grid would look like for a poisson
# distribution

#run the quadrat count
Qcount <- blue_plaques_sub_ppp %>%
  quadratcount(.,nx = 10, ny = 10) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

# adjust check if the data type is not numeric as you will need to change it to 
# this if this is the case

Qcount %>% 
  summarise_all(class)

# Now you will need to compute Quadrat analysis
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

# now in order for us to reject the poisson distribution we need to use the
# chi-test - in order to reject the null hypothesis (i.e. there is no pattern)
# the p_value of the ChiSquared test should be below 0.05

teststats <- quadrat.test(blue_plaques_sub_ppp, nx = 10, ny = 10)
teststats

# so the p-value we get is 0.25 which means we can't say that this spatial 
# pattern in the blue plaques is not random

plot(blue_plaques_sub_ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")

# the larger your bins the worse the p-value becomes 
# the smaller your bins (i.e. the more grid cells you have) the lower your 
# p-value - why is this the case? is this MAUP in practice?



#------------------------------------------------------------------------------
# Analysis part 3: Ripley's K
#------------------------------------------------------------------------------

K <- blue_plaques_sub_ppp %>%
  Kest(., correction="border") %>%
  plot()

# When the black line (observed) is greater than the line, we can say that there
# is some degree of clustering going on there whereas when it drops below the 
# the opposite is true (spatial separation??)

Kval <- as.data.frame(Kest(blue_plaques_sub_ppp, correction = "Ripley"))

Kval

#------------------------------------------------------------------------------
# Analysis part 4: DBSCAN
#------------------------------------------------------------------------------

library(fpc)

#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(london_gdf)


#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- blue_plaques_sub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(london_gdf$geometry, add=T)

# An alternative way to get your epsilon value is to use K Nearest Neighbors 
# Distribution Plot (kNNdistplot()) in the dbscan package (fpc). 

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)


#------------------------------------------------------------------------------
# Visualise your results
#------------------------------------------------------------------------------

# you can plot the output from your analysis in DBSCAN over a map using ggplot

library(ggplot2)

db
# you can access the actual clusters each observation belongs to in the usual 
# way you can access columns - below is the pythonic way but you ccan do the 
# same by using the $ symbol

db["cluster"]
# create a new column using mutate like so
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)

# or alternatively you can do it the pythonic way
BluePlaquesSubPoints["db_cluster"] = db$cluster

# next is the creation of convex hull objects 
chulls <- BluePlaquesSubPoints %>%
  group_by(db_cluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)
# so as 0 is the valye given by DBSCAN to all values that are not in a cluster
# we will drop them for now

chulls <- chulls %>% dplyr::filter(!c(db_cluster)==0)

# now you can plot these using ggplot

dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=db_cluster, fill=db_cluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=db_cluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()


# To add a basemap you can first create a bounding box based on the Harrow geom
harrow_wgs_bb <- Harrow %>% st_transform(., 4326) %>% st_bbox()



# the osm library in R doesn't work on new Macbooks - so ignore the code below

#library(OpenStreetMap)
#basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
#                                  zoom=NULL,
#                                  "osm")

# now convert this to BNG

st_transform(harrow_wgs_bb, st_crs(Harrow))


# the code below will now try to plot a basemap, the convex hulls polygons 
# and the points 
# 

tmap_mode("view")
tm_basemap(c(StreetMap = "OpenStreetMap",
             TopoMap = "OpenTopoMap")) +
  tm_tiles(c(TonerHybrid = "Stamen.TonerHybrid"))+
  tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tmap::tm_shape(blue_plaques_in_harrow_gdf) + 
  tm_dots(col = "blue")


# to view the clusters as convex hulls you first need to convert the chulls 
# dataframe to a sf object

chulls_sf <- chulls %>% st_as_sf(
  coords = c("coords.x1", "coords.x2"), crs=st_crs(london_gdf)
)

chulls_tmap_ready <- chulls_sf %>%
                    group_by( dbcluster ) %>%
                    summarise( geometry = st_combine( geometry ) ) %>%
                    st_convex_hull()

# next you need to then convert this dataframe to convex hulls based on the 
# hull column




#create simple feature

            
#chulls_geoms <- geom_polygon(data = chulls, 
#             aes(coords.x1,coords.x2, group=db_cluster), 
#             alpha = 0.5) 

tmap_mode("view")
tm_basemap(c(StreetMap = "OpenStreetMap",
             TopoMap = "OpenTopoMap")) +
  tm_tiles(c(TonerHybrid = "CartoDB.Positron"))+
  tm_shape(Harrow) +
  tm_polygons(col = "magenta", alpha = 0.05) +
  tm_shape(chulls_tmap_ready)+
  tm_polygons(col="darkorange", alpha=0.6)+
  tmap::tm_shape(blue_plaques_in_harrow_gdf) + 
  tm_dots(col = "white",size=0.1)+
  tmap::tm_shape(blue_plaques_in_harrow_gdf) + 
  tm_dots(col = "midnightblue",size=0.05)
