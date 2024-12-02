# this is the practical for week 8 on spatial regressions

# -----------------------------------------------------------------------------
# Library imports 
# -----------------------------------------------------------------------------

#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(plotly)
library(broom)
library(mapview)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(tidyr)




# -----------------------------------------------------------------------------
# Load data 
# -----------------------------------------------------------------------------
Londonwards <- sf::st_read("week_7_data/London-wards-2014/London-wards-2014_ESRI/London_Ward_CityMerged.shp")

#check the data
tmap::qtm(Londonwards)

#read in some attribute data
LondonWardProfiles <- readr::read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               col_names = TRUE, 
                               locale = readr::locale(encoding = 'Latin1'))


#check all of the columns have been read in correctly
Datatypelist <- LondonWardProfiles %>% 
  dplyr::summarise_all(class) %>%
  tidyr::pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")


Datatypelist

#We can use readr to deal with the issues in this dataset - which are to do with text values being stored in columns containing numeric values

#read in some data - couple of things here. Read in specifying a load of likely 'n/a' values, also specify Latin1 as encoding as there is a pound sign (£) in one of the column headers - just to make things fun!

LondonWardProfiles <- readr::read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = readr::locale(encoding = 'Latin1'), 
                               col_names = TRUE)

#check all of the columns have been read in correctly
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LondonWardProfiles

#merge boundaries and data
LonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles, 
            by = c("GSS_CODE" = "New code"),
            )

# drop any NA values
LonWardProfiles <- LonWardProfiles %>% filter(!if_any(c('Average GCSE capped point scores - 2014'), is.na))

#let's map our dependent variable to see if the join has worked:
tmap_mode("plot")
tmap::qtm(LonWardProfiles, 
    fill = "A-Level Average Point Score Per Student - 2013/14", 
    borders = NULL,  
    fill.palette = "Blues")


#might be a good idea to see where the secondary schools are in London too
london_schools <- read_csv("https://data.london.gov.uk/download/london-schools-atlas/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")

#from the coordinate values stored in the x and y columns, which look like they are latitude and longitude values, create a new points dataset
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)

lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

tmap_mode("plot")
qtm(lond_sec_schools_sf)

# -----------------------------------------------------------------------------
# Data wrangling 
# -----------------------------------------------------------------------------






# -----------------------------------------------------------------------------
# Analysis 
# -----------------------------------------------------------------------------

# Regression basics

q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=LonWardProfiles)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


# -----------------------------------------------------------------------------
# Visualisation 
# -----------------------------------------------------------------------------


# Running a regression model in R

#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

# drop any NA values
Regressiondata <- Regressiondata %>% filter(!if_any(c(average_gcse_capped_point_scores_2014), is.na))

#now model
model1 <- Regressiondata %>%
  lm(average_gcse_capped_point_scores_2014 ~
       unauthorised_absence_in_all_schools_percent_2013,
     data=.)

#show the summary of those outputs
summary(model1)


library(broom)
tidy(model1)

# glance is an alternative way to get the summary insights from the
# linear regression model - it also comes from the broom library

glance(model1)

# in order to see the actual predictions for each point - similar to how you 
# you would use .predict you can use the tidypredict library as below:

library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model1)


# Tidymodels

# this is another way to train models being developed in R allowing easy switching
# of the type of model you are trying to train i.e. having linear models or forest models
# by adjusting the engine argument

library(tidymodels)

# set the model
lm_mod <- linear_reg()

# fit the model
lm_fit <- 
  lm_mod %>% 
  fit(average_gcse_capped_point_scores_2014 ~
        unauthorised_absence_in_all_schools_percent_2013,
      data=Regressiondata)

# we cover tidy and glance in a minute...
tidy(lm_fit)

glance(lm_fit)


# 8.5.7 Assumptions Underpinning Linear Regression

## Assumption 1 - There is a linear relationship between the dependent and 
## independent variables

#let's check the distribution of these variables first

ggplot(LonWardProfiles, aes(x=`Average GCSE capped point scores - 2014`)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(LonWardProfiles, aes(x=`Unauthorised Absence in All Schools (%) - 2013`)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)

# the distributions look very different when we look at house prices instead

library(ggplot2)

# from 21/10 there is an error on the website with 
# median_house_price_2014 being called median_house_price<c2>2014
# this was corrected around 23/11 but can be corrected with rename..

LonWardProfiles <- LonWardProfiles %>%
  #try removing this line to see if it works...
  dplyr::rename(median_house_price_2014 =`Median House Price (£) - 2014`)%>%
  janitor::clean_names()

ggplot(LonWardProfiles, aes(x=median_house_price_2014)) + 
  geom_histogram()

# to see that this relationship between house prices and gcse results is 
# most likely not a linear one we can plot the data on a scatter plot 

qplot(x = median_house_price_2014, 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)

# But what if you really want to use that column which has a non-linear relationship
# you can apply some transformations to make it seem to have a linear relationship
# some of these include:

# Using Tukey's ladder of transformations
symbox(~median_house_price_2014, 
       LonWardProfiles, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

qplot(x = (median_house_price_2014)^-1, 
      y = average_gcse_capped_point_scores_2014,
      data=LonWardProfiles)

qplot(x = log(median_house_price_2014), 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)

# to interpret this - the way to make the house price data normally distributed 
# would be to raise the data to the box plot with no outliers outside the whiskers
# in this case this would be raising the column to the power of -1

ggplot(LonWardProfiles, aes(x=(median_house_price_2014)^-1)) + 
  geom_histogram()

# as you can see the data looks a lot more normally distributed but doing this 
# can allow you to model such data with linear regression models but at the loss 
# of the model's explainability 


## Assumption 2 - The residuals in your model should be normally distributed

# As in python this is basically a check of the distribution of the residuals
# from your linear regression model

#save the residuals into your dataframe

model_data <- model1 %>%
  augment(., Regressiondata)

#plot residuals
model_data %>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

## Assumption 3 - No Multicolinearity in the independent variables
## When you get high numbers of variables you need to be careful of multicollinearity effects
## that is that the variables are independent and uncorrelated with each other 

Regressiondata2<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014)
# drop any NA values
Regressiondata2 <- Regressiondata2 %>% filter(!if_any(c(average_gcse_capped_point_scores_2014), is.na))

model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = Regressiondata2)

#show the summary of those outputs
tidy(model2)

glance(model2)


#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata2)

# also add them to the shapelayer
LonWardProfiles <- LonWardProfiles %>%
  mutate(model2resids = residuals(model2))

LonWardProfiles

# to check for multicollinearity you can use the corr library

library(corrr)

Correlation <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014) %>%
  mutate(median_house_price_2014 =log(median_house_price_2014))%>%
  correlate() %>%
  # just focus on GCSE and house prices
  focus(-average_gcse_capped_point_scores_2014, mirror = TRUE) 


Correlation

#visualise the correlation matrix
rplot(Correlation)


# Another way to check for multi-collinearity is to look at Variance Inflation Factor (VIF)

vif(model2)


# to do this against a large number of different variable you would do the
# the following: 

position <- c(10:74)

Correlation_all<- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(position)%>%
  correlate()

Correlation_all

rplot(Correlation_all)


## Assumption 4 - Homoscedasticity

# The best way to check for homo/hetroscedasticity is to plot the residuals
# in the model against the predicted values.

#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

# In the plots here we are looking for:
  
# * Residuals vs Fitted: a flat and horizontal line. This is looking at the linear relationship assumption between our variables

# * Normal Q-Q: all points falling on the line. This checks if the residuals (observed minus predicted) are normally distributed

# * Scale vs Location: flat and horizontal line, with randomly spaced points. This is the homoscedasticity (errors/residuals in the model exhibit constant / homogeneous variance). Are the residuals (also called errors) spread equally along all of the data.

# * Residuals vs Leverage - Identifies outliers (or influential observations), the three largest outliers are identified with values in the plot

# you can do the same thing by using the performance library

library(performance)

check_model(model2, check="all")

## Assumption 5 - Independence of Errors
## This assumption simply states that the residual values (errors)
## in your model must not be correlated in any way

# You would typically look for standard autocorrelation like this:
#run durbin-watson test
DW <- durbinWatsonTest(model2)
tidy(DW)

# HOWEVER as it's spatial data we need to do some further checks
# check 1 is to plot your residuals on a choropleth map and see if there is any
# visible autocorrelation 

#now plot the residuals
tmap_mode("view")
#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(LonWardProfiles) +
  tm_polygons("model2resids",
              palette = "RdYlBu") +
  tm_shape(lond_sec_schools_sf) + tm_dots(col = "TYPE")

# the systematic way to test for this would be to use some of the spatial 
# methods from PPP analysis

#calculate the centroids of all Wards in London
coordsW <- LonWardProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

#Now we need to generate a spatial weights matrix 
#(remember from the lecture a couple of weeks ago). 
#We'll start with a simple binary matrix of queen's case neighbours

LWard_nb <- LonWardProfiles %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

plot(LWard_knn, st_geometry(coordsW), col="blue")

#create a spatial weights matrix object from these weights

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="W")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="W")

Queen <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

Nearest_neighbour <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

## Looking at the outputs from both the Queens and hte Nerest Neighbour spatial Weights
## Matrices we can see that there is some spatial autocorrelation we are observing which
## means anyone interpreting this model should be aware of this as well as the things we are 
## observing may be overstated as a result
Queen

Nearest_neighbour




# 
