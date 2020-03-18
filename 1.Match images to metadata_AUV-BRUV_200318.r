#### This script is to match metadata (images) to zones in Geographe bay and the Capes region ###

### TO IMPROVE THIS SCRIPT####
# Include missing polygons
# Reduce size on Ngary capes poly, to area of interest
# Make nice plot by zone at the end


# load required libraries ####
library( rgdal)
library( sp)
library( raster)
library( spatialEco)
library( tidyverse)
library( maptools)

# Clear memory ----
rm(list=ls())

# Set working directory ####
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)


## Read Shape file of GEO bay Marine Park zones ----
geobay <- readOGR(paste(w.dir, "Data", "shapefiles", "GeoBay.shp", sep='/'))

# see the shapefile you just loaded
plot(geobay)
geobay$ZoneName
gb.zones <- c("Habitat Protection Zone", "Special Purpose Zone (Mining Exclusion)",
              "National Park Zone", "Multiple Use Zone")

gb.zones

proj4string(geobay) # "+proj=longlat +ellps=GRS80 +no_defs"

 

#### Read the data from BRUVs 2014  ----
# This data from Benthobox and Brooke's script --
auv <- read.csv(paste(w.dir, "Data", "raw", "auv_detailed.percent.cover.csv", sep='/'))
# This is a data frame with 3 columns sample, lat and long, see:
head(auv)
# this is to see the properties of the data frame
str(auv) # there are 322 observations (bruvs), or points. 


#### Make spatial point object of BRUVS with lats and longs ####

# first make a copy of the data frame which you will then transfor in to spatial points
auvsp <- auv
# turn into spatial points with  'coordinates' function
coordinates(auv) <- c('longitude', 'latitude')
# check the projection of this sp points object: will be NA
proj4string(auv)
# give it the same projection as the shapefiles
proj4string(auv) <- "+proj=longlat +ellps=GRS80 +no_defs"
# check it worked
proj4string(auv)

auvsp$Sample[3]

points(auv)

#### Match AUV locations to the corresponding zone ####

# do this using the function point.in.poly from package spatialEco
# this code returns a spatial points object with more variables
# info from this page: https://gis.stackexchange.com/questions/287846/extract-data-of-polygon-shape-which-intersect-with-point-shape-r-and-create-data
auvcat <- point.in.poly(auv, geobay, sp = T)
# check it
auvcat
plot(geobay)
points(auvcat)
# turn spatial points into data frame
auvcatdf <- as.data.frame(auvcat)
# check it
head(auvcatdf) # you can see that for each sample and coordinates, there is a bunch of IUCN and names, this is
# because each shapefile or zone, has a column for name and for IUCN category
# if the point doesn't fall into that zone, it comes up as NA in those columns.
# to fix this we need to get rid of NAs and make only column for name, and only one for IUCN category:

# there are many ways to do this, but one way is to use the function coalesce
# for this make sure that your columns are not read as factors:

str(auvcatdf) # Na's in this df are BRUVs in state waters
# Save this data frame
write.csv(auvcatdf, paste(w.dir, "Data", "tidy", "Auv_detailed_zoning.csv", sep='/'))




### MATCH BRUV IMAGES TO METADATA ####

# Give file name --
filen <- "stereo-BRUVs_detailed.percent.cover.csv"

#### Read the metadata from BRUVs 2014 ####
t <- read.csv(paste(w.dir, "Data", "raw", filen, sep='/'))
# This is a data frame with 3 columns sample, lat and long, see:
head(t)
# this is to see the properties of the data frame
str(t) # there are 322 observations (bruvs), or points. 

#### Make spatial point object of BRUVS with lats and longs ####

# first make a copy of the data frame which you will then transfor in to spatial points
tsp <- t
# turn into spatial points with  'coordinates' function
coordinates(tsp) <- c('longitude', 'latitude')
# check the projection of this sp points object: will be NA
proj4string(tsp)
# give it the same projection as the shapefiles
proj4string(tsp) <- "+proj=longlat +ellps=GRS80 +no_defs"
# check it worked
proj4string(tsp)

tsp$Sample[3]

points(tsp)


#### Match BRUV locations to the zone ####

# do this using the function point.in.poly from package spatialEco
# this code returns a spatial points object with more variables
# info from this page: https://gis.stackexchange.com/questions/287846/extract-data-of-polygon-shape-which-intersect-with-point-shape-r-and-create-data
tcat <- point.in.poly(tsp, geobay, sp = T)
# check it
tcat
plot(geobay)
points(tcat)
str(tcat)
# turn spatial points into data frame
tcatdf <- as.data.frame(tcat)
# check it
head(tcatdf) # you can see that for each sample and coordinates, there is a bunch of IUCN and names, this is
# because each shapefile or zone, has a column for name and for IUCN category
# if the point doesn't fall into that zone, it comes up as NA in those columns.
# to fix this we need to get rid of NAs and make only column for name, and only one for IUCN category:

# there are many ways to do this, but one way is to use the function coalesce
# for this make sure that your columns are not read as factors:

str(tcatdf)
# Save this data frame ----
write.csv(tcatdf, paste(w.dir, "Data", "tidy", "Bruv2014_detailed_zoning.csv", sep='/'))













