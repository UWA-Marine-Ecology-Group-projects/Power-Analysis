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
bruvs2014 <- read.csv(paste(w.dir, "Data", "raw", "stereo-BRUVs_broad.percent.cover.csv", sep='/'))
# This is a data frame with 3 columns sample, lat and long, see:
head(bruvs2014)
# this is to see the properties of the data frame
str(bruvs2014) # there are 322 observations (bruvs), or points. 


#### Make spatial point object of BRUVS with lats and longs ####

# first make a copy of the data frame which you will then transfor in to spatial points
bruvs2014sp <- bruvs2014
# turn into spatial points with  'coordinates' function
coordinates(bruvs2014sp) <- c('longitude', 'latitude')
# check the projection of this sp points object: will be NA
proj4string(bruvs2014sp)
# give it the same projection as the shapefiles
proj4string(bruvs2014sp) <- "+proj=longlat +ellps=GRS80 +no_defs"
# check it worked
proj4string(bruvs2014sp)

bruvs2014sp$Sample[3]

points(bruvs2014sp)

#### Match BRUV locations to the corresponding zone ####

# do this using the function point.in.poly from package spatialEco
# this code returns a spatial points object with more variables
# info from this page: https://gis.stackexchange.com/questions/287846/extract-data-of-polygon-shape-which-intersect-with-point-shape-r-and-create-data
bruvscat <- point.in.poly(bruvs2014sp, geobay, sp = T)
# check it
bruvscat
plot(geobay)
points(bruvscat)
# turn spatial points into data frame
bruvscatdf <- as.data.frame(bruvscat)
# check it
head(bruvscatdf) # you can see that for each sample and coordinates, there is a bunch of IUCN and names, this is
# because each shapefile or zone, has a column for name and for IUCN category
# if the point doesn't fall into that zone, it comes up as NA in those columns.
# to fix this we need to get rid of NAs and make only column for name, and only one for IUCN category:

# there are many ways to do this, but one way is to use the function coalesce
# for this make sure that your columns are not read as factors:

str(bruvscatdf) # Na's in this df are BRUVs in state waters
# Save this data frame
write.csv(bruvscatdf, paste(w.dir, "Data", "tidy", "Bruvs2014_zoning.csv", sep='/'))



### split the data by zone name
bruvs_list <- split(bruvscatdf, bruvscatdf$ZoneName)
str(bruvs_list)
bruv.pa <- bruvs_list[-4]
str(bruv.pa)

# new listo into data frame
df <- do.call(rbind.data.frame, bruv.pa)
df
str(df)
names(df)

### Give new categories
write.csv(df, paste)

# Fxed it manually

bruv <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Bruv.csv")
str(bruv)

pcdata <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/Seagrass/BRUV_seagrass.csv")
str(pcdata)

pcbruv <- merge(bruv, pcdata, by = "sample", all = F)
str(pcbruv)

write.csv(pcbruv,"C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/PercentC_Bruv.csv" )

## now match it to percent cover


# to plot new point in map
dfsp <- df

coordinates(dfsp) <- ~coords.x1+coords.x2

points(dfsp)


### MATCH AUV IMAGES TO METADATA ####


#### Read the metadata from BRUVs 2014 ####
auv <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/auv_broad.percent.cover.csv")
# This is a data frame with 3 columns sample, lat and long, see:
head(auv)
# this is to see the properties of the data frame
str(auv) # there are 322 observations (bruvs), or points. 

#### Make spatial point object of BRUVS with lats and longs ####

# first make a copy of the data frame which you will then transfor in to spatial points
auvsp <- auv
# turn into spatial points with  'coordinates' function
coordinates(auvsp) <- c('longitude', 'latitude')
# check the projection of this sp points object: will be NA
proj4string(auvsp)
# give it the same projection as the shapefiles
proj4string(auvsp) <- "+proj=longlat +ellps=GRS80 +no_defs"
# check it worked
proj4string(auvsp)

auvsp$Sample[3]

points(auvsp)


#### Match BRUV locations to the zone ####

# do this using the function point.in.poly from package spatialEco
# this code returns a spatial points object with more variables
# info from this page: https://gis.stackexchange.com/questions/287846/extract-data-of-polygon-shape-which-intersect-with-point-shape-r-and-create-data
auvcat <- point.in.poly(auvsp, geobay, sp = T)
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

str(auvcatdf)

### split the data by zone name
auv_list <- split(auvcatdf, auvcatdf$ZoneName)
str(auv_list)
auv.pa <- auv_list[-4]
str(auv.pa)

# new listo into data frame
df <- do.call(rbind.data.frame, auv.pa)
df
names(df)

### Give new categories
write.csv(df, "C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Auv.csv")

# Fxed it manually

bruv <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Bruv.csv")
str(bruv)

#  This data was already merged with percen covers, so now, check sites
pcdata <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Auv.csv")
str(pcdata)
levels(pcdata$campaignid)

dsp <- pcdata
coordinates(dsp) <- ~coords.x1+coords.x2

plot(geobay)
points(dsp, pch=21, bg = dsp$campaignid["r20150526_024446_wa-gb-auv-01"])
legend()

write.csv(pcbruv,"C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/PercentC_auv.csv" )



# to plot new point in map
dfsp <- df

coordinates(dfsp) <- ~coords.x1+coords.x2

points(dfsp)


### MATCH TOWED VIDEO IMAGES TO METADATA ####

# Give file name --
filen <- "towed_broad.percent.cover.csv"

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


#### Match TV locations to the zone ####

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
write.csv(tcatdf, paste(w.dir, "Data", "tidy", "TV_zoning.csv", sep='/'))

### split the data by zone name
t_list <- split(tcatdf, tcatdf$ZoneName)
str(t_list)
length(t_list)
names(t_list)
t.pa <- t_list[-4]
str(t.pa)

# new listo into data frame
df <- do.call(rbind.data.frame, t.pa)
df
names(df)
str(df)

dfsp <- df
coordinates(dfsp) <- ~coords.x1+coords.x2
plot(geobay)
points(dfsp)


### Give new categories
write.csv(df, "C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/TV.csv")

# Fxed it manually

bruv <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Bruv.csv")
str(bruv)

#  This data was already merged with percen covers, so now, check sites
pcdata <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Auv.csv")
str(pcdata)
levels(pcdata$campaignid)

dsp <- pcdata
coordinates(dsp) <- ~coords.x1+coords.x2

plot(geobay)
points(dsp, pch=21, bg = dsp$campaignid["r20150526_024446_wa-gb-auv-01"])
legend()

write.csv(pcbruv,"C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/PercentC_auv.csv" )



# to plot new point in map
dfsp <- df

coordinates(dfsp) <- ~coords.x1+coords.x2

points(dfsp)










