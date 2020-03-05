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



geobay <- readOGR( dsn="G:/My Drive/Analysis_Hannah_Geographe/SpatialData/Geo/GeoBay.shp")
# see the shapefile you just loaded
plot(geobay)
geobay$ZoneName
gb.zones <- c("Habitat Protection Zone", "Special Purpose Zone (Mining Exclusion)",
              "National Park Zone", "Multiple Use Zone")

gb.zones

proj4string(geobay) # "+proj=longlat +ellps=GRS80 +no_defs"

 

#### Read the metadata from BRUVs 2014 ####
bruvs2014 <- read.csv("//uniwa.uwa.edu.au/userhome/staff1/00093391/My Documents/GEO_SW/Data/2014-12_Geographe.Bay_stereoBRUVs_Metadata2.csv")
# This is a data frame with 3 columns sample, lat and long, see:
head(bruvs2014)
# this is to see the properties of the data frame
str(bruvs2014) # there are 322 observations (bruvs), or points. 

#### Make spatial point object of BRUVS with lats and longs ####

# first make a copy of the data frame which you will then transfor in to spatial points
bruvs2014sp <- bruvs2014
# turn into spatial points with  'coordinates' function
coordinates(bruvs2014sp) <- c('Longitude', 'Latitude')
# check the projection of this sp points object: will be NA
proj4string(bruvs2014sp)
# give it the same projection as the shapefiles
proj4string(bruvs2014sp) <- "+proj=longlat +ellps=GRS80 +no_defs"
# check it worked
proj4string(bruvs2014sp)

bruvs2014sp$Sample[3]


#### Match BRUV locations to the zone ####

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

str(bruvscatdf)

### split the data by zone name
bruvs_list <- split(bruvscatdf, bruvscatdf$ZoneName)
str(bruvs_list)
bruv.pa <- bruvs_list[-4]
str(bruv.pa)

# new listo into data frame
df <- do.call(rbind.data.frame, bruv.pa)
df
names(df)

### Give new categories
write.csv(df, "C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/Bruv.csv")

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


#### Read the metadata from BRUVs 2014 ####
t <- read.csv("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/DATA/towed_broad.percent.cover.csv")
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














####################### FROM HERE DOWN NOT DONE ########################


bruvscatdf$Sample<- as.character(bruvscatdf$Sample)
bruvscatdf$ORIG_NAME.1 <- as.character(bruvscatdf$ORIG_NAME.1)
bruvscatdf$ORIG_NAME.2 <- as.character(bruvscatdf$ORIG_NAME.2)
bruvscatdf$ORIG_NAME <- as.character(bruvscatdf$ORIG_NAME)
bruvscatdf$IUCN_CAT.1 <- as.character(bruvscatdf$IUCN_CAT.1)
bruvscatdf$IUCN_CAT.2 <- as.character(bruvscatdf$IUCN_CAT.2)
bruvscatdf$IUCN_CAT <- as.character(bruvscatdf$IUCN_CAT)
str(bruvscatdf)

# now coalesce Names and IUCN Categories
bruvscatdf <- bruvscatdf %>% mutate(Name = coalesce(bruvscatdf$ORIG_NAME.1,bruvscatdf$ORIG_NAME.2,bruvscatdf$ORIG_NAME),
                                    IUCNCategory = coalesce(bruvscatdf$IUCN_CAT.1,bruvscatdf$IUCN_CAT.2,bruvscatdf$IUCN_CAT)) 
                                    
# check it
head(bruvscatdf)

## now drop uneeded columns
bruvsdf <- bruvscatdf[,-(c(2:7))]
head(bruvsdf)

#check for NAs in your new data frame
any(is.na(bruvsdf)) # TRUE: There are NAs
# check which ones:
which(is.na(bruvsdf$Name)) # there are 33 NAs in our data, because some point fall outside the polygones that we have so far
# we are going to deal with this later.
### fix problem: add missing polygons ####
plot(gbregion)
points(bruvscat)

### make spatial points of new data frame ####
bruvssp <- bruvsdf
coordinates(bruvssp) <- ~ coords.x1 + coords.x2
plot(gbregion)
points(bruvssp, col = "blue")

# Make categories factors so you can plot them 
bruvsdf$IUCNCategory <- as.factor(bruvsdf$IUCNCategory)
bruvsdf$Name <- as.factor(bruvsdf$Name)

# Plot them 
########## Notice that some points do not fall in any polygon, we should fix this at some point ##
# Plot by IUCN category
plot(geo, add= T)
points(bruvssp, col = "blue", pch = 21, bg = bruvsdf$IUCNCategory, add = T)

# Plot by name of area (Ngary Capes, Geoph bay)
points(bruvssp, col = "black", pch = 22, bg = bruvsdf$Name, add = T)

##### save bruvs data frame and spatial points #####
write.csv(bruvsdf, "G:/My Drive/Analysis_Hannah_Geographe/Data/BRUVs2014df.csv")
writeOGR(bruvssp, "G:/My Drive/Analysis_Hannah_Geographe/Data", "BRUVs2014sp", drive = "ESRI Shapefile")


############ Match images to percent cover data ###############

### load bruvs 2014 percent cover data ####
pc <- read.csv("G:/My Drive/Analysis_Hannah_Geographe/Data/stereo-BRUVs_habitat.percent.cover.csv")
# check the df
str(pc)

## you want to merge the pc data frame with the bruvsdf data frame by "sample" name or number
# just make sure the "sample" columns in the df have the same names
names(pc) # sample
names(bruvsdf) # sample
 
## rename the column in bruvsdf
names(bruvsdf)[names(bruvsdf)=="Sample"] <- "sample"
names(bruvsdf)

bruvs2014data <- merge(pc, bruvsdf, by.x = "sample", all = T)
head(bruvs2014data)

##### save this new df ####
write.csv(bruvs2014data, "G:/My Drive/Analysis_Hannah_Geographe/Data/Bruvs2014_Pc_zone.csv")



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



####################### FROM HERE DOWN NOT DONE ########################


bruvscatdf$Sample<- as.character(bruvscatdf$Sample)
bruvscatdf$ORIG_NAME.1 <- as.character(bruvscatdf$ORIG_NAME.1)
bruvscatdf$ORIG_NAME.2 <- as.character(bruvscatdf$ORIG_NAME.2)
bruvscatdf$ORIG_NAME <- as.character(bruvscatdf$ORIG_NAME)
bruvscatdf$IUCN_CAT.1 <- as.character(bruvscatdf$IUCN_CAT.1)
bruvscatdf$IUCN_CAT.2 <- as.character(bruvscatdf$IUCN_CAT.2)
bruvscatdf$IUCN_CAT <- as.character(bruvscatdf$IUCN_CAT)
str(bruvscatdf)

# now coalesce Names and IUCN Categories
bruvscatdf <- bruvscatdf %>% mutate(Name = coalesce(bruvscatdf$ORIG_NAME.1,bruvscatdf$ORIG_NAME.2,bruvscatdf$ORIG_NAME),
                                    IUCNCategory = coalesce(bruvscatdf$IUCN_CAT.1,bruvscatdf$IUCN_CAT.2,bruvscatdf$IUCN_CAT)) 

# check it
head(bruvscatdf)

## now drop uneeded columns
bruvsdf <- bruvscatdf[,-(c(2:7))]
head(bruvsdf)

#check for NAs in your new data frame
any(is.na(bruvsdf)) # TRUE: There are NAs
# check which ones:
which(is.na(bruvsdf$Name)) # there are 33 NAs in our data, because some point fall outside the polygones that we have so far
# we are going to deal with this later.
### fix problem: add missing polygons ####
plot(gbregion)
points(bruvscat)

### make spatial points of new data frame ####
bruvssp <- bruvsdf
coordinates(bruvssp) <- ~ coords.x1 + coords.x2
plot(gbregion)
points(bruvssp, col = "blue")

# Make categories factors so you can plot them 
bruvsdf$IUCNCategory <- as.factor(bruvsdf$IUCNCategory)
bruvsdf$Name <- as.factor(bruvsdf$Name)

# Plot them 
########## Notice that some points do not fall in any polygon, we should fix this at some point ##
# Plot by IUCN category
plot(geo, add= T)
points(bruvssp, col = "blue", pch = 21, bg = bruvsdf$IUCNCategory, add = T)

# Plot by name of area (Ngary Capes, Geoph bay)
points(bruvssp, col = "black", pch = 22, bg = bruvsdf$Name, add = T)

##### save bruvs data frame and spatial points #####
write.csv(bruvsdf, "G:/My Drive/Analysis_Hannah_Geographe/Data/BRUVs2014df.csv")
writeOGR(bruvssp, "G:/My Drive/Analysis_Hannah_Geographe/Data", "BRUVs2014sp", drive = "ESRI Shapefile")


############ Match images to percent cover data ###############

### load bruvs 2014 percent cover data ####
pc <- read.csv("G:/My Drive/Analysis_Hannah_Geographe/Data/stereo-BRUVs_habitat.percent.cover.csv")
# check the df
str(pc)

## you want to merge the pc data frame with the bruvsdf data frame by "sample" name or number
# just make sure the "sample" columns in the df have the same names
names(pc) # sample
names(bruvsdf) # sample

## rename the column in bruvsdf
names(bruvsdf)[names(bruvsdf)=="Sample"] <- "sample"
names(bruvsdf)

bruvs2014data <- merge(pc, bruvsdf, by.x = "sample", all = T)
head(bruvs2014data)

##### save this new df ####
write.csv(bruvs2014data, "G:/My Drive/Analysis_Hannah_Geographe/Data/Bruvs2014_Pc_zone.csv")

