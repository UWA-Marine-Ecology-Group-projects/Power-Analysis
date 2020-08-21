### Create artificial sites ###

## libraries ----
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
library(sp)
library(raster)
library(rgdal)
library(pdist)
library(fields)
library(rgeos)


# clear environment ---
rm(list=ls()) #clear memory

# Study name----

study <- "stereo-BRUVs"

# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
m.dir <- "Y:/Power-Analysis"
d.dir <- paste(w.dir,"data",sep="/")
p.dir <- paste(w.dir,"plots",sep="/")
s.dir <- "G:/My Drive/Anita/Shapefiles" 



# CREATE ARTIFICAL SITES ----

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)

# read GB poly in utm ----
gbu <- readOGR('G:/My Drive/Anita/Shapefiles/GeoBay_CMR_UTM.shp')
gbu$ZoneName
gbu$Area_KM2 # NPZ = 15.27252 km2  -- HPZ = 21.07283 km2
plot(gbu)

crsp <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


## Read Bruv locations ----


# Method 1 : hierarchical clustering approach----
# https://gis.stackexchange.com/questions/17638/clustering-spatial-data-in-r

library(geosphere)

# Read Bruv locations ----

xyp <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/CMR-points-utm.shp")
xy <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/CMR-points.shp")


## Cluster by zone ----

plot(gb)
plot(xy, pch=20, add=T)
xyzones <- raster::extract(gb, xy)
xy.df <- as.data.frame(xy)

xyzones <- cbind(xy.df, xyzones)
head(xyzones)

coordinates(xyzones) <- ~coords.x1+coords.x2
xyzones
proj4string(xyzones) <- proj4string(gb)

# MUZ clusters ----
muz <- xyzones[xyzones$ZoneName=="Multiple Use Zone",]
muz # 3486 features

# SPZ clusters ----
spz <- xyzones[xyzones$ZoneName=="Special Purpose Zone (Mining Exclusion)",]
spz # 4415 features

bigzones <- raster::union(muz, spz)
plot(bigzones, col='red', add =T)



# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(bigzones)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="centroid")

# define the distance threshold, in this case 40 m
d <- 3000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) use k to define number of clusters

### UP TO HERE !! ####
bigzones$clust <- cutree(hc, h=d)

bigzones

plot(gb)
plot(bigzones, col = bigzones$clust, add=T) # 7 clusters
# join clusters 2, 6 and 7
plot(muz, col = muz$clust=="2", add=T)
plot(muz, col = muz$clust=="6", add=T)
plot(muz, col = muz$clust=="7", add=T) 

str(muz)
muz$clust <- as.factor(muz$clust)
levels(muz$clust)[levels(muz$clust)=="7"] <- "2"
levels(muz$clust)[levels(muz$clust)=="6"] <- "2"


# SPZ clusters ----
spz <- xyzones[xyzones$ZoneName=="Special Purpose Zone (Mining Exclusion)",]
spz # 4415 features

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(spz)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

# define the distance threshold, in this case 40 m
d <- 8000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) #use k to define number of clusters
spz$clust <- cutree(hc, h=d) #use k to define number of clusters

spz
str(spz)
spz$clust <- as.factor(spz$clust)
levels(spz$clust) # 8 clusters

plot(gb)
plot(spz, col = spz$clust, add=T) # 8 clusters


# NPZ clusters ----
npz <- xyzones[xyzones$ZoneName=="National Park Zone",]
npz # 340 features

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(npz)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

# define the distance threshold, in this case 40 m
d <- 1000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) #use k to define number of clusters
npz$clust <- cutree(hc, h=d) #use k to define number of clusters

npz
str(npz)
npz$clust <- as.factor(npz$clust)
levels(npz$clust) # 8 clusters

plot(gb)
plot(npz, col = npz$clust, add=T) # 8 clusters

str(npz)


# join clusters 1,3,5 and 8 = 1 and then 2,4,6,7
plot(npz, col = npz$clust=="1", add=T)#
plot(npz, col = npz$clust=="2", add=T)
plot(npz, col = npz$clust=="3", add=T)#
plot(npz, col = npz$clust=="4", add=T)
plot(npz, col = npz$clust=="5", add=T)#
plot(npz, col = npz$clust=="6", add=T)
plot(npz, col = npz$clust=="7", add=T)
plot(npz, col = npz$clust=="8", add=T)#

str(npz)
npz$clust <- as.factor(npz$clust)
levels(npz$clust)[levels(npz$clust)=="3"] <- "1"
levels(npz$clust)[levels(npz$clust)=="5"] <- "1"
levels(npz$clust)[levels(npz$clust)=="8"] <- "1"

levels(npz$clust)[levels(npz$clust)=="4"] <- "2"
levels(npz$clust)[levels(npz$clust)=="6"] <- "2"
levels(npz$clust)[levels(npz$clust)=="7"] <- "2"


plot(gb)
plot(npz, col = npz$clust, add=T) # 2 clusters


# HPZ clusters ----
hpz <- xyzones[xyzones$ZoneName=="Habitat Protection Zone",]
hpz # 340 features

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(hpz)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

# define the distance threshold, in this case 40 m
d <- 2000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) #use k to define number of clusters
hpz$clust <- cutree(hc, h=d) #use k to define number of clusters

hpz
str(hpz)
hpz$clust <- as.factor(hpz$clust)
levels(hpz$clust) # 8 clusters

plot(gb)
plot(hpz, col = hpz$clust, add=T) # 2 clusters

str(hpz)


# join clusters 1 and 4 = 1 and then 2,3, and 5 = 2
plot(hpz, col = hpz$clust=="1", add=T)#
plot(hpz, col = hpz$clust=="2", add=T)
plot(hpz, col = hpz$clust=="3", add=T)
plot(hpz, col = hpz$clust=="4", add=T)#
plot(hpz, col = hpz$clust=="5", add=T)



str(hpz)
hpz$clust <- as.factor(hpz$clust)
levels(hpz$clust)[levels(hpz$clust)=="4"] <- "1"


levels(hpz$clust)[levels(hpz$clust)=="3"] <- "2"
levels(hpz$clust)[levels(hpz$clust)=="5"] <- "2"



plot(gb)
plot(hpz, col = hpz$clust, add=T) # 2 clusters


### rename clusters for unique IDs -----
levels(muz$clust)
levels(muz$clust)[levels(muz$clust)=="1"] <- "9"
levels(muz$clust)[levels(muz$clust)=="2"] <- "10"
levels(muz$clust)[levels(muz$clust)=="3"] <- "11"
levels(muz$clust)[levels(muz$clust)=="4"] <- "12"
levels(muz$clust)[levels(muz$clust)=="5"] <- "13"

levels(npz$clust)
levels(npz$clust)[levels(npz$clust)=="1"] <- "14"
levels(npz$clust)[levels(npz$clust)=="2"] <- "15"

levels(hpz$clust)
levels(hpz$clust)[levels(hpz$clust)=="1"] <- "16"
levels(hpz$clust)[levels(hpz$clust)=="2"] <- "17"


## Join all clustered points ----
clusteredpoints <- union(spz,muz)
clusteredpoints <- union(clusteredpoints,npz)
clusteredpoints <- union(clusteredpoints,hpz)
clusteredpoints$Seagrss # check you still have sg data

plot(gb)
plot(clusteredpoints, col = clusteredpoints$clust, pch=20, add=T)

#writeOGR(clusteredpoints, "Y:/Power-Analysis/Bimodal/Data/spatial", "BRUV_clusteredpoints", driver = "ESRI Shapefile", overwrite=T)

clusteredpoints <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/BRUV_clusteredpoints.shp")

clusteredp.df <- as.data.frame(clusteredpoints)

#write.csv(clusteredp.df, paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/'))








