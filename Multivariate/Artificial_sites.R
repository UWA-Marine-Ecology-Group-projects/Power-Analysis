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
hc <- hclust(as.dist(mdist), method="average")

# define the distance threshold, in this case 40 m
d <- 3100

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) use k to define number of clusters

### UP TO HERE !! ####
bigzones$clust <- cutree(hc, h=d)

bigzones

plot(gb)
plot(bigzones, col = bigzones$clust, add=T) 
bigzones$clust <- as.factor(bigzones$clust)
levels(bigzones$clust) # 51 clusters

# ID the clusters you need for NPZ and HPZ
plot(gb)
#plot(bigzones[,levels(bigzones$clust == "33")],  add=T)
plot(bigzones, col = bigzones$clust=="38",  add=T) 
plot(bigzones, col = bigzones$clust, pch=levels(bigzones$clust), add=T) 

# NPZ : 3, 4, 10, 14, 25, 27, 31, 11
# HPZ : 5, 6, 8, 12, 9

# Clusters: 3, 4, 5 + 12, 6 +9 + 15, 10, 25 +34, 31+36, 27+38

# Check  clusters
plot(gb)
plot(bigzones, col = bigzones$clust=="3", add=T)
plot(bigzones, col = bigzones$clust=="4", add=T)
plot(bigzones, col = bigzones$clust=="5", add=T) 
plot(bigzones, col = bigzones$clust=="6", add=T)
plot(bigzones, col = bigzones$clust=="8", add=T)
plot(bigzones, col = bigzones$clust=="10", add=T)
plot(bigzones, col = bigzones$clust=="12", add=T)
plot(bigzones, col = bigzones$clust=="14", add=T)
plot(bigzones, col = bigzones$clust=="25", add=T)
plot(bigzones, col = bigzones$clust=="27", add=T)
plot(bigzones, col = bigzones$clust=="31", add=T)

str(bigzones)

# Join clusters
# Clusters: 5 + 12, 6 +9 + 15, 25 +34, 31+36, 27+38
levels(bigzones$clust)[levels(bigzones$clust)=="12"] <- "5"
levels(bigzones$clust)[levels(bigzones$clust)=="9"] <- "6"
levels(bigzones$clust)[levels(bigzones$clust)=="15"] <- "6"
levels(bigzones$clust)[levels(bigzones$clust)=="34"] <- "25"
levels(bigzones$clust)[levels(bigzones$clust)=="36"] <- "31"
levels(bigzones$clust)[levels(bigzones$clust)=="38"] <- "27"

# check
plot(gb)
plot(bigzones, col = bigzones$clust, add=T)

# Clusters: 3, 4, 5 = 5 + 12, 6 = 6 +9 + 15, 10, 25 = 25 +34, 31 = 31+36, 27 = 27+38

# Make a different spatial points objects just with wanted clusters --
newc <- bigzones[bigzones$clust=="5",]
plot(newc, add=T)

newc <- union(newc, bigzones[bigzones$clust=="3",])
newc <- union(newc, bigzones[bigzones$clust=="4",])
newc <- union(newc, bigzones[bigzones$clust=="6",])
newc <- union(newc, bigzones[bigzones$clust=="8",])
newc <- union(newc, bigzones[bigzones$clust=="10",])
newc <- union(newc, bigzones[bigzones$clust=="25",])
newc <- union(newc, bigzones[bigzones$clust=="31",])
newc <- union(newc, bigzones[bigzones$clust=="27",])

plot(gb)
plot(newc, col=newc$clust, add=T)


# NPZ cluster ----
npz <- xyzones[xyzones$ZoneName=="National Park Zone",]
npz # 4415 features
plot(npz, add=T)
npz$clust <- "1"

# HPZ cluster ----
hpz <- xyzones[xyzones$ZoneName=="Habitat Protection Zone",]
hpz # 4415 features
plot(hpz, add=T)
hpz$clust <- "2"



## Join all clustered points ----
coastclust <- union(npz, hpz)
coastclust <- union(coastclust,newc)

plot(gb)
plot(coastclust, col = coastclust$clust, pch=20, add=T)

# save new clusters
#writeOGR(coastclust, "Y:/Power-Analysis/Multivariate/spatial", "BRUV_coastalclusters", driver = "ESRI Shapefile", overwrite=T)

# save df of new clusters
clusteredp.df <- as.data.frame(coastclust)
#write.csv(clusteredp.df, paste(d.dir, "BRUV_coastalclusters.csv", sep ='/'))


# read coastal clusters --
cc <- readOGR("Y:/Power-Analysis/Multivariate/spatial/BRUV_coastalclusters.shp")










