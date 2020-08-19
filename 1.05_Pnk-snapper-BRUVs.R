

#####  Raw point data into Binary: presence - absence of seagrass ###

# libraries ----
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
library(sp)
library(raster)
library(rgdal)
#install.packages("remotes")
#remotes::install_github("UWAMEGFisheries/GlobalArchive")
#library(GlobalArchive)

rm(list=ls()) #clear memory

# Study name----

study <- "stereo-BRUVs"

# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
raw.dir = paste(w.dir,"Data/raw",sep="/")
tidy.dir = paste(w.dir,"Data/tidy",sep="/")

dir(raw.dir)

# Set File to use ----
f <- "2014-12_Geographe.Bay_stereoBRUVs.pink.snapper.maxn.csv"

# set species name
s <- "pink-snapper"

#### Raw to PA ----

## read data --
# these files already have lat and long

fish <- read.csv(paste(raw.dir, f, sep ='/')) %>%
  dplyr::select(sample,maxn,latitude, longitude, depth) %>% # Select columns to keep
  glimpse()

str(fish) # 322 obs
head(fish)

# test for NAs --
any(is.na(fish)) 





#### Match to CMR zones ---- 

# read GB shapefile --
s.dir <- "G:/My Drive/Anita/Shapefiles"

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)
gb



# read already matched data (king wrasse)--
kw <- read.csv(paste(tidy.dir, paste(study, paste("king-wrasse", "CMR", 'csv', sep='.'), sep='_'), sep='/'))
head(kw)
str(kw) # 243 obs

str(fish)

df <- merge(fish, kw[,c(2,7)], by="sample")
head(df)
str(df) # 243 obs

# check it --
dfs <- df
coordinates(dfs) <- ~longitude+latitude 
points(dfs, col=dfs$ZoneName)


# Save CMR data ----
write.csv(df, paste(tidy.dir, paste(study, paste(s, "CMR", 'csv', sep='.'), sep='_'), sep='/'))
writeOGR(dfs, dsn = "Y:/Power-Analysis/Data/shapefiles", layer = paste(study, s, sep='_'), driver = "ESRI Shapefile", overwrite = T)






#####  CLUSTER BRUVS  #####

# Method 1 : hierarchical clustering approach----
# https://gis.stackexchange.com/questions/17638/clustering-spatial-data-in-r

library(geosphere)

# convert data to a SpatialPointsDataFrame object
#xy <- SpatialPointsDataFrame(
 # matrix(c(df$Longitude,df$Latitude), ncol=2), data.frame(ID=seq(1:length(df$Longitude))),
  #proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))

#writeOGR(xy, "Y:/Power-Analysis/Bimodal/Data/spatial", "CMR-points", driver = "ESRI Shapefile")
paste(study, s, sep='_')

xyp <- readOGR("Y:/Power-Analysis/Data/shapefiles/stereo-BRUVs_pink-snapper.shp")

plot(xyp)


#### FROM HERE : THIS I DID WITH KING WRASSE AND JUST COPIED CLUSTERS TO THE REST ####
## Cluster by zone ----

plot(gb)
plot(xyp, add=T)
xyzones <- raster::extract(gb, xyp)
xy.df <- as.data.frame(xyp)

xyzones <- cbind(xy.df, xyzones)
head(xyzones)

coordinates(xyzones) <- ~coords.x1+coords.x2
xyzones
proj4string(xyzones) <- proj4string(gb)

# MUZ clusters ----
muz <- xyzones[xyzones$ZoneName=="Multiple Use Zone",]
muz # 3486 features

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(muz)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

# define the distance threshold, in this case 40 m
d <- 6000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
#muz$clust <- cutree(hc, k=15, h=d) use k to define number of clusters
muz$clust <- cutree(hc, h=d)

muz

plot(gb)
plot(muz, col = muz$clust, add=T) # 6 clusters
levels(muz$clust)
# join clusters 2 and 6 --
# check which ones they are
plot(muz, col = muz$clust=="2", add=T)
plot(muz, col = muz$clust=="6", add=T)
# Join them
str(muz)
muz$clust <- as.factor(muz$clust)
levels(muz$clust)[levels(muz$clust)=="6"] <- "2"

plot(muz, col = muz$clust, add=T) # 5 clusters


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


#### : TO HERE ####

### Read shapefile and df already clustered (king wrasse)----

kws <- readOGR("Y:/Power-Analysis/Data/shapefiles/BRUV_clusteredpoints.shp")
plot(gb)
kws$clust
points(kws, col=kws$clust)

kw <- read.csv(paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/'))
str(kw)
names(kw)

# merge with pink snapper --
str(df)
df2 <- merge(df, kw[,c(2,17)], by ='sample')
head(df2)
str(df2) # 243 obs

# check in plot --
df2s <- df2
coordinates(df2s) <- ~longitude+latitude

plot(gb)
points(df2s, col=df2s$clust)


### Save shapefile and df ----

write.csv(df2, paste(tidy.dir, paste(s, "BRUV_clusteredpoints.csv", sep='-'), sep ='/'))

writeOGR(df2s, "Y:/Power-Analysis/Data/shapefiles", paste(s, "BRUV_clusteredpoints", sep='-'), driver = "ESRI Shapefile", overwrite=T)





### ---- ###

# Method 2: MBH ----

############ I DID NOT END UP USING THIS METHOD --------------------

# Using clusters planned for GB transects as centres of clusters --

# but first I need to reduce the amount of clusters, because they are too many

library(pdist)
library(fields)
library(rgeos)

# read GB poly in utm ----
gbu <- readOGR('G:/My Drive/Anita/Shapefiles/GeoBay_CMR_UTM.shp')
gbu

crsp <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

## High res - MUZ ----

hmuz <- readOGR("G:/My Drive/meg_projects/Project_Parks_Geo_fish and habitat/Sampling design/GB_Design/Multib_Lidar_Design/coords_MUZ/coords_MUZ_2020-05-18.shp")
plot(gb)
plot(hmuz, add=T)

hmuzp <- spTransform(hmuz, crsp)

hmuz.mat <- gWithinDistance(hmuzp, dist = 3000, byid = TRUE)
diag(hmuz.mat) <- NA
hmuz.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

hmuz.mat[lower.tri(hmuz.mat, diag=TRUE)] <- NA
hmuz.mat

colSums(hmuz.mat, na.rm=TRUE) == 0
v1 <- colSums(hmuz.mat, na.rm=TRUE) == 0
hmuz[v1, ] # 5 features left

# plot --
plot(gbu)
plot(hmuzp[v1, ], pch=20, col="red", add=T)

plot(gb)
plot(hmuz[v1,], add=T)

# Save cluster centroids --
hmuz.cluters <- hmuz[v1, ]
hmuz.clus.df <- as.data.frame(hmuz.cluters)


## High res - SPZ----

transects <- readOGR("G:/My Drive/meg_projects/Project_Parks_Geo_fish and habitat/Sampling design/GB_Design/Multib_Lidar_Design/coords_SPZ/coords_spz_2020-05-18.shp")
plot(gb)
plot(transects, add=T)

transectsp <- spTransform(transects, crsp)

transects.mat <- gWithinDistance(transectsp, dist = 3000, byid = TRUE)
diag(transects.mat) <- NA
transects.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

transects.mat[lower.tri(transects.mat, diag=TRUE)] <- NA
transects.mat

colSums(transects.mat, na.rm=TRUE) == 0
v1 <- colSums(transects.mat, na.rm=TRUE) == 0
transects[v1, ] # 5 features left

# plot --
plot(gbu)
plot(transectsp[v1, ], pch=20, col="red", add=T)

plot(gb)
plot(transects[v1,], add=T)

# Save cluster centroids --
hspz.cluters <- transects[v1, ]
hspz.clus.df <- as.data.frame(hspz.cluters)


## High res - NPZ and HPZ ----

#transects <- readOGR("G:/My Drive/meg_projects/Project_Parks_Geo_fish and habitat/Sampling design/GB_Design/Multib_Lidar_Design/coords_SPZ/coords_spz_2020-05-18.shp")

t <- read.csv("G:/My Drive/meg_projects/Project_Parks_Geo_fish and habitat/Sampling design/GB_Design/Multib_Lidar_Design/coordinates_hires_NPZ_HPZ.csv")
str(t)

transectsp <- t

coordinates(transectsp) <- ~ start_x + start_y

proj4string(transectsp) <- proj4string(gbu)
#plot(gb)
#plot(transects, add=T)

#transectsp <- spTransform(transects, crsp)

transects.mat <- gWithinDistance(transectsp, dist = 2000, byid = TRUE)
diag(transects.mat) <- NA
transects.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

transects.mat[lower.tri(transects.mat, diag=TRUE)] <- NA
transects.mat

colSums(transects.mat, na.rm=TRUE) == 0
v1 <- colSums(transects.mat, na.rm=TRUE) == 0
transects[v1, ] # 5 features left

# plot --
plot(gbu)
plot(transectsp[v1, ], pch=20, col="red", add=T)

#plot(gb)
#plot(transects[v1,], add=T)

# Save cluster centroids --
hspz.cluters <- transectsp[v1, ]

gb
crsgb <- "+proj=longlat +ellps=GRS80 +no_defs"

hnpz.hpz.cluters2 <- spTransform(hspz.cluters, crsgb)


hnpz.hpz.clus.df <- as.data.frame(hnpz.hpz.cluters2)


## Low res - SPZ----

transects <- readOGR("G:/My Drive/meg_projects/Project_Parks_Geo_fish and habitat/Sampling design/GB_Design/Bathy250m_design/rstudio-export (1)/Design4_notClustTransects_2020-05-17.shp")
plot(gbu)
plot(transects, add=T)

transectsp <- transects

#transectsp <- spTransform(transects, crsp)

transects.mat <- gWithinDistance(transectsp, dist = 4000, byid = TRUE)
diag(transects.mat) <- NA
transects.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

transects.mat[lower.tri(transects.mat, diag=TRUE)] <- NA
transects.mat

colSums(transects.mat, na.rm=TRUE) == 0
v1 <- colSums(transects.mat, na.rm=TRUE) == 0
transects[v1, ] # 5 features left

# plot --
plot(gbu)
plot(transectsp[v1, ], pch=20, col="red", add=T)

#plot(gb)
#plot(transects[v1,], add=T)

# Save cluster centroids --
transects2 <- transects[v1, ]
transects3 <- spTransform(transects2, crsgb)

plot(gb)
plot(transects3, add=T)

bz.cluters <- transects3
bz.clus.df <- as.data.frame(bz.cluters)

#### Join all the cluster centroids ----
head(bz.clus.df)
bz <- bz.clus.df[, c(10,11)]
bz$cluster <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
bz$bathy.res <- "Low"
names(bz) <- c("long", "lat", "cluster",   "bathy.res")

head(hmuz.clus.df)
hmuz <- hmuz.clus.df[,c(7,8)]
hmuz$cluster <- c(1,2,3,4,5)
hmuz$bathy.res <- "High"
names(hmuz) <- c("long", "lat", "cluster",   "bathy.res")

head(hspz.clus.df)
hspz <- hspz.clus.df[,c(7,8)]
hspz$cluster <- c(1,2,3,4,5)
hspz$bathy.res <- "High"
names(hspz) <- c("long", "lat", "cluster",   "bathy.res")

head(hnpz.hpz.clus.df)
hnhpz <- hnpz.hpz.clus.df[,c(12,13)]
hnhpz$cluster <- c(1,2,3,4)
hnhpz$bathy.res <- "High"
names(hnhpz) <- c("long", "lat", "cluster",   "bathy.res")

# joing all clusters
gbclusters <- rbind(bz, hmuz, hspz, hnhpz)
gbclusters

c <- gbclusters

coordinates(c) <- ~long+lat
plot(gb)
plot(c, add=T)
proj4string(c) <- proj4string(gb)

c2 <- spTransform(c, crsp)

## some around the HPZ deep zone are really close.. run another buffer
c.mat <- gWithinDistance(c2, dist = 2000, byid = TRUE)
diag(c.mat) <- NA
c.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

c.mat[lower.tri(c.mat, diag=TRUE)] <- NA
c.mat

colSums(c.mat, na.rm=TRUE) == 0
v1 <- colSums(c.mat, na.rm=TRUE) == 0
c2[v1, ] # 5 features left

# plot --
plot(gbu)
plot(c2[v1, ], pch=20, col="red", add=T)

plot(gb)
plot(c[v1, ], pch=20, col="red", add=T)

gbclust <- c[v1, ]

writeOGR(gbclust, "Y:/Power-Analysis/Bimodal/Data/spatial", "BRUV_MBHclusters", driver = "ESRI Shapefile")

gbclust.df <- as.data.frame(gbclust)
write.csv(gbclust.df, paste(tidy.dir, "BRUV_MBHclusters.csv", sep ='/'))
gbclust.df <- read.csv(paste(tidy.dir, "BRUV_MBHclusters.csv", sep ='/'))

####
gbclust.df$clusterID <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)

df <- gbclust.df

coordinates(df) <- ~ long + lat
proj4string(df) <- proj4string(gb)

gbclustp <- spTransform(df, crsp)

#cids <- gbclustp$clusterID

## Create 2000m buffer ----

buff <- gBuffer(gbclustp, byid=T, width = 2000)

plot(gbu)
plot(buff, add=T)

writeOGR(buff, "Y:/Power-Analysis/Bimodal/Data/spatial", "BRUV_MBHclusters_2000mbuffer", driver = "ESRI Shapefile")

buff <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/BRUV_MBHclusters_2000mbuffer.shp")

## extract points within buffers ----


xyp <- spTransform(xy, crsp)

buff2 <- spTransform(buff, proj4string(gb))

clustpoints <- raster::extract(buff2, xy, df=T)
head(clustpoints)
toremove <- clustpoints$point.ID[duplicated(clustpoints$point.ID)] # check for replicates
# remove reps
clustpoints <- clustpoints[-toremove,]

head(clustpoints)
# bind to df
test <- cbind(clustpoints, as.data.frame(xy))

# remove nas from culsterIDs
# Remove NA's -  in the zone column --
#df <- na.omit(df)
head(test)
test <- test %>% drop_na(clusterID)
str(test)# 3342 obs
test$clusterID <- as.factor(test$clusterID)
head(test)
levels(test$clusterID)

plot(gb)
points(xy)
#points(test$coords.x1, test$coords.x2, col = "red", cex=1)
points(test$coords.x1, test$coords.x2, col = test$clusterID, cex=1)
plot(buff2, add=T)


## Create buffer according to zone ----

plot(gbu)
plot(gbclustp, add=T)

gbclustp.df <- as.data.frame(gbclustp)

gbclustp2 <- raster::extract(gbu, gbclustp, sp = T)

gbclustp2 <- cbind(gbclustp.df, gbclustp2)

clustgb <- gbclustp2

coordinates(clustgb) <- ~long + lat

plot(gbu)
plot(clustgb, col = clustgb$ZoneName, add=T)

# run another buffer for MUZ and SPZ so they include more BRUVs --
# 4000 m buffer --

muzone <- clustgb[clustgb$ZoneName==("Multiple Use Zone"), ]
spzone <- clustgb[clustgb$ZoneName==("Special Purpose Zone (Mining Exclusion)"), ]
plot(muzone)
plot(spzone)
bigzones <- union(muzone, spzone)
plot(bigzones)

m.mat <- gWithinDistance(bigzones, dist = 5000, byid = TRUE)
diag(m.mat) <- NA
m.mat

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

m.mat[lower.tri(m.mat, diag=TRUE)] <- NA
m.mat

colSums(m.mat, na.rm=TRUE) == 0
v1 <- colSums(m.mat, na.rm=TRUE) == 0
bigzones[v1, ] # 16 features left

# plot --
plot(gbu)
plot(bigzones[v1, ], pch=20, col="red", add=T)

#plot(gb)
#plot(muzone[v1, ], pch=20, col="red", add=T)

bigzones.c <- bigzones[v1, ]


buff.bigz <- gBuffer(bigzones.c, byid=T, width = 4000)

plot(gbu)
plot(buff.bigz, add=T)
plot(xyp, add=T, col = "blue", pch = 20, cex = 1)

writeOGR(buff.bigz, "Y:/Power-Analysis/Bimodal/Data/spatial", "BRUV_MBHclusters_4000mbuffer", driver = "ESRI Shapefile")

buff.bigz <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/BRUV_MBHclusters_4000mbuffer.shp")



### --- ### --- ### ---

## Method 1 : Get data ready for epower ----

### NPZ ####

c.points <- readOGR("Y:/Power-Analysis/Data/shapefiles/pink-snapper-BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "pink-snapper-BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df %>% dplyr::select(sample, maxn, ZoneName, clust) %>%
  glimpse()
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneNam)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
dfn
str(dfn)
names(dfn)
head(dfn)
summary(dfn)

dfn <- droplevels(dfn)

levels(dfn$ZoneNam)


#### NOT GOING TO SUBSAMPLE BRUV DATA --


dfn1 <- dfn
names(dfn1)
head(dfn1)
str(dfn1) #126 obs

## BRUVS as samples ----

# T1 --
f1 <- dfn1
f1$Time <- "T1"
# T2 --
f2 <- dfn1
f2$Time <- "T2"
# T3 -- 
f3 <- dfn1
f3$Time <- "T3"

# join
fall <- rbind(f1, f2, f3)
head(fall)
names(fall) <- c("sample", "maxn", "ZoneName", "Cluster", "Time")

fall$Period <- "Before"

# Make control impact column
levels(fall$ZoneName)
fall$CvI <- ifelse(fall$ZoneName=="National Park Zone", "Impact", "Control")
head(fall)

#### Save data for epower ----

write.csv(fall, paste(tidy.dir, paste(study, s, "NPZ-All-epower.csv", sep='-'), sep='/'))


## Mean maxn by cluster ----

# T1 --

f1 <- aggregate(maxn ~ clust + ZoneName, data = dfn1, sum)
f1
names(f1) <- c("Cluster", "ZoneName", "maxn_sum")

no.scored1 <-  aggregate(sample ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.samples")

fm <- aggregate(maxn ~ clust +ZoneName, data = dfn1, mean)
fm
names(fm) <- c("Cluster", "ZoneName", "maxn_mean")


df1 <- cbind(f1, no.samples= no.scored1[,3], maxn_mean=fm[,3])
df1
#names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

# T2 --

df2 <- df1
df2$Time <- "T2"
df2

# T3 --

df3 <- df1
df3$Time <- "T3"
df3


## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="National Park Zone", "Impact", "Control")
head(dfall)

#### Save mean data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, s, "NPZ-epower.csv", sep='_'), sep='/'))





### HPZ ####

c.points <- readOGR("Y:/Power-Analysis/Data/shapefiles/pink-snapper-BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "pink-snapper-BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df %>% dplyr::select(sample, maxn, ZoneName, clust) %>%
  glimpse()
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneNam)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
dfn
str(dfn)
names(dfn)
head(dfn)
summary(dfn)

dfn <- droplevels(dfn)

levels(dfn$ZoneNam)


#### NOT GOING TO SUBSAMPLE BRUV DATA --


## BRUVS as samples ----

dfn1 <- dfn
names(dfn1)

# T1 --
f1 <- dfn1
f1$Time <- "T1"
# T2 --
f2 <- dfn1
f2$Time <- "T2"
# T3 -- 
f3 <- dfn1
f3$Time <- "T3"

# join
fall <- rbind(f1, f2, f3)
head(fall)
names(fall) <- c("sample", "maxn", "ZoneName", "Cluster", "Time")

fall$Period <- "Before"

# Make control impact column
levels(fall$ZoneName)
fall$CvI <- ifelse(fall$ZoneName=="Habitat Protection Zone", "Impact", "Control")
head(fall)

#### Save mean data for epower ----

write.csv(fall, paste(tidy.dir, paste(study, s, "HPZ-All-epower.csv", sep='-'), sep='/'))



## Mean maxn by cluster ----

# T1 --

head(dfn1)

f1 <- aggregate(maxn ~ clust + ZoneName, data = dfn1, sum)
f1
names(f1) <- c("Cluster", "ZoneName", "maxn_sum")

no.scored1 <-  aggregate(sample ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.samples")

fm <- aggregate(maxn ~ clust +ZoneName, data = dfn1, mean)
fm
names(fm) <- c("Cluster", "ZoneName", "maxn_mean")


df1 <- cbind(f1, no.samples= no.scored1[,3], maxn_mean=fm[,3])
df1
#names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

# T2 --

#dfn2 <- dfn
#head(dfn1)

df2 <- df1
df2$Time <- "T2"
df2

# T3 --

df3 <- df2
df3$Time <- "T3"


## joint these together --

dfall <- rbind(df1, df2, df3)
dfall

# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Habitat Protection Zone", "Impact", "Control")
head(dfall)

#### Save mean data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, s, "HPZ-epower.csv", sep='_'), sep='/'))



### MUZ ####

c.points <- readOGR("Y:/Power-Analysis/Data/shapefiles/pink-snapper-BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "pink-snapper-BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df %>% dplyr::select(sample, maxn, ZoneName, clust) %>%
  glimpse()
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneNam)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
dfn
str(dfn)
names(dfn)
head(dfn)
summary(dfn)

dfn <- droplevels(dfn)

levels(dfn$ZoneNam)


#### NOT GOING TO SUBSAMPLE BRUV DATA --

## BRUVS as samples ----

dfn1 <- dfn
names(dfn1)

# T1 --
f1 <- dfn1
f1$Time <- "T1"
# T2 --
f2 <- dfn1
f2$Time <- "T2"
# T3 -- 
f3 <- dfn1
f3$Time <- "T3"

# join
fall <- rbind(f1, f2, f3)
head(fall)
names(fall) <- c("sample", "maxn", "ZoneName", "Cluster", "Time")

fall$Period <- "Before"

# Make control impact column
levels(fall$ZoneName)
fall$CvI <- ifelse(fall$ZoneName=="Multiple Use Zone", "Impact", "Control")
head(fall)

#### Save  data for epower ----

write.csv(fall, paste(tidy.dir, paste(study, s, "MUZ-All-epower.csv", sep='-'), sep='/'))



## Mean maxn by cluster ----

dfn1 <- dfn
names(dfn1)

# T1 --

head(dfn1)

f1 <- aggregate(maxn ~ clust + ZoneName, data = dfn1, sum)
f1
names(f1) <- c("Cluster", "ZoneName", "maxn_sum")

no.scored1 <-  aggregate(sample ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.samples")

fm <- aggregate(maxn ~ clust +ZoneName, data = dfn1, mean)
fm
names(fm) <- c("Cluster", "ZoneName", "maxn_mean")


df1 <- cbind(f1, no.samples= no.scored1[,3], maxn_mean=fm[,3])
df1
#names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

# T2 --

df2 <- df1
df2$Time <- "T2"
df2

# T3 --

df3 <- df1
df3$Time <- "T3"
df3


## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Multiple Use Zone", "Impact", "Control")
head(dfall)
dfall

#### Save mean data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, s, "MUZ-epower.csv", sep='-'), sep='/'))




#### SPZ joined clusters #####

c.points <- readOGR("Y:/Power-Analysis/Data/shapefiles/pink-snapper-BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "pink-snapper-BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df %>% dplyr::select(sample, maxn, ZoneName, clust, coords.x1, coords.x2) %>%
  glimpse()
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneNam)
str(dfz)

# remove national park zone
dfn <-  dfz[-3]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
dfn
str(dfn)
names(dfn)
head(dfn)
summary(dfn)

dfn <- droplevels(dfn)

levels(dfn$ZoneNam)

# combine spz clusters ----
dfz <- split(dfn, dfn$ZoneNam)
str(dfz)
dfz$`Special Purpose Zone (Mining Exclusion)`
dfz1 <-  as.data.frame(dfz$`Special Purpose Zone (Mining Exclusion)`)
str(dfz1)

pal1 <- c("black", "grey", "blue", "green", "red", "yellow", "orange", "pink")

spz <- dfz1
coordinates(spz) <- ~longitude+latitude
spz$clust <- as.factor(spz$clust)

# PLOT SPZ CLUSTERS ---
# figure out where the upper RIGHT hand corner of your plot extent is
the_plot_extent <- extent(spz)

# grab the upper right hand corner coordinates
#furthest_pt_east <- the_plot_extent@xmax
#furthest_pt_north <- the_plot_extent@ymax

plot(spz, col=(pal1)[spz$clust])


legend(#x = furthest_pt_east, y = furthest_pt_north,
  "bottomleft",   # location of legend
       legend = levels(spz$clust), fill = pal1, ncol=8, cex =0.7) # categories or elements to render in



#### NOT GOING TO SUBSAMPLE BRUV DATA --


#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn1 <-as.data.frame(dfn %>% group_by(clust) %>% sample_n(size = 2500))
#str(dfn1)
#dfn1 <- droplevels(dfn1)
#str(dfn1) # 11250 obs

dfn1 <- dfn
str(dfn)
names(dfn1)
dfn1$clust <- as.factor(dfn1$clust)
levels(dfn1$clust)

## Group Clusters ----
#Cluster 1 = 1+4
#Cluster 2 = 2+6+8
#Cluster 3 = 3
#Cluster 5 = 5 + 7

# rename SPZ BRUV clusters
levels(dfn1$clust)[levels(dfn1$clust)=="4"] <- "1"
levels(dfn1$clust)[levels(dfn1$clust)=="6"] <- "2"
levels(dfn1$clust)[levels(dfn1$clust)=="8"] <- "2"
levels(dfn1$clust)[levels(dfn1$clust)=="7"] <- "5"

dfn <- droplevels(dfn1$clust)
levels(dfn1$clust)

head(dfn1)
names(dfn1)


## BRUVS as samples ----

#dfn1 <- dfn
names(dfn1)

# T1 --
f1 <- dfn1
f1$Time <- "T1"
# T2 --
f2 <- dfn1
f2$Time <- "T2"
# T3 -- 
f3 <- dfn1
f3$Time <- "T3"

# join
fall <- rbind(f1, f2, f3)
head(fall)
names(fall) <- c("X", "sample", "maxn", "latitude", "longitude", "depth", "ZoneName", "Cluster", "Time")

fall$Period <- "Before"

# Make control impact column
levels(fall$ZoneName)
fall$CvI <- ifelse(fall$ZoneName=="Special Purpose Zone (Mining Exclusion)", "Impact", "Control")
head(fall)

#### Save  data for epower ----

write.csv(fall, paste(tidy.dir, paste(study, s, "SPZ-All-epower.csv", sep='-'), sep='/'))



## Mean maxn by cluster ----

#dfn1 <- dfn
names(dfn1)
str(dfn1)

# T1 --

head(dfn1)

f1 <- aggregate(maxn ~ clust + ZoneName, data = dfn1, sum)
f1
names(f1) <- c("Cluster", "ZoneName", "maxn_sum")

no.scored1 <-  aggregate(sample ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.samples")

fm <- aggregate(maxn ~ clust +ZoneName, data = dfn1, mean)
fm
names(fm) <- c("Cluster", "ZoneName", "maxn_mean")


df1 <- cbind(f1, no.samples= no.scored1[,3], maxn_mean=fm[,3])
df1
#names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

# T2 --

df2 <- df1
df2$Time <- "T2"
df2

# T3 --

df3 <- df1
df3$Time <- "T3"
df3


## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Special Purpose Zone (Mining Exclusion)", "Impact", "Control")
head(dfall)
dfall

#### Save mean data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, s, "SPZ-epower.csv", sep='-'), sep='/'))







### SPZ not clustered #### HAVE NOT DONE THIS FOR FISH -----

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn2)
#dfn2 <- droplevels(dfn2)
#str(dfn2) # 11250 obs

dfn2 <- dfn

## calculate presences and no. scored ----

sg.pres2 <- aggregate(Seagrss ~ clust + ZoneNam, data = dfn2, sum)
sg.pres2

no.scored2 <-  aggregate(Seagrss ~ clust + ZoneNam, data = dfn2, length)           
no.scored2
names(no.scored2) <- c("Transect", "ZoneName", "no.scored")

df2<- cbind(sg.pres2, no.scored2[,3])
df2
names(df2) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn3)
#dfn3 <- droplevels(dfn3)
#str(dfn3) # 11250 obs

dfn3 <- dfn

## calculate presences and no. scored ----

sg.pres3 <- aggregate(Seagrss ~ clust + ZoneNam, data = dfn3, sum)
sg.pres3

no.scored3 <-  aggregate(Seagrss ~ clust + ZoneNam, data = dfn3, length)           
no.scored3
names(no.scored3) <- c("Transect", "ZoneName", "no.scored")

df3<- cbind(sg.pres3, no.scored3[,3])
df3
names(df3) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df3$Time <- "T3"
df3

## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Special Purpose Zone (Mining Exclusion)", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "SPZ-seag-epower.csv", sep='-'), sep='/'))




