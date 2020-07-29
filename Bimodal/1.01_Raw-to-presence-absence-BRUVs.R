

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


#### Raw to PA ----

## read data --
habitat <- read.csv(paste(raw.dir, "stereo-BRUVS_PointTags.csv", sep ='/')) %>%
  dplyr::select(Image.Name,Image.Source.Dataset,Point.X..from.top.left.corner., Point.Y..from.top.left.corner.,Display.Name, Display.Code) %>% # Select columns to keep
  mutate(Image.Name=str_replace_all(Image.Name,c("https://uwa-bruvs.s3-ap-southeast-2.amazonaws.com/"="",".jpg"="","/images"="",
                                                 "/2014_BRUV_Forward"="","/2014_BRUV_Rear"=""))) %>% # Remove url from sample names
  dplyr::rename(habitat = Display.Name, point.x = Point.X..from.top.left.corner., point.y = Point.Y..from.top.left.corner.)%>%
  tidyr::separate(Image.Name, into = c("campaignid","sample"),sep="/")%>%
  filter(!habitat%in%c(NA, "")) %>%
  dplyr::mutate(habitat=paste("hab:",habitat))%>%
  glimpse()

str(habitat)
head(habitat)

## Make new column for presence-absence of seagrass --
habitat[substr(habitat$Display.Code,1,2)=="BS", "Seagrass"] <- "1"
head(habitat)

# Replace Nas with zeros --
habitat$Seagrass[is.na(habitat$Seagrass)] <- 0
head(habitat)


#### Now combine with metadata ----
dir(raw.dir) # get names of files in this directory

metadata2014 <- read.csv(paste(raw.dir, "2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv", sep='/'))
names(metadata2014)

metadata2014 <- metadata2014 %>%
  dplyr::select(Sample, Latitude, Longitude, Date, Time, Location, Status, Site, Comment) %>%
  dplyr::mutate(campaignid = "2014-12_Geographe_Bay_stereoBRUVs") %>%
  glimpse()

## 
metadata2007 <- read.csv(paste(raw.dir, "2007-03_Capes.MF_stereoBRUVs_Metadata.csv", sep='/'))
names(metadata2007)

metadata2007 <- metadata2007 %>%
  dplyr::select(Sample, Latitude, Longitude, Date, Time, Location, Status, Site, Comment) %>%
  dplyr::mutate(campaignid = "2007-03_Capes_MF_stereoBRUVs") %>%
  glimpse()

#### Read in 2014 image metadata ----
images <- read.csv(paste(raw.dir, "2014-12_Geographe_Bay_stereoBRUVs_images.csv", sep ='/'))%>%
  dplyr::mutate(campaignid = "2014-12_Geographe_Bay_stereoBRUVs")%>%
  glimpse()

# Combine all metadata into one dataframe --
metadata <- bind_rows(metadata2007,metadata2014) 
metadata <- metadata%>%
  dplyr::mutate(id = paste(campaignid, Sample, sep = "."))
head(metadata)

## don't think I really need metadata2, but some stuff that Brooke had from previous scripts --

metadata2 <- metadata%>%
 #dplyr::mutate(id = paste(campaignid, Sample, sep = ".")) %>%
  dplyr::left_join(images)

head(metadata2)

names(metadata2)
metadata2 <- metadata2[,-c(12)]

# Test to see habitat that doesn't have metadata entry ----
missing.metadata <- anti_join(habitat,metadata2) # no images missing metadata 

# Test to see metadata entry that doesn't have habitat ----
missing.habitat <- anti_join(metadata2,habitat) # no missing habitat

# Test to see number of points
non.unique<-habitat%>%
  dplyr::group_by(campaignid,sample)%>%
  dplyr::summarise(n=n())%>%
  filter(n>20) # shows images with more than 20 measurements - this is because of forwards and backwards for 2014


#### Join metadata with point habitat data ----

names(habitat)
habitat <- dplyr::rename(habitat, Sample=sample)
head(habitat)

hab.points <- merge(habitat, metadata, by="Sample", all=T)

#### Match to CMR zones ---- 

# read GB shapefile --
s.dir <- "G:/My Drive/Anita/Shapefiles"

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)
gb

# habitat data into spatial points ---
head(hab.points)
hsp <- hab.points

coordinates(hsp) <- ~Longitude+Latitude
points(hsp)

# extract zone from each point --
points.zone <- raster::extract(gb, hsp, df = T)
head(points.zone)
# check for duplicates --
points.zone$point.ID[duplicated(points.zone$point.ID)]
#points.zone <- points.zone[!duplicated(points.zone$point.ID),] # remove

hab <- as.data.frame(hsp)
head(hab)

# combine habitat and zone dfs --
hab.points.zone <- cbind(hab, points.zone)
head(hab.points.zone)


#### Save presence absence data ----

write.csv(hab.points.zone, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))


#### Remove BRUVS w NAs

df <- read.csv(paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))
str(df)


# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 9001 obs

# Remove unwanted columns
names(df)
df <- df[, c(3:5,9:12,15,26)]
names(df)
head(df)

dfs <- df

coordinates(dfs) <- ~ Longitude + Latitude
points(dfs)
proj4string(dfs) <- proj4string(gb)

writeOGR(dfs, "Y:/Power-Analysis/Bimodal/Data/spatial", "CMR-points", driver = "ESRI Shapefile", overwrite = T)

#####  CLUSTER BRUVS  #####

# Method 1 : hierarchical clustering approach----
# https://gis.stackexchange.com/questions/17638/clustering-spatial-data-in-r

library(geosphere)

# convert data to a SpatialPointsDataFrame object
#xy <- SpatialPointsDataFrame(
 # matrix(c(df$Longitude,df$Latitude), ncol=2), data.frame(ID=seq(1:length(df$Longitude))),
  #proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))

#writeOGR(xy, "Y:/Power-Analysis/Bimodal/Data/spatial", "CMR-points", driver = "ESRI Shapefile")

xyp <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/CMR-points-utm.shp")
xy <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/CMR-points.shp")

## Cluster by zone ----

plot(gb)
plot(xy, add=T)
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
plot(muz, col = muz$clust, add=T) # 7 clusters
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





### ---- ###

# Method 2: MBH ----

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

c.points <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df[, c(2,3,6,8,20:22)]
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

#### T1 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn1 <-as.data.frame(dfn %>% group_by(clust) %>% sample_n(size = 2500))
#str(dfn1)
#dfn1 <- droplevels(dfn1)
#str(dfn1) # 11250 obs

dfn1 <- dfn
names(dfn1)

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrss ~ clust + ZoneNam, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrss ~ clust + ZoneNam, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

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
dfall$CvI <- ifelse(dfall$ZoneName=="National Park Zone", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "NPZ-seag-epower.csv", sep='-'), sep='/'))



### HPZ ####

c.points <- readOGR("Y:/Power-Analysis/Bimodal/Data/spatial/BRUV_clusteredpoints.shp")

df <- read.csv(paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/'))


# Remove unwanted columns
names(df)
head(df)
df <- df[, c(2,3,6,8,20:22)]
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

#### T1 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn1 <-as.data.frame(dfn %>% group_by(clust) %>% sample_n(size = 2500))
#str(dfn1)
#dfn1 <- droplevels(dfn1)
#str(dfn1) # 11250 obs

dfn1 <- dfn
names(dfn1)

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrss ~ clust + ZoneNam, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrss ~ clust + ZoneNam, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

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
dfall$CvI <- ifelse(dfall$ZoneName=="Habitat Protection Zone", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "HPZ-seag-epower.csv", sep='-'), sep='/'))

