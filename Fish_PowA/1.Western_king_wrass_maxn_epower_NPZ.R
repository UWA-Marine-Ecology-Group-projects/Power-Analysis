

##### New Fish Power Analysis ######

####      Abundance of Western King Wrass     #####

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



# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
raw.dir = paste(w.dir,"data/raw",sep="/")
tidy.dir = paste(w.dir,"data/tidy",sep="/")
s.dir <- "G:/My Drive/Anita/Shapefiles" 


# Read CMR shapefile ----
gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)

# Set File to use ----
f <- "2014-12_Geographe.Bay_stereoBRUVs.king.wrasse.maxn.csv"

# Study name----

study <- "stereo-BRUVs"

# set species name
s <- "king-wrasse"

## read fish data ----
# these files already have lat and long

fish <- read.csv(paste(raw.dir, f, sep ='/')) %>%
  dplyr::select(sample,maxn,latitude, longitude, depth) %>% # Select columns to keep
  glimpse()

str(fish) # 322 obs
head(fish)

# test for NAs --
any(is.na(fish))

# check their location 
fsp <- fish
coordinates(fsp) <- ~longitude+latitude
points(fsp)

## read clusters to match to fish sample ----
dir(tidy.dir)

clusterdf <- "GB_fish_cluster.csv"

clusters <- read.csv(paste(tidy.dir, clusterdf, sep ='/')) # 135 obs
str(clusters)
clusters$clust <- as.factor(clusters$clust)
names(clusters)
# remove unnecessary columns --
clusters <- clusters[,-c(3:160)]
head(clusters)
# make sample the same as in fish df --
names(clusters) <- c("X",      "sample", "clust" )
names(clusters)

# Match clusters to samples number ----
fclust <- merge(fish, clusters, by='sample')
head(fclust)
str(fclust) #135 obs
# check on map
fcsp <- fclust
coordinates(fcsp) <- ~longitude+latitude
points(fcsp, col=fcsp$clust)
levels(fclust$clust)

# Remove unnecessary clusters ----
# for NPZ I only need 1, 4, 27

fclustp <- fclust[fclust$clust == '1' | fclust$clust == '4' | fclust$clust == '27',  ]
head(fclustp)
str(fclustp) # 31 obs
fclustp <- droplevels(fclustp)
summary(fclustp)
levels(fclustp$clust)



### Divide samples into artificial 'sampling times' ----
cl <- split(fclustp, fclustp$clust)
cl

# Add time to each cluster
# Cluster 1
cl$`1`$time <- c('T1','T1','T1','T2','T2','T2','T3','T3','T3')
# Cluster 4
cl$`4`$time <- c('T1','T1','T1','T1','T2','T2','T2','T3','T3','T3')
# Cluster 27
cl$`27`$time <- c('T1','T1','T1','T1','T2','T2','T2','T2','T3','T3','T3','T3')

# rejoin them into one df --

clf <- do.call(rbind.data.frame, cl)
clf
str(clf)
clf$time <- as.factor(clf$time)

# Make Period Column ----
clf$Period <- "Before"
head(clf)
 
# Make control impact column ----
levels(clf$clust)
clf$CvI <- ifelse(clf$clust=="1", "Impact", "Control")
head(clf)
str(clf)
clf$Period <- as.factor(clf$Period)
clf$CvI <- as.factor(clf$CvI)
str(clf)

#### Save data for epower ----
write.csv(clf, paste(tidy.dir, paste(study, s, "NPZ_epower.csv", sep='-'), sep='/'))
