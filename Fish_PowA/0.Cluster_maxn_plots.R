

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
library(ggplot2)
library(ggthemes)
library(extrafont)
library(RColorBrewer)
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

## PINK SNAPPER MAXN -----

# Set File to use ----
dir(raw.dir)
f <- "2014-12_Geographe.Bay_stereoBRUVs.king.wrasse.maxn.csv"

# Study name----

study <- "stereo-BRUVs"

# set species name
s <- "King wrasse"

## read fish data ----
# these files already have lat and long

fish <- read.csv(paste(raw.dir, f, sep ='/')) %>%
  dplyr::select(sample,maxn,latitude, longitude, depth) %>% # Select columns to keep
  glimpse()

str(fish) # 322 obs
head(fish)

# test for NAs --
any(is.na(fish))


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
head(fclust)
str(fclust)


## Agregate by cluster ----
clustmean <- aggregate(maxn~clust, data = fclust, mean)
clustvar <- aggregate(maxn~clust, data = fclust, var)
cluststd <- aggregate(maxn~clust, data = fclust, sd)

clustsum <- cbind(clustmean, variance = clustvar[,2], std.dev = cluststd[,2])
head(clustsum)    
str(clustsum)

# PLOT ----
## Set theme
theme_set(theme_bw())

# give the plot a title according to method and zone
titleplot <- paste(s, study, sep=' - ')


p <- ggplot(aes(y = maxn, x = clust, fill = clust),  data = clustsum) + 
  geom_bar(stat ="identity", width = 1) +
  scale_fill_manual(values = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
                                      "#0000CC", "#FF66CC", "#660066", "#00FFFF")) +
  geom_errorbar(aes(ymin = maxn-std.dev, ymax = maxn+std.dev)) +
  labs(fill = "Cluster", title = titleplot) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"), plot.title=element_text(size=14, face = "bold")) 
  
                        

p





