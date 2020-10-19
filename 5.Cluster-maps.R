## Script to make maps of clusters ----


### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(sp)
library(spDta)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(tmap)
library(tmaptools)
library(mapdata)
library(leaflet)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(latticeExtra)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)
library(fields)
library(geoR)
library(gstat)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(patchwork)
library(shinyjs)
#library(elsa)
#install.packages("corrplot")
#library(corrplot)
library(colorspace)
library(broman)
library(viridis)
library(RColorBrewer)


# Clear memory ----
rm(list=ls())


### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep='/')
m.dir <- paste(w.dir, "Multivariate", sep='/')
md.dir <- paste(m.dir, "data", sep='/')
mp.dir <- paste(m.dir, "plots", sep='/')


## load GB shapefiles ----
gb <- readOGR(paste(w.dir, "Data/shapefiles/GeoBay.shp", sep='/'))
plot(gb)


### BRUV clusters ----

# Load BRUV Clusters ----

c <- read.csv(paste(md.dir, "GB_hab_BRUV_cluster_20200914.csv", sep='/'))
str(c)
c$clust  <- as.factor(c$clust)

## Change cluster number to be sequential ----
levels(c$clust)
library(plyr)
c$clust <- revalue(c$clust, c("1" = 'NPZ', "2" = 'HPZ', "3" = 'c1', "4" = 'c2', "5" = 'c3', "6" ='c4', "8"='c5', "10"='c6', "25"='c7', "27"='c8', "31"='c9'))
levels(c$clust)

# make sp ----
cs <- c
coordinates(cs) <- ~longitude+latitude
points(cs, pch = 20, col=cs$clust)
proj4string(cs) <- proj4string(gb)


# reorder clusters ----
plot(gb)

plot(cs[cs$clust=="NPZ",], add=T)
plot(cs[cs$clust=="HPZ",], add=T)
plot(cs[cs$clust=="c1",], add=T) # c5
plot(cs[cs$clust=="c2",], add=T) # c4
plot(cs[cs$clust=="c3",], add=T) # c1
plot(cs[cs$clust=="c4",], add=T) # c3
plot(cs[cs$clust=="c5",], add=T) # c2
plot(cs[cs$clust=="c6",], add=T) # c6
plot(cs[cs$clust=="c7",], add=T) # c7
plot(cs[cs$clust=="c8",], add=T) # c9
plot(cs[cs$clust=="c9",], add=T) # c8

#library(plyr)
levels(cs$clust)
cs$clust <- revalue(cs$clust, c("NPZ" = "NPZ", "HPZ"="HPZ", "c1"='c5',  "c2"='c4',  "c3"='c1',  "c4"='c3',  "c5"='c2',
                              "c6" ='c6', "c7"='c7',  "c8" ='c9', "c9"='c8' ))
levels(cs$clust)
cs$clust <- ordered(cs$clust, levels = c("NPZ", "HPZ", "c1", "c2", "c3","c4", "c5","c6", "c7", "c8", "c9"))


## All BRUV coastal clusters ----

# choose color --
#pal1 <- choose_palette() # 14 colours
#hcl_palettes(plot = TRUE)
pal <- sequential_hcl(14, palette = "viridis")

lab <- levels(cs$clust)

tmaptools::palette_explorer()
pal2 <- viridisLite::viridis(14)
pal3 <- get_brewer_pal("Spectral", n = 14)
pal4 <- get_brewer_pal("Paired", n = 14)

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

map1 <- map + tm_shape(cs) + tm_symbols('clust', size = 0.5,  palette = pal4, shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
map1


## save map ----
tmap_save(map1, paste(mp.dir, "BRUV-multivar-clusters.tiff", sep='/'))

# get the legend ----

map1 <- map + tm_shape(cs) + tm_symbols('clust', size = 4,  palette = pal4, shape = 21, border.col='black') + 
  tm_layout(legend.only = TRUE,
            legend.text.size = 1.2)
map1

## save map ----
tmap_save(map1, paste(mp.dir, "BRUV-multivar-clusters-legend.tiff", sep='/'))


## PLOT BRUV NPZ ----
plot(gb)
plot(cs[cs$clust=="NPZ",], add=T)
plot(cs[cs$clust=="HPZ",], add=T)
plot(cs[cs$clust=="c3",], add=T)
plot(cs[cs$clust=="c7",], add=T)


map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

NPZ <- map + tm_shape(cs[cs$clust=="NPZ",]) + tm_symbols(size = 0.5, col='#FF3333', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
NPZ

HPZ <-  NPZ + tm_shape(cs[cs$clust=="HPZ",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
HPZ

c4 <-  HPZ + tm_shape(cs[cs$clust=="c3",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
c4

c7 <-  c4 + tm_shape(cs[cs$clust=="c7",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
c7

## save map ----
tmap_save(c7, paste(mp.dir, "BRUV-control-clusters.tiff", sep='/'))


## PLOT BRUV HPZ ----
plot(gb)
plot(cs[cs$clust=="HPZ",], add=T)
plot(cs[cs$clust=="c2",], add=T)
plot(cs[cs$clust=="c5",], add=T)
plot(cs[cs$clust=="c9",], add=T)


map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

HPZ <-  map + tm_shape(cs[cs$clust=="HPZ",]) + tm_symbols(size = 0.5, col='#FF3333', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
HPZ

c2 <-  HPZ + tm_shape(cs[cs$clust=="c2",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
c2

c5 <-  c2 + tm_shape(cs[cs$clust=="c5",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
c5

c9 <-  c5 + tm_shape(cs[cs$clust=="c9",]) + tm_symbols(size = 0.5, col='#6699FF', shape = 21, border.col='black') + 
  tm_layout(legend.show = FALSE) + map
c9

## save map ----
tmap_save(c9, paste(mp.dir, "BRUV-control-clusters-HPZ.tiff", sep='/'))



##      ##        ##        ##        ##

## DTV clusters ----

### DTV clusters ----

# Load DTV Clusters ----

c <- read.csv(paste(md.dir, "GB_hab_DTV_cluster.csv", sep='/'))
str(c)
#c$clust  <- as.factor(c$clust)

levels(c$cluster)
c <- c[c$cluster!="deep",]
c <- c[c$cluster!="mid1",]
c <- c[c$cluster!="mid2",]
c <- droplevels(c)

levels(c$cluster)

# make sp ----
cs <- c
coordinates(cs) <- ~Longitude+Latitude
plot(gb)
points(cs, pch = 20, col=cs$cluster)
proj4string(cs) <- proj4string(gb)



#library(plyr)
levels(cs$cluster)
cs$cluster <- revalue(cs$cluster, c( "HPZclust"= 'HPZ',"NPZclust"= "NPZ", "shallow1" = "Shallow1", "shallow2" ="Shallow2", "shallow3" = "Shallow3"))
                                    
levels(cs$cluster)
cs$cluster<- ordered(cs$cluster, levels = c("NPZ", "HPZ", "Shallow1", "Shallow2", "Shallow3"))


plot(gb)
points(npz)

tmaptools::palette_explorer()
pal2 <- viridisLite::viridis(14)
pal3 <- get_brewer_pal("Spectral", n = 14)
pal4 <- get_brewer_pal("Paired", n = 5)
pal4

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map


map1 <- map + tm_shape(cs) + tm_symbols('cluster', size = 0.9,  palette = pal4, shape = 20) + 
  tm_layout(legend.show = FALSE) + map
map1


## save map ----
tmap_save(map1, paste(mp.dir, "DTV-multivar-clusters.tiff", sep='/'))

# get legend

map1 <- map + tm_shape(cs) + tm_symbols('cluster', size = 0.9,  palette = pal4, shape = 21, border.col='black') + 
  tm_layout(legend.only = TRUE) + map
map1

## save map ----
tmap_save(map1, paste(mp.dir, "DTV-multivar-clusters-legend.tiff", sep='/'))


## DTV NPZ controls ----

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map

NPZ <- map + tm_shape(cs[cs$cluster=="NPZ",]) + tm_symbols(size = 0.9, col='#FF3333', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
NPZ

HPZ <-  NPZ + tm_shape(cs[cs$cluster=="HPZ",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
HPZ

s2 <-  HPZ + tm_shape(cs[cs$cluster=="Shallow2",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
s2

s3 <-  s2 + tm_shape(cs[cs$cluster=="Shallow3",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
s3

## save map ----
tmap_save(s3, paste(mp.dir, "DTV-NPZ-control-clusters.tiff", sep='/'))


## DTV HPZ controls ----

map <- tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.8, 0.2), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
map


HPZ <-  map + tm_shape(cs[cs$cluster=="HPZ",]) + tm_symbols(size = 0.9, col='#FF3333', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
HPZ

NPZ <-  HPZ + tm_shape(cs[cs$cluster=="NPZ",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
NPZ

s1 <-  NPZ + tm_shape(cs[cs$cluster=="Shallow1",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
s1

s3 <-  s1 + tm_shape(cs[cs$cluster=="Shallow3",]) + tm_symbols(size = 0.9, col='#6699FF', shape = 20) + 
  tm_layout(legend.show = FALSE) + map
s3

## save map ----
tmap_save(s3, paste(mp.dir, "DTV-HPZ-control-clusters.tiff", sep='/'))
