
########      Match downward facing towed video to metadata     #####

# load required libraries ####
library( rgdal)
library( sp)
library( raster)
library( spatialEco)
library( tidyverse)
library( maptools)
library( dplyr)
library(tidyr)
library(readr)
library(stringr)
library(readr)

# Clear memory ----
rm(list=ls())

# Set working directory ####
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
raw.dir <- paste(w.dir, "Data/raw", sep ='/')
tidy.dir <- paste(w.dir, "Data/tidy", sep='/')


## Read Shape file of GEO bay Marine Park zones ----
geobay <- readOGR(paste(w.dir, "Data", "shapefiles", "GeoBay.shp", sep='/'))

# see the shapefile you just loaded
plot(geobay)
geobay$ZoneName
gb.zones <- c("Habitat Protection Zone", "Special Purpose Zone (Mining Exclusion)",
              "National Park Zone", "Multiple Use Zone")

gb.zones

proj4string(geobay) # "+proj=longlat +ellps=GRS80 +no_defs"

#### Read raw data ----

# export from ReefCloud --
dir(raw.dir)
f1<- "DTV_May2020_UWA-HC-08-09-2020.csv"

dtv <- read.csv(paste(raw.dir, f1, sep='/'))
# This is a data frame with 3 columns sample, lat and long, see:
head(dtv)
# this is to see the properties of the data frame
str(dtv) # there are 129425 obs. -- DTV points

# Catami codes --
dir(raw.dir)
f2 <- "UWA Downwards Facing Catami.csv"

codes <- read.csv(paste(raw.dir, f2, sep='/'))
head(codes)

# DTV metadata --
dir(raw.dir)
f3 <- "Geographe bay towed video datasheet_200525 - Image Data for Reefcloud upload.csv"

mdata <- read.csv(paste(raw.dir, f3, sep='/'))
head(mdata)
str(mdata) # 5177 obs = images

## 1. ReefCloud to percent cover ----

dtv <- dtv %>% 
  tidyr::separate(image_path, into = c("surveys", "institution", "id1", "id2", "image"), sep = "/") %>% 
  dplyr::rename(class = human_classification) %>% # change from human_class into class
  dplyr::mutate(image = as.factor(image)) %>%
  glimpse()

str(dtv)
head(dtv)



# CREATE catami point score ----

unique(dtv$class)%>%sort()

point.score <- dtv %>%
  distinct()%>%
  #dplyr::select(-point_num) %>%
  filter(!class%in%c("", NA, "U")) %>% # remove unwanted classes: U=unscorable
  mutate(count = 1) %>%
  dplyr::group_by(image) %>%
  spread(key = class, value = count, fill=0) %>%
  dplyr::select(-c(surveys, institution, id1, id2)) %>%
  ungroup() %>%
  dplyr::group_by(image) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup()


head(point.score)
names(point.score)
str(point.score) # 5024 obs - 5177 levels for image
point.score <- droplevels(point.score) #  5024 levels
str(point.score) # 5024 levels for image

# calculate % cover ---- UP TO HERE
percent.cover <- point.score %>%
  tidyr::separate(image, into = c("image.id", "type"), sep = ".j") %>% # remove the .jpeg
  dplyr::select(-c(point_num, type)) %>%
  mutate(total.sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(image.id) %>%
  mutate_if(is.numeric, funs(./total.sum*100)) %>%
  mutate_if(is.numeric, funs(round(.,digits=2))) %>%
  #tidyr::separate(image, into = c("image", "type"), sep = ".") %>%
  #mutate_at(vars(starts_with("hab: ")),funs(./total.sum*100))%>%
  #mutate_at(vars(starts_with("hab: ")),funs(round(.,digits=2)))%>%
  #dplyr::select(-total.sum) %>%
  ungroup()%>%
  #left_join(metadata) %>%
  #filter(!is.na(latitude))%>%
  distinct()%>%
  #tidyr::separate(image, into = c("image", "type"), sep = ".") %>%
  glimpse()



class(percent.cover)
pc <- as.data.frame(percent.cover)
str(pc)
head(pc)
names(pc)


# double check the sum --
pc2 <- pc %>%
  mutate(total.sum2 = rowSums(.[,2:60], na.rm = TRUE))

head(pc2)
class(pc2) 
str(pc2) # 5024 obs


#### Match image names to metadata ----
head(mdata)
head(pc2)
 
# mdata and dtv don't have the same amount of obs, but that is fine
str(mdata) # 5177 obs
str(pc2) # 5024 obs

# rename image column in metadata --
names(mdata) <-  c("width_pixels", "height_pixels", "image.id" , "Latitude" , "Longitude" ,
                   "Reason.for.not.georeferenced", "Zone", "Transect")
head(mdata)

pcmeta <- merge(pc2, mdata, by = 'image.id')
str(pcmeta) # 5024 obs
names(pcmeta)
pcmeta <- pcmeta[,-c(63,64,67)]
names(pcmeta)

write.csv(pcmeta, paste(raw.dir, "DTV_GeoBay_May2020_percent.cover.n.metadata.csv", sep='/'))
