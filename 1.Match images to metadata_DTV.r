
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
library(reshape2)

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
str(dtv) # there are 129425 obs. -- DTV points = 5177 images

# Catami codes --
dir(raw.dir)
f2 <- "UWA Downwards Facing Catami.csv"

codes <- read.csv(paste(raw.dir, f2, sep='/'))
head(codes)
#simplify clsses
codes$c1 <- gsub(": ", ".", codes$DESCRIPTION) # remove : and spaces
codes$c2 <- gsub(". ", ".", codes$c1)
codes$c3 <- gsub(" ", ".", codes$c2)
codes$CLASS <- gsub("\\s*\\([^\\)]+\\)","", codes$c3) # remove parenthesis and content within

#remove unwanted columns
names(codes)
codes <- codes[,c(1,7)]


# DTV metadata --
dir(raw.dir)
f3 <- "Geographe bay towed video datasheet_200525 - Image Data for Reefcloud upload.csv"

mdata <- read.csv(paste(raw.dir, f3, sep='/'))
head(mdata)
str(mdata) # 5177 obs = images
any(is.na(mdata))
length(is.na(mdata))
mdata <- mdata[!is.na(mdata$Latitude), ]
any(is.na(mdata))
str(mdata) # 4989 obs. ~ 121 images per transect


## 1. CODES to CLASSES ----
str(codes) # 731 obs - codes

str(dtv) # 129425 obs
dtv$image_path <- as.factor(dtv$image_path)

names(dtv)
names(dtv) <- c("image_path",           "point_num",            "CODE")


df2 <- merge(dtv, codes, by = "CODE", all.x=T)
head(df2)
str(df2) # 129,425 obs
dtv <- df2 %>%
  dplyr::select(CODE, image_path, point_num, CLASS) %>% # columns to keep
  dplyr::mutate_if(is.character, as.factor) %>% # several cols into factors
  tidyr::separate(image_path, into = c("surveys", "institution", "id1", "id2", "image"), sep = "/") %>%
  #dplyr::mutate(image = as.factor(image)) %>%
  tidyr::separate(image, into = c("image", "type"), sep = ".j") %>% # remove the .jpeg
  dplyr::mutate(image = as.factor(image)) %>%
  dplyr::select(-c(surveys, institution, id1, id2, type)) %>%
  glimpse # Rows: 129,425



## 2. ReefCloud to percent cover ----

str(dtv) # 5177 images 129425 obs
head(dtv)
names(dtv)
length(levels(dtv$image)) # 5177
any(is.na(dtv))


# catami point score ----

unique(dtv$CLASS)%>%sort()

point.score <- dtv %>%
  distinct()%>%
  dplyr::select(-CODE) %>%  # remove CODE column
  #dplyr::rename(image = image_path) %>%
  filter(!CLASS%in%c("", NA, "Unscorable", "Fishes", "Fishes.Bon.Fishes")) %>% # remove unwanted classes:unscorable, fishes
  mutate(count = 1) %>% # give them all value of 1
  dplyr::group_by(image) %>%
  tidyr::spread(key = CLASS, value = count, fill=0) %>% # to wide format
  #tidyr::pivot_wider(names_from = CLASS, values_from = count, values_fill=0) %>%
  ungroup() %>%
  dplyr::group_by(image) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::select(-point_num) %>%
  glimpse


head(point.score) # in wide format now
names(point.score)
str(point.score) # 5024 obs - 5177 levels for image
point.score <- droplevels(point.score) #  5024 levels
str(point.score) 
length(levels(point.score$image)) # 5024


# calculate % cover ---- 
percent.cover <- point.score %>%
  mutate(total.sum=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>% # create sum per image
  dplyr::group_by(image) %>%
  mutate_if(is.numeric, funs(./total.sum*100)) %>%
  mutate_if(is.numeric, funs(round(.,digits=2))) %>%
  ungroup()%>%
  #left_join(metadata) %>%
  #filter(!is.na(latitude))%>%
  distinct()%>%
  #tidyr::separate(image, into = c("image", "type"), sep = ".") %>%
  glimpse()



class(percent.cover)
pc <- as.data.frame(percent.cover)
str(pc) # 5024 obs
head(pc)
names(pc)


# double check the sum --
pc2 <- pc %>%
  mutate(total.sum2 = rowSums(.[,2:58], na.rm = TRUE))

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
names(mdata) <-  c("width_pixels", "height_pixels", "image" , "Latitude" , "Longitude" ,
                   "Reason.for.not.georeferenced", "Zone", "Transect")
head(mdata)
str(mdata)


pcmeta <- merge(pc2, mdata, by = 'image')
str(pcmeta) # 4847 obs
names(pcmeta)
pcmeta <- pcmeta[,-c(60:62,65)]
names(pcmeta)
pcmeta$Transect.id <- as.factor(paste(pcmeta$Zone, pcmeta$Transect, sep ='.'))

str(pcmeta) # 41 levels

#write.csv(pcmeta, paste(raw.dir, "DTV_GeoBay_May2020_percent.cover.n.metadata.csv", sep='/'))


## read all data ----
df <- read.csv(paste(raw.dir, "DTV_GeoBay_May2020_percent.cover.n.metadata.csv", sep='/'))
str(df)
head(df)
#df$Latitude <- as.numeric(df$Latitude)
#df$Longitude <- as.numeric(df$Longitude)
df$Zone <- as.factor(df$Zone)
head(df)
any(is.na(df$Latitude))
which(is.na(df$Latitude))
# remove NA's 
#df <- df[!is.na(df$Latitude), ]
#any(is.na(df$Longitude))

# make south latitude
df$Latitude <- df$Latitude*(-1)
head(df)
# make spatial point
dfs <- df
coordinates(dfs) <- ~Longitude+Latitude
proj4string(dfs) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(geobay)
plot(dfs, col = dfs$Zone, add=T)

## Remove columns with only zero values --
df2 <- df[,apply(df,2,function(df) !all(df==0))]
str(df2)
names(df) # 65 cols
names(df2) # 56 cols
length(levels(df2$Transect.id)) #41

# Detailed habitat classes ----

# this from Brooke's Benthobox to percent cover script --

names(df2)
str(df2)

hab.detailed <- df2 %>%
  # total seagrass
  dplyr::mutate(total.seagrass=
                  Seagrasses.Elliptica.Leaves.Halophil.sp. +                
                Seagrasses.Elliptica.Leaves.Halophil.sp..epiphytes.algae +
                Seagrasses.Strap.Lik.Leaves  +                            
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp.   +             
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.algae+
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.other+
                Seagrasses.Strap.Lik.Leaves.epiphytes.algae   +           
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp. +                
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.algae+ 
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.other ) %>%
  # total seagrass with epiphytes
  dplyr::mutate(total.seagrass.with.epiphytes=                
                  Seagrasses.Elliptica.Leaves.Halophil.sp..epiphytes.algae +            
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.algae+
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.other+
                  Seagrasses.Strap.Lik.Leaves.epiphytes.algae   +               
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.algae+ 
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.other) %>%
  # Posidonia
  dplyr::mutate(Posidonia=
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp. +                
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.algae+ 
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.other )%>%
  dplyr::mutate(Posidonia.with.epiphytes=
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.algae+ 
                  Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.other )%>%
  # Amphibolis
  dplyr::mutate(Amphibolis=
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp.   +             
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.algae+
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.other) %>%
  dplyr::mutate(Amphibolis.with.epiphytes=
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.algae+
                  Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.other) %>%

  # total Macroalgae
  dplyr::mutate(total.Macroalgae=
                  Macroalgae           +                                    
                Macroalgae.Articulate.Calcareous.Red +                    
                Macroalgae.Drif.Algae     +                               
                Macroalgae.Erec.Coars.Branching +                         
                Macroalgae.Erec.Coars.Branching.Brown+                    
                Macroalgae.Erec.Coars.Branching.Brown.Drift+              
                Macroalgae.Erec.Coars.Branching.Brown.Sargassu.Spp+       
                Macroalgae.Erec.Coars.Branching.Red   +                   
                Macroalgae.Erec.Fin.Branching +                           
                Macroalgae.Erec.Fin.Branching.Brown+                      
                Macroalgae.Erec.Fin.Branching.Brown.Brow.Understor.Algae +
                Macroalgae.Erec.Fin.Branching.Red  +                      
                Macroalgae.Erec.Fin.Branching.Red.Foliose +               
                Macroalgae.Encrusting.Brown    +                          
                Macroalgae.Encrusting.Red  +                              
                Macroalgae.Filamentou..Filiform.Green  +                  
                Macroalgae.Filamentou..Filiform.Turfin.Algae   +          
                Macroalgae.Globos..Saccate.Brown    +                     
                Macroalgae.Globos..Saccate.Green +                        
                Macroalgae.Laminate.Brown +                               
                Macroalgae.Laminate.Green +                               
                Macroalgae.Laminate.Red)%>%
  #total turf macroalgae
  dplyr::mutate(Turf.algae =
                  Macroalgae.Filamentou..Filiform.Green  +                  
                  Macroalgae.Filamentou..Filiform.Turfin.Algae   +          
                  Macroalgae.Globos..Saccate.Brown    +                     
                  Macroalgae.Globos..Saccate.Green +                        
                  Macroalgae.Laminate.Brown +                               
                  Macroalgae.Laminate.Green +                               
                  Macroalgae.Laminate.Red)%>%
  # total erect course
  dplyr::mutate(Erect.coarse.branching=
                  Macroalgae.Erec.Coars.Branching +                         
                  Macroalgae.Erec.Coars.Branching.Brown+                    
                  Macroalgae.Erec.Coars.Branching.Brown.Drift+              
                  Macroalgae.Erec.Coars.Branching.Brown.Sargassu.Spp+       
                  Macroalgae.Erec.Coars.Branching.Red  )%>%
  # total fine course
  dplyr::mutate(Erect.fine.branching=
                  Macroalgae.Erec.Fin.Branching +                           
                  Macroalgae.Erec.Fin.Branching.Brown+                      
                  Macroalgae.Erec.Fin.Branching.Brown.Brow.Understor.Algae +
                  Macroalgae.Erec.Fin.Branching.Red  +                      
                  Macroalgae.Erec.Fin.Branching.Red.Foliose)%>%

  # total sponges
  dplyr::mutate(total.sponges=
                Sponges  +                                                
                Sponges.Cup.Likes.Cups.Cu..Goblet    +                    
                Sponges.Massiv.Forms)%>%
   # other inverts 
   dplyr::mutate(other.inverts=
                 Ascidians+                                                
                 Bryozoa    +                                              
                 Cnidaria.Corals   +                                       
                 Echinoderms.Feathe.Stars.Stalke.Crinoids)%>%
   # select columns
  dplyr::select(image,
                total.seagrass,
                total.seagrass.with.epiphytes,
                Amphibolis,Amphibolis.with.epiphytes,
                Posidonia,Posidonia.with.epiphytes,
                total.Macroalgae,
                Turf.algae,
                Erect.coarse.branching,
                Erect.fine.branching,
                #Large.canopy.forming,
                total.sponges, 
                other.inverts,
                Latitude,
                Longitude,
                Zone,
                Transect,
                Transect.id) %>%
  glimpse


#write.csv(hab.detailed, paste(raw.dir, "DTV_detailed_habitat_percent.cover.csv", sep='/'))
