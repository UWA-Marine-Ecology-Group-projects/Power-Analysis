

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

study <- "DTV"

# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
m.dir <- "C:/Users/21933549/Desktop/GitRepos/Power-Analysis/Data/raw"
raw.dir = paste(w.dir,"Data/raw",sep="/")
tidy.dir = paste(w.dir,"Data/tidy",sep="/")


#### Raw to PA ----
dir(m.dir)

df <- read.csv(paste(m.dir, "DTV_May2020_UWA-HC-08-09-2020.csv", sep='/'))
head(df)
str(df) # 129425 obs


# Catami codes ----
dir(m.dir)
f2 <- "UWA Downwards Facing Catami.csv"

codes <- read.csv(paste(m.dir, f2, sep='/'))
head(codes)

#simplify clsses
codes$c1 <- gsub(": ", ".", codes$DESCRIPTION) # remove : and spaces
codes$c2 <- gsub(". ", ".", codes$c1)
codes$c3 <- gsub(" ", ".", codes$c2)
codes$c4 <- gsub("-", ".", codes$c3)
codes$CLASS <- gsub("\\s*\\([^\\)]+\\)","", codes$c4) # remove parenthesis and content within

#remove unwanted columns
names(codes)
head(codes)
codes <- codes[,c(1,8)]
head(codes)


# DTV metadata ----
dir(m.dir)
f3 <- "Geographe bay towed video datasheet_200525 - Image Data for Reefcloud upload.csv"

mdata <- read.csv(paste(m.dir, f3, sep='/'))
head(mdata)
str(mdata) # 5177 obs = images
any(is.na(mdata)) # some NA's for SPZ 2, 7 and 11
#length(is.na(mdata))
#mdata <- mdata[!is.na(mdata$Latitude), ]
#any(is.na(mdata))
#str(mdata) # 4989 obs. ~ 121 images per transect


## 1. CODES to CLASSES ----
str(codes) # 731 obs - codes

str(df) # 129425 obs
df$image_path <- as.factor(df$image_path)

names(df)
names(df) <- c("image_path",           "point_num",            "CODE")


df2 <- merge(df, codes, by = "CODE", all.x=T)
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


# catami point score ----
head(dtv)

unique(dtv$CLASS)%>%sort()

point.score <- dtv %>%
  distinct()%>%
  dplyr::select(-CODE) %>%  # remove CODE column
  #dplyr::rename(image = image_path) %>%
  #filter(!CLASS%in%c("", NA, "Unscorable", "Fishes", "Fishes.Bon.Fishes")) %>% # remove unwanted classes:unscorable, fishes
  mutate(count = 1) %>% # give them all value of 1
  dplyr::group_by(image) %>%
  tidyr::spread(key = CLASS, value = count, fill=0) %>% # to wide format
  #tidyr::pivot_wider(names_from = CLASS, values_from = count, values_fill=0) %>%
  ungroup() %>%
  #dplyr::group_by(image) %>%
  #dplyr::summarise_all(funs(sum)) %>%
  #ungroup() %>%
  #dplyr::select(-point_num) %>%
  glimpse


str(point.score) # 129,425 x 62

point.score <- as.data.frame(point.score)

# select only seagrass--
sgpoints <- point.score %>%
  dplyr::select(image, 
                Seagrasses.Elliptica.Leaves.Halophil.sp.,
                Seagrasses.Elliptica.Leaves.Halophil.sp..epiphytes.algae,
                Seagrasses.Strap.Lik.Leaves,
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp.,
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.algae,
                Seagrasses.Strap.Lik.Leaves.Amphiboli.sp..epiphytes.other,
                Seagrasses.Strap.Lik.Leaves.epiphytes.algae,
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp.,
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.algae,
                Seagrasses.Strap.Lik.Leaves.Posidoni.sp..epiphytes.other,
                Seagrasses.Strap.Lik.Leaves.Rupi.sp..epiphytes.algae
                ) %>%
  mutate(total.sg=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) #seag presence absence


head(sgpoints)
names(sgpoints)
max(sgpoints$total.sg)


#### Match image names to metadata ----
head(mdata)
# change name to image
names(mdata) <- c("width_pixels" , "height_pixels" , "image" ,"Latitude","Longitude" ,
                  "Reason.for.not.georeferenced", "Zone"  , "Transect")

head(sgpoints)
# mdata and dtv don't have the same amount of obs, but that is fine
str(mdata) # 5177 obs
str(sgpoints) # 129425 obs

# rename image column in metadata --
names(mdata) <-  c("width_pixels", "height_pixels", "image" , "Latitude" , "Longitude" ,
                   "Reason.for.not.georeferenced", "Zone", "Transect")
head(mdata)
str(mdata)


pcmeta <- merge(sgpoints, mdata, by = 'image')
str(pcmeta) # 4847 obs
names(pcmeta)
pcmeta <- pcmeta[,-c(2:12, 14,15,18)]
names(pcmeta)
pcmeta$Transect.id <- as.factor(paste(pcmeta$Zone, pcmeta$Transect, sep ='.'))
head(pcmeta)
str(pcmeta) # 41 levels



#save all seagrass presence absence ----
write.csv(pcmeta, paste(raw.dir, "DTV_seagrass_presence_absence_alltransects.csv", sep='/'))


#### PREPARE DATA FOR E POWER -----

# read data
df <- read.csv(paste(raw.dir, "DTV_seagrass_presence_absence_alltransects.csv", sep='/'))

str(df)
df$Transect.id <- as.factor(df$Transect.id)

dfs <- df
dfs <- na.omit(dfs)
dfs$Lat <- paste('-', dfs$Latitude, sep='')
str(dfs)
dfs$Lat <- as.numeric(dfs$Lat)
head(dfs)
coordinates(dfs) <- ~Longitude+Lat
plot(dfs)

# to see points if wanted --
#points(dfs[dfs$Transect.id == "HPZ.1",], col ='red')
#points(dfs[dfs$Transect.id == "HPZ.2",], col ='red')
#points(dfs[dfs$Transect.id == "HPZ.3",], col ='red')

# remove NPZ zone ----
levels(df$Zone)
df <- df[df$Zone != "NPZ",]
#df <- df[df$Zone != "HPZ",]
df <- droplevels(df)
levels(df$Zone)

# Group transects into clusters ----
levels(df$Transect.id)
lt <- levels(df$Transect.id)
cluster <- c( "HPZ.c1" , "HPZ.c1",  "HPZ.c1" , "HPZ.c1"  ,"HPZ.c1"  ,"HPZ.c2" , "HPZ.c2",  "HPZ.c2",  "HPZ.c2" ,
              "MUZ.c1" , "MUZ.c1" ,"MUZ.c2" ,"MUZ.c1" , "MUZ.c1" , "MUZ.c1" , "MUZ.c1" , "MUZ.c2",  "MUZ.c2" , 
              "MUZ.c2", "MUZ.c2" , "SPZ.c1" , "SPZ.c1", "SPZ.c1", "SPZ.c2", "SPZ.c1" , "SPZ.c1",  "SPZ.c1" ,
              "SPZ.c2"  ,"SPZ.c2", "SPZ.c2",  "SPZ.c2",  "SPZ.c2" )
              
              

clusterf <- as.data.frame(cbind(lt, cluster))
head(clusterf)
names(clusterf) <- c("Transect.id"  ,    "cluster")

df2 <- merge(df, clusterf, by = "Transect.id")
head(df2)
str(df2)
df2$image <- as.factor(df2$image)
df2$Transect <- as.factor(df2$Transect)
df2$Zone <- as.factor(df2$Zone)
df2$cluster <- as.factor(df2$cluster)
names(df2)
str(df2)
df2$Latitude <- df2$Latitude*(-1)
head(df2)

df3 <- df2
df3 <- na.omit(df3)
coordinates(df3) <- ~Longitude+Latitude
plot(df3, pch=21, bg = df3$cluster)

df <- df2
str(df2)

#### MUZ ####

# Remove unnecessary clusters ----
# for NPZ I only need shallow 1, 2, 3
levels(df2$cluster)

dfn <- df2

#### T1 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

names(dfn1)


## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zone, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zone, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn2 <- as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zone, data = dfn2, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zone, data = dfn2, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df2 <- cbind(sg.pres1, no.scored1[,3])
df2
names(df2) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zone, data = dfn3, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zone, data = dfn3, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df3 <- cbind(sg.pres1, no.scored1[,3])
df3
names(df3) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df3$Time <- "T3"
df3

## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)
str(dfall)
# Make control impact column
levels(dfall$Cluster)
dfall <- droplevels(dfall)
levels(dfall$Cluster)
dfall$CvI <- ifelse(dfall$ZoneName=="MUZ", "Impact", "Control")
head(dfall)
summary(dfall)
str(dfall)
levels(dfall$Cluster)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "MUZ-seag-epower-22102020.csv", sep='-'), sep='/'))




#### SPZ ####

# read data
df <- read.csv(paste(raw.dir, "DTV_seagrass_presence_absence_alltransects.csv", sep='/'))

str(df)
df$Transect.id <- as.factor(df$Transect.id)

dfs <- df
dfs <- na.omit(dfs)
dfs$Lat <- paste('-', dfs$Latitude, sep='')
str(dfs)
dfs$Lat <- as.numeric(dfs$Lat)
head(dfs)
#coordinates(dfs) <- ~Longitude+Lat
#plot(dfs)

# to see points if wanted --
#points(dfs[dfs$Transect.id == "HPZ.1",], col ='red')
#points(dfs[dfs$Transect.id == "HPZ.2",], col ='red')
#points(dfs[dfs$Transect.id == "HPZ.3",], col ='red')

# remove NPZ zone ----
levels(df$Zone)
df <- df[df$Zone != "NPZ",]
df <- df[df$Zone != "HPZ",]
df <- droplevels(df)
levels(df$Zone)

# Group transects into clusters ----
levels(df$Transect.id)
lt <- levels(df$Transect.id)
cluster <- c( "MUZ.c1",  "MUZ.c2", "MUZ.c4", "MUZ.c1", "MUZ.c1" , "MUZ.c2",
              "MUZ.c2",  "MUZ.c4", "MUZ.c3" , "MUZ.c3" , "MUZ.c3",  "SPZ.c1",  
              "SPZ.c1" ,"SPZ.c1", "SPZ.c2", "SPZ.c1" , "SPZ.c1" , "SPZ.c1" , 
              "SPZ.c2" , "SPZ.c2" , "SPZ.c2",  "SPZ.c2",  "SPZ.c2" )
           



clusterf <- as.data.frame(cbind(lt, cluster))
head(clusterf)
names(clusterf) <- c("Transect.id"  ,    "cluster")

df2 <- merge(df, clusterf, by = "Transect.id")
head(df2)
str(df2)
df2$image <- as.factor(df2$image)
df2$Transect <- as.factor(df2$Transect)
df2$Zone <- as.factor(df2$Zone)
df2$cluster <- as.factor(df2$cluster)
names(df2)
str(df2)
df2$Latitude <- df2$Latitude*(-1)
head(df2)

levels(df2$cluster)

## Make MUZ deep and shallow for controls
cluster <- levels(df2$cluster)
Zones <- c("MUZdeep", "MUZdeep", "MUZshallow", "MUZshallow", "SPZ", "SPZ")
zone2 <- cbind(cluster, Zones)

df3 <- merge(df2, zone2, by = "cluster")
head(df3)


#df3 <- na.omit(df3)
#coordinates(df3) <- ~Longitude+Latitude
#plot(df3, pch=21, bg = df3$cluster)

df <- df3
str(df3)
summary(df3)


dfn <- df3

#### T1 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

names(dfn1)


## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zones, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zones, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn2 <-as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zones, data = dfn2, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zones, data = dfn2, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df2 <- cbind(sg.pres1, no.scored1[,3])
df2
names(df2) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

# subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(cluster) %>% sample_n(size = 2500))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(total.sg ~ cluster + Zones, data = dfn3, sum)
sg.pres1

no.scored1 <-  aggregate(total.sg ~ cluster + Zones, data = dfn3, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df3 <- cbind(sg.pres1, no.scored1[,3])
df3
names(df3) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df3$Time <- "T3"
df3

## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)
str(dfall)
# Make control impact column
levels(dfall$Cluster)
dfall <- droplevels(dfall)
levels(dfall$Cluster)
dfall$CvI <- ifelse(dfall$ZoneName=="SPZ", "Impact", "Control")
head(dfall)
summary(dfall)


#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "SPZ-seag-epower-22102020.csv", sep='-'), sep='/'))




