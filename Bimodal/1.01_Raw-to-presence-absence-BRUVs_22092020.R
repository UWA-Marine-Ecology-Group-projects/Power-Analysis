

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

#write.csv(hab.points.zone, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))


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
str(df)

# add column of sequential numbers for id and match with clusters
df$X <- 1:nrow(df) 
head(df)

dfs <- df

coordinates(dfs) <- ~ Longitude + Latitude
points(dfs)
proj4string(dfs) <- proj4string(gb)

#writeOGR(dfs, "Y:/Power-Analysis/Bimodal/Data/spatial", "CMR-points", driver = "ESRI Shapefile", overwrite = T)

#####  CLUSTER BRUVS  #####

# Using the same clusters from Fish Power Analysis ----
# After Multivariate analysis:
# NPZ: Clusters 1, 2, 6, 25
# HPZ: Clusters 2, 8, 3, 27


## Get data ready for epower ----

### NPZ ####
# NPZ: Clusters 1, 2, 6, 25
#c.points <- readOGR(paste(w.dir, "Data/spatial/BRUV_clusteredpoints.shp", sep='/')) # Not coastal
#df <- read.csv(paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/')) # Not coastal

## Open coastal clusters -----
w.dir
c.dir <- "C:/Users/21933549/Desktop/GitRepos/Power-Analysis/Fish_PowA/data/tidy"

clusters1 <- read.csv(paste(c.dir, "BRUV_coastalclusters.csv", sep='/'))
head(clusters1)
str(clusters1) # 4946 obs
clusters1$clust <- as.factor(clusters1$clust)
clusters1$Sample <- as.factor(clusters1$Sample)
clusters1 <- clusters1 %>%
  dplyr::select(Sample, Seagrss, ZoneName, clust, coords.x1, coords.x2)

head(clusters1)
csp <- clusters1
coordinates(csp) <- ~coords.x1+coords.x2
points(csp, col=csp$clust, pch=20)

clusters <- aggregate(Seagrss~Sample+clust, data = clusters1, sum)
head(clusters)
str(clusters)

sgclust <- merge(clusters, df, by ="Sample")
str(sgclust)

# Remove unnecessary clusters ----
# for NPZ I only need 1, 2, 6, 25

dfn <- sgclust[sgclust$clust == '1' | sgclust$clust == '2' | sgclust$clust == '6' | sgclust$clust == '25',  ]
str(sgclust)



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

sg.pres1 <- aggregate(Seagrass ~ clust + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn2)
#dfn2 <- droplevels(dfn2)
#str(dfn2) # 11250 obs

df2 <- df1
df2$Time <- "T2"
head(df2)


#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn3)
#dfn3 <- droplevels(dfn3)
#str(dfn3) # 11250 obs

df3 <- df1
df3$Time <- "T3"
head(df3)


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
dfall$CvI <- ifelse(dfall$Cluster=="1", "Impact", "Control")
head(dfall)


#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "NPZ-seag-epower-22092020.csv", sep='-'), sep='/'))



### HPZ ####


# HPZ: Clusters 2, 8, 3, 27
#c.points <- readOGR(paste(w.dir, "Data/spatial/BRUV_clusteredpoints.shp", sep='/')) # Not coastal
#df <- read.csv(paste(tidy.dir, "BRUV_clusteredpoints.csv", sep ='/')) # Not coastal

## Open coastal clusters -----
w.dir
c.dir <- "C:/Users/21933549/Desktop/GitRepos/Power-Analysis/Fish_PowA/data/tidy"

clusters1 <- read.csv(paste(c.dir, "BRUV_coastalclusters.csv", sep='/'))
head(clusters1)
str(clusters1) # 4946 obs
clusters1$clust <- as.factor(clusters1$clust)
clusters1$Sample <- as.factor(clusters1$Sample)
clusters1 <- clusters1 %>%
  dplyr::select(Sample, Seagrss, ZoneName, clust, coords.x1, coords.x2)

head(clusters1)
csp <- clusters1
coordinates(csp) <- ~coords.x1+coords.x2
points(csp, col=csp$clust, pch=20)

clusters <- aggregate(Seagrss~Sample+clust, data = clusters1, sum)
head(clusters)
str(clusters)

sgclust <- merge(clusters, df, by ="Sample")
str(sgclust)

# Remove unnecessary clusters ----
# for NPZ I only need 2, 8, 3, 27

dfn <- sgclust[sgclust$clust == '2' | sgclust$clust == '3' | sgclust$clust == '8' | sgclust$clust == '27',  ]
str(dfn)
names(dfn)
head(dfn)



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

sg.pres1 <- aggregate(Seagrass ~ clust + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ clust + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Cluster", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Cluster",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn2)
#dfn2 <- droplevels(dfn2)
#str(dfn2) # 11250 obs

df2 <- df1
df2$Time <- "T2"
head(df2)


#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

#dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
#str(dfn3)
#dfn3 <- droplevels(dfn3)
#str(dfn3) # 11250 obs

df3 <- df1
df3$Time <- "T3"
head(df3)


## joint these together --

dfall <- rbind(df1, df2, df3)
dfall


# Make Period Column
dfall$Period <- "Before"
names(dfall)

# Make control impact column
levels(dfall$Cluster)
dfall <-droplevels(dfall)
levels(dfall$Cluster)
dfall$CvI <- ifelse(dfall$Cluster=="2", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "HPZ-seag-epower-22092020.csv", sep='-'), sep='/'))



