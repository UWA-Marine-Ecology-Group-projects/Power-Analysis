

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
library(rgdal)#install.packages("remotes")
#remotes::install_github("UWAMEGFisheries/GlobalArchive")
#library(GlobalArchive)

rm(list=ls()) #clear memory

# Study name----

study <- "Towed-Video"

# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
raw.dir = paste(w.dir,"Data/raw",sep="/")
tidy.dir = paste(w.dir,"Data/tidy",sep="/")


#### Raw to PA ----
dir(raw.dir)
## read data --
habitat <- read.csv(paste(raw.dir, "towed_PointTags.csv", sep ='/')) %>%
  dplyr::select(Image.Name,Image.Source.Dataset,Point.X..from.top.left.corner., Point.Y..from.top.left.corner.,Display.Name, Display.Code) %>% # Select columns to keep
  mutate(Image.Name=str_replace_all(Image.Name,c("https://uwa-auv.s3-ap-southeast-2.amazonaws.com/"="",".jpg"="","/Images"="","2014_TowedVideo/"="",".jpeg"=""))) %>% # Remove url from sample names
  dplyr::rename(Transect = Image.Source.Dataset, habitat = Display.Name, point.x = Point.X..from.top.left.corner., point.y = Point.Y..from.top.left.corner.)%>%
  tidyr::separate(Image.Name, into = c("Image.Name","Sample"),sep="/")%>%
  filter(!habitat%in%c(NA, "")) %>%
  #rename(Sample=Image.Name)%>%
  dplyr::mutate(habitat=paste("hab:",habitat))%>%
  glimpse()

str(habitat)
head(habitat)
habitat$campaignid <- "2014_TowedVideo"

## Make new column for presence-absence of seagrass --
habitat[substr(habitat$Display.Code,1,2)=="BS", "Seagrass"] <- "1"
head(habitat)

# Replace Nas with zeros --
habitat$Seagrass[is.na(habitat$Seagrass)] <- 0
head(habitat)


#### Now combine with metadata ----
metadata<-read.csv(paste(raw.dir, "2014_GB_Towed_Video_Metadata_dd.csv", sep='/'))%>%
  dplyr::filter(!is.na(latitude))%>%
  dplyr::mutate(sample=str_replace_all(.$image_name,c(".jpeg"="")))%>%
  dplyr::select(sample,latitude,longitude, dataset_name)%>%
  dplyr::rename(Transect = dataset_name, Image.Name = sample) %>%
  glimpse()



# Test to see habitat that doesn't have metadata entry ----
missing.metadata <- anti_join(habitat,metadata) # no images missing metadata 

# Test to see metadata entry that doesn't have habitat ----
missing.habitat <- anti_join(metadata,habitat) # no missing habitat

# Test to see number of points
non.unique<-habitat%>%
  dplyr::group_by(campaignid,Image.Name)%>%
  dplyr::summarise(n=n())%>%
  filter(n>20) # shows images with more than 20 measurements - this is because of forwards and backwards for 2014



#### Join metadata with point habitat data ----

names(habitat)

hab.points <- merge(habitat, metadata[,c(1:3)], by="Image.Name")

#### Match to CMR zones ---- 

# read GB shapefile --
s.dir <- "G:/My Drive/Anita/Shapefiles"

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)
gb

# habitat data into spatial points ---
head(hab.points)
hsp <- hab.points

coordinates(hsp) <- ~longitude+latitude
points(hsp)

# extract zone from each point --
points.zone <- raster::extract(gb, hsp, df = T)
head(points.zone)
# check for duplicates --
points.zone$point.ID[duplicated(points.zone$point.ID)]
points.zone <- points.zone[!duplicated(points.zone$point.ID),] # remove

hab <- as.data.frame(hsp)
head(hab)

# combine habitat and zone dfs --
hab.points.zone <- cbind(hab, points.zone)
head(hab.points.zone)

#### Save presence absence data ----

write.csv(hab.points.zone, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))


## Get data ready for epower ----

### HPZ ####

hab.points.zone <- read.csv(paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))
str(hab.points.zone)


df <- hab.points.zone

# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 25340 obs

# Remove unwanted columns
names(df)
df <- df[, c(4:6,9:12,20)]
names(df)
head(df)


### split the data by zone name--
dfz <- split(df, df$ZoneName)
str(dfz)
# remove special purpose zone
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

levels(dfn$ZoneName)

## Make T2 part of HPZ and  Make T6 and T7 part of another zone
trans <- dfn$Transect
levels(trans)
dfn$zone.sim <- factor(trans, labels = c("HPZ", "HPZ", "MUZ", "MUZ", "MUZ2", "MUZ2"))
summary(dfn)
str(dfn)

levels(dfn$zone.sim)

#### T1 ####

#subsample dat to n=1900(76 images/25 points per transect that is the max in on of the transects) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(Transect) %>% sample_n(size = 1900))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrass ~ Transect + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ Transect + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=1900(76 images/25 points per transect that is the max in on of the transects) to improve speed

dfn2 <-as.data.frame(dfn %>% group_by(Transect) %>% sample_n(size = 1900))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres2 <- aggregate(Seagrass ~ Transect + ZoneName, data = dfn2, sum)
sg.pres2

no.scored2 <-  aggregate(Seagrass ~ Transect + ZoneName, data = dfn2, length)           
no.scored2
names(no.scored2) <- c("Transect", "ZoneName", "no.scored")

df2 <- cbind(sg.pres2, no.scored2[,3])
df2
names(df2) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

#subsample dat to n=1900(76 images/25 points per transect that is the max in on of the transects) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(Transect) %>% sample_n(size = 1900))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres3 <- aggregate(Seagrass ~ Transect + ZoneName, data = dfn3, sum)
sg.pres3

no.scored3 <-  aggregate(Seagrass ~ Transect + ZoneName, data = dfn3, length)           
no.scored3
names(no.scored3) <- c("Transect", "ZoneName", "no.scored")

df3 <- cbind(sg.pres3, no.scored3[,3])
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

## Make T2 part of HPZ and  Make T6 and T7 part of another zone
trans <- dfall$Transect
levels(trans)
dfall$zone.sim <- factor(trans, labels = c("HPZ", "HPZ", "MUZ", "MUZ", "MUZ2", "MUZ2"))
summary(dfall)

levels(dfall$zone.sim)

# Make control impact column
levels(dfall$zone.sim)
dfall$CvI <- ifelse(dfall$zone.sim=="HPZ", "Impact", "Control")
head(dfall)


#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "HPZ-seag-epower.csv", sep='-'), sep='/'))


