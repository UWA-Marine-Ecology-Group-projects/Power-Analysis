

#####  Raw point data into Binary: presence - absence of seagrass ###

# libraries ----
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
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

