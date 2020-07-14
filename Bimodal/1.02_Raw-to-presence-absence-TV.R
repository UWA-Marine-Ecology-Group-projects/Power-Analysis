

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

write.csv(hab.points, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))


