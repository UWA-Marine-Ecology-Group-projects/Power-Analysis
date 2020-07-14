

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

study <- "AUV"

# Set work directory----
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
raw.dir = paste(w.dir,"Data/raw",sep="/")
tidy.dir = paste(w.dir,"Data/tidy",sep="/")


#### Raw to PA ----
dir(raw.dir)
## read data --
habitat <- read.csv(paste(raw.dir, "AUV_PointTags.csv", sep ='/')) %>%
  dplyr::select(Image.Name,Image.Source.Dataset,Point.X..from.top.left.corner., Point.Y..from.top.left.corner.,Display.Name, Display.Code) %>% # Select columns to keep
  mutate(Image.Name=str_replace_all(Image.Name,c("https://uwa-auv.s3-ap-southeast-2.amazonaws.com/GeographeBay052015/"="",".jpg"=""))) %>% # Remove url from sample names
  dplyr::rename(Grid = Image.Source.Dataset, habitat = Display.Name, point.x = Point.X..from.top.left.corner., point.y = Point.Y..from.top.left.corner.)%>%
  tidyr::separate(Image.Name, into = c("campaignid", "gtif","Sample"),sep="/")%>%
  dplyr::filter(!habitat%in%c(NA, "")) %>%
  #rename(Sample=Image.Name)%>%
  dplyr::mutate(habitat=paste("hab:",habitat))%>%
  glimpse()

str(habitat)
head(habitat)
habitat$campaignid <- "2014_TowedVideo"

## Make new column for presence-absence of seagrass --
habitat[substr(habitat$Display.Code,1,4)=="SEAG", "Seagrass"] <- "1"
head(habitat)

# Replace Nas with zeros --
habitat$Seagrass[is.na(habitat$Seagrass)] <- 0
head(habitat)


#### Now combine with metadata ----
setwd(raw.dir)
metadata <-list.files(path=raw.dir,pattern="2015_AUV_METADATA")%>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~read_csv(.,col_types = cols(.default = "c")))%>%
  dplyr::filter(!is.na(latitude))%>%
  dplyr::rename(campaignid=dataset_name)%>%
  dplyr::rename(Sample=image_name)%>%
  dplyr::select(campaignid,Sample,latitude,longitude)%>%
  dplyr::mutate(Sample=str_replace_all(.$Sample,c(".png"="")))%>%
  mutate(Sample=as.character(Sample))%>%
  mutate(campaignid=as.character(campaignid))%>%
  glimpse()



# Test to see habitat that doesn't have metadata entry ----
missing.metadata <- anti_join(habitat,metadata) # no images missing metadata 

# Test to see metadata entry that doesn't have habitat ----
missing.habitat <- anti_join(metadata,habitat) # no missing habitat

# Test to see number of points
non.unique<-habitat%>%
  dplyr::group_by(campaignid,Sample)%>%
  dplyr::summarise(n=n())%>%
  filter(n>20) # shows images with more than 20 measurements - this is because of forwards and backwards for 2014



#### Join metadata with point habitat data ----

head(habitat)
head(metadata)


hab.points <- merge(habitat, metadata[,c(2:4)], by="Sample")


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

hab <- as.data.frame(hsp)
head(hab)

# combine habitat and zone dfs --
hab.points.zone <- cbind(hab, points.zone)
head(hab.points.zone)

#### Save presence absence data ----


write.csv(hab.points.zone, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))


