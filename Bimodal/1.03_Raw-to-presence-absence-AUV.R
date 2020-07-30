

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

## Get fata ready for epower ----

df <- hab.points.zone

# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 25340 obs



#### Save presence absence data ----


write.csv(hab.points.zone, paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))



## Get data ready for epower ----

### NPZ ####

hab.points.zone <- read.csv(paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))
str(hab.points.zone)


df <- hab.points.zone

# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 60464 obs

# Remove unwanted columns
names(df)
head(df)
df <- df[, c(3,4,6,11,19)]
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneName)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
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

# joing grid 02 with 02-exp ---

# rename to combine AUV 2 grids
levels(dfn$campaignid)[levels(dfn$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"

dfn <- droplevels(dfn)
levels(dfn$campaignid)
summary(dfn)

# Remove grid 6 from MUZ to make a balanced design - 2 grids per zone
dfn <- dfn[dfn$campaignid!="r20150527_034110_wa-gb-auv-06",]
dfn <- droplevels(dfn)
levels(dfn$campaignid)

#### Make a replicate df to dupicate and rename NPZ to acheive a balanced design ###
dfnNPZ2 <- dfn
str(dfnNPZ2)
levels(dfnNPZ2$campaignid)[levels(dfnNPZ2$campaignid)=="r20150527_055625_wa-gb-auv-05"] <- "DuplicateNPZ"
head(dfnNPZ2)
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_024446_wa-gb-auv-01",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_035926_wa-gb-auv-02",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_050219_wa-gb-auv-03",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150527_072129_wa-gb-auv-04",]

dfnNPZ2 <- droplevels(dfnNPZ2)
str(dfnNPZ2)

# Change the names of the AUV images
names(dfnNPZ2)
dfnNPZ2$Sample <- sub("^", "Dup", dfnNPZ2$Sample) # the ^ represents the point just before the first character
head(dfnNPZ2)

# join with other data --
dfn <- rbind(dfn, dfnNPZ2)
summary(dfn)

#### T1 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres2 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, sum)
sg.pres2

no.scored2 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, length)           
no.scored2
names(no.scored2) <- c("Transect", "ZoneName", "no.scored")

df2<- cbind(sg.pres2, no.scored2[,3])
df2
names(df2) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres3 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, sum)
sg.pres3

no.scored3 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, length)           
no.scored3
names(no.scored3) <- c("Transect", "ZoneName", "no.scored")

df3<- cbind(sg.pres3, no.scored3[,3])
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

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="National Park Zone", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "NPZ-seag-epower.csv", sep='-'), sep='/'))



### HPZ ####

hab.points.zone <- read.csv(paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))
str(hab.points.zone)


df <- hab.points.zone

# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 60464 obs

# Remove unwanted columns
names(df)
head(df)
df <- df[, c(3,4,6,11,19)]
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneName)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
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

# joing grid 02 with 02-exp ---

# rename to combine AUV 2 grids
levels(dfn$campaignid)[levels(dfn$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"

dfn <- droplevels(dfn)
levels(dfn$campaignid)
summary(dfn)

# Remove grid 6 from MUZ to make a balanced design - 2 grids per zone
dfn <- dfn[dfn$campaignid!="r20150527_034110_wa-gb-auv-06",]
dfn <- droplevels(dfn)
levels(dfn$campaignid)

#### Make a replicate df to dupicate and rename NPZ to acheive a balanced design ###
dfnNPZ2 <- dfn
str(dfnNPZ2)
levels(dfnNPZ2$campaignid)[levels(dfnNPZ2$campaignid)=="r20150527_055625_wa-gb-auv-05"] <- "DuplicateNPZ"
head(dfnNPZ2)
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_024446_wa-gb-auv-01",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_035926_wa-gb-auv-02",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_050219_wa-gb-auv-03",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150527_072129_wa-gb-auv-04",]

dfnNPZ2 <- droplevels(dfnNPZ2)
str(dfnNPZ2)

# Change the names of the AUV images
names(dfnNPZ2)
dfnNPZ2$Sample <- sub("^", "Dup", dfnNPZ2$Sample) # the ^ represents the point just before the first character
head(dfnNPZ2)

# join with other data --
dfn <- rbind(dfn, dfnNPZ2)
summary(dfn)

#### T1 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres2 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, sum)
sg.pres2

no.scored2 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, length)           
no.scored2
names(no.scored2) <- c("Transect", "ZoneName", "no.scored")

df2<- cbind(sg.pres2, no.scored2[,3])
df2
names(df2) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres3 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, sum)
sg.pres3

no.scored3 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, length)           
no.scored3
names(no.scored3) <- c("Transect", "ZoneName", "no.scored")

df3<- cbind(sg.pres3, no.scored3[,3])
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

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Habitat Protection Zone", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "HPZ-seag-epower.csv", sep='-'), sep='/'))



### MUZ ####

hab.points.zone <- read.csv(paste(tidy.dir, paste(study, "seag-pres-abs.csv", sep='-'), sep='/'))
str(hab.points.zone)


df <- hab.points.zone

# Remove NA's -  in the zone column --
#df <- na.omit(df)
df <- df %>% drop_na(ZoneName)
str(df)# 60464 obs

# Remove unwanted columns
names(df)
head(df)
df <- df[, c(3,4,6,11,19)]
names(df)
head(df)

### split the data by zone name--
dfz <- split(df, df$ZoneName)
str(dfz)
# remove special purpose zone
dfn <-  dfz[-4]
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

# joing grid 02 with 02-exp ---

# rename to combine AUV 2 grids
levels(dfn$campaignid)[levels(dfn$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"

dfn <- droplevels(dfn)
levels(dfn$campaignid)
summary(dfn)

# Remove grid 6 from MUZ to make a balanced design - 2 grids per zone
dfn <- dfn[dfn$campaignid!="r20150527_034110_wa-gb-auv-06",]
dfn <- droplevels(dfn)
levels(dfn$campaignid)

#### Make a replicate df to dupicate and rename NPZ to acheive a balanced design ###
dfnNPZ2 <- dfn
str(dfnNPZ2)
levels(dfnNPZ2$campaignid)[levels(dfnNPZ2$campaignid)=="r20150527_055625_wa-gb-auv-05"] <- "DuplicateNPZ"
head(dfnNPZ2)
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_024446_wa-gb-auv-01",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_035926_wa-gb-auv-02",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150526_050219_wa-gb-auv-03",]
dfnNPZ2 <- dfnNPZ2[dfnNPZ2$campaignid!="r20150527_072129_wa-gb-auv-04",]

dfnNPZ2 <- droplevels(dfnNPZ2)
str(dfnNPZ2)

# Change the names of the AUV images
names(dfnNPZ2)
dfnNPZ2$Sample <- sub("^", "Dup", dfnNPZ2$Sample) # the ^ represents the point just before the first character
head(dfnNPZ2)

# join with other data --
dfn <- rbind(dfn, dfnNPZ2)
summary(dfn)

#### T1 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn1 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn1)
dfn1 <- droplevels(dfn1)
str(dfn1) # 11250 obs

## calculate presences and no. scored ----

sg.pres1 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, sum)
sg.pres1

no.scored1 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn1, length)           
no.scored1
names(no.scored1) <- c("Transect", "ZoneName", "no.scored")

df1 <- cbind(sg.pres1, no.scored1[,3])
df1
names(df1) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df1$Time <- "T1"
df1

#### T2 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn2 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn2)
dfn2 <- droplevels(dfn2)
str(dfn2) # 11250 obs

## calculate presences and no. scored ----

sg.pres2 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, sum)
sg.pres2

no.scored2 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn2, length)           
no.scored2
names(no.scored2) <- c("Transect", "ZoneName", "no.scored")

df2<- cbind(sg.pres2, no.scored2[,3])
df2
names(df2) <- c("Transect",        "ZoneName",       "Seagrass",    "no.scored")
df2$Time <- "T2"
df2

#### T3 ####

#subsample dat to n=2500( 100 images/25 points per transect ) to improve speed

dfn3 <-as.data.frame(dfn %>% group_by(campaignid) %>% sample_n(size = 2500))
str(dfn3)
dfn3 <- droplevels(dfn3)
str(dfn3) # 11250 obs

## calculate presences and no. scored ----

sg.pres3 <- aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, sum)
sg.pres3

no.scored3 <-  aggregate(Seagrass ~ campaignid + ZoneName, data = dfn3, length)           
no.scored3
names(no.scored3) <- c("Transect", "ZoneName", "no.scored")

df3<- cbind(sg.pres3, no.scored3[,3])
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

# Make control impact column
levels(dfall$ZoneName)
dfall$CvI <- ifelse(dfall$ZoneName=="Multiple Use Zone", "Impact", "Control")
head(dfall)

#### Save data for epower ----

write.csv(dfall, paste(tidy.dir, paste(study, "MUZ-seag-epower.csv", sep='-'), sep='/'))

