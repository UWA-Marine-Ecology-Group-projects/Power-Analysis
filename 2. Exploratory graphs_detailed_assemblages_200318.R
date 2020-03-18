
### Script to plot habitat data for each survey method ####



library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(tidyverse)
library(dplyr)

## Clear workspace ----
rm(list = ls())

# Set working directory ####
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
# Set data directory - to read the data from
d.dir <- paste(w.dir, "Data", "tidy", sep='/')
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')


#### BRUVS ####

### Load data ----

## Set file name --
filen <- "Bruv2014_detailed_zoning.csv"

# Load data
df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)


# Use only the columns needed for seagrass ----
#sg <- df[,c(2:3,8,12:15,20:21)]
#str(sg)

# Remove NAs -- images from state waters
df <- na.omit(df)
str(df)
names(df)


# Compute statistics ----


# list classes:



dfmean <- aggregate(. ~ZoneName, data=df, mean, na.rm=T)
dfmean

dfsd <- aggregate(. ~ZoneName, data=df, sd, na.rm=T)
dfsd

#dfn <- aggregate(. ~ZoneName, data=df, length, na.rm=T)
#dfn

se <- function(x) sqrt(var(x)/length(x))

dfse <- aggregate(. ~ZoneName, data=df, se)
dfse

#### UP TO HERE ###

datas <- cbind(dfmean, dfsd$Seagrasses, sgse$Seagrasses)

datas
str(datas)
names <- c("Zone","mean", "sd", "se")
names(datas) <- names
names(datas)
datas$Zone

# Rename one of the zones
levels(datas$Zone)[levels(datas$Zone)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
datas$Zone

### Plot : Bruv Mean habitat per zone ----

## Set colors for plotting --
# Need 4 colours, one for each zone:
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
blue <- brocolors("crayons")["Turquoise Blue"] # "#77dde7"
green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"
red <- brocolors("crayons")["Wild Watermelon"] # "#fc6c85" 
yellow <- brocolors("crayons")["Sunglow"] # "#ffcf48"

theme_set(theme_bw())
p<-ggplot(data=datas, aes(x=Zone, y=mean, fill = Zone)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  labs(title = "Stereo-BRUVs", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold"))
p

ggsave(paste(p.dir, "Sg-bruvs-zones.png", sep='/'), plot=p, device = "png", scale = 1, dpi =300 )



#### TOWED VIDEO ####

### Load data ----

## Set file name --
filen <- "TV_zoning.csv"

# Load data
df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)


# Use only the columns needed for seagrass ----
sg <- df[,c(2,6,11:14,19,20)]
str(sg)

# Remove NAs --
sg <- na.omit(sg)
str(sg)


# Compute statistics ----


sgmean <- aggregate(data=sg, Seagrasses~ZoneName, FUN=mean)
sgmean

sgsd <- aggregate(data=sg, Seagrasses~ZoneName, FUN=sd)
sgsd

sgn <- aggregate(data=sg, Seagrasses~ZoneName, FUN=length)
sgn

se <- function(x) sqrt(var(x)/length(x))

sgse <- aggregate(data=sg, Seagrasses~ZoneName, FUN=se)
sgse

datas <- cbind(sgmean, sgsd$Seagrasses, sgse$Seagrasses)

datas
str(datas)
names <- c("Zone","mean", "sd", "se")
names(datas) <- names
names(datas)
datas$Zone

# Rename one of the zones
levels(datas$Zone)[levels(datas$Zone)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
datas$Zone

### Plot : TV Mean habitat per zone ----

## Set colors for plotting --
# Need 4 colours, one for each zone:
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
blue <- brocolors("crayons")["Turquoise Blue"] # "#77dde7"
green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"
red <- brocolors("crayons")["Wild Watermelon"] # "#fc6c85" 
yellow <- brocolors("crayons")["Sunglow"] # "#ffcf48"


## Plot--

theme_set(theme_bw())
p2<-ggplot(data=datas, aes(x=Zone, y=mean, fill = Zone)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#fc6c85", "#ffcf48")) +
  labs(title = "Towed Video", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold")) 
p2

ggsave(paste(p.dir, "Sg-tv-zones.png", sep='/'), plot=p2, device = "png", scale = 1, dpi =300 )


### Transect plots ----

library(stringr)

str(sg)

## Get the transect code form Sample number --
sg$Transect <- str_sub(sg$sample, 1,6)
sg$Transect <- as.factor(sg$Transect)
levels(sg$Transect)
str(sg)

sgmean <- aggregate(data=sg, Seagrasses~Transect, FUN=mean)
sgmean

sgsd <- aggregate(data=sg, Seagrasses~Transect~Transect, FUN=sd)
sgsd

sgn <- aggregate(data=sg, Seagrasses~Transect~Transect, FUN=length)
sgn

#se <- function(x) sqrt(var(x)/length(x))

sgse <- aggregate(data=sg, Seagrasses~Transect~Transect, FUN=se)
sgse

dataSites <- cbind(sgmean, sgsd$Seagrasses, sgse$Seagrasses)

dataSites
str(dataSites)
names <- c("Transect","mean", "sd", "se")
names(dataSites) <- names
names(dataSites)


#### TV Plots ----

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
#blue <- brocolors("crayons")["Navy Blue"] # "#1974d2"
#green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

# Site names


levels(dataSites$Transect)

#sitenames <- c("LGBDT2", "Habitat Protection Zone", "LGBDT4", "LGBDT5", "LGBDT6","LGBDT7")


#levels(dataSites$Sites) <- sitenames


## Plot

theme_set(theme_bw())
p3<-ggplot(data=dataSites, aes(x=Transect, y=mean, fill = Transect)) +
  #geom_rect(xmin = as.numeric(dataSites$Sites[[2]])-0.5, ymin = 0, xmax= as.numeric(dataSites$Sites[[2]])+0.5, ymax=90, fill = "grey70")+
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex =1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color ="grey50") +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Towed Video transects", y = "Seagrass % cover", x = "Sites") +
  #scale_fill_manual(values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12, face = "bold"), axis.text = element_text(size=12), title = element_text(size=14, face="bold"),
        legend.position = "none")
p3

ggsave(paste(p.dir, "Sg-tv-transects.png", sep='/'), plot=p3, device = "png", scale = 1, dpi =300 )








## AUV ----

# Set working directory ####
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
# Set data directory - to read the data from
d.dir <- paste(w.dir, "Data", "tidy", sep='/')
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')

### Load data ----

## Set file name --
filen <- "Auv_zoning.csv"

# Load data
df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)


# Control versus Impact plots ----
CImean <- aggregate(data=df, Seagrasses~ZoneName, FUN=mean)
CImean

CIsd <- aggregate(data=df, Seagrasses~ZoneName, FUN=sd)
CIsd

CIn <- aggregate(data=df, Seagrasses~ZoneName, FUN=length)
CIn

#se <- function(x) sqrt(var(x)/length(x))

CIse <- aggregate(data=df, Seagrasses~ZoneName, FUN=se)
CIse

datas <- cbind(CImean, CIsd$Seagrasses, CIse$Seagrasses)

datas
str(datas)
names <- c("ZoneName","mean", "sd", "se")
names(datas) <- names
names(datas)

# Rename one of the zones
levels(datas$Zone)[levels(datas$Zone)=="Special Purpose Zone (Mining Exclusion)"] <- "Special Purpose Zone"
datas$Zone

#datas$semin <- datas$mean - datas$se
#datas$semax <- datas$mean + datas$se

library(ggplot2)



# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
blue <- brocolors("crayons")["Turquoise Blue"] # "#77dde7"
green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"
red <- brocolors("crayons")["Wild Watermelon"] # "#fc6c85" 
yellow <- brocolors("crayons")["Sunglow"] # "#ffcf48"


theme_set(theme_bw())
p2<-ggplot(data=datas, aes(x=ZoneName, y=mean, fill = ZoneName)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  labs(title = "AUV", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold"))
p2

ggsave(paste(p.dir, "sg_Auv_zones.png", sep='/'), plot=p2, device = "png", scale = 1, dpi =300 )

### AUV grid plots ----

# check the different AUV grids
levels(df$campaignid)

# wa-gb-auv-02 and wa-gb-auv-02-exp are the same, join them
levels(df$campaignid)[levels(df$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"
levels(df$campaignid) # 13 grids in total

str(df)

Smean <- aggregate(data=df, Seagrasses~campaignid, FUN=mean)
Smean

Ssd <- aggregate(data=df, Seagrasses~campaignid, FUN=sd)
Ssd

Sn <- aggregate(data=df, Seagrasses~campaignid, FUN=length)
Sn

#se <- function(x) sqrt(var(x)/length(x))

Sse <- aggregate(data=df, Seagrasses~campaignid, FUN=se)
Sse

dataSites <- cbind(Smean, Ssd$Seagrasses, Sse$Seagrasses)

dataSites
str(dataSites)
names <- c("Site","mean", "sd", "se")
names(dataSites) <- names
names(dataSites)

# change the names of the sites
levels(dataSites$Site)
sitenames <- c("AUV1", "AUV2", "AUV3", "AUV6", "AUV5", "AUV4", "AUV10", "AUV9", "AUV11", "AUV13", "AUV12", "AUV14", "AUV15")
levels(dataSites$Site) <- sitenames
levels(dataSites$Site)

# reorder levels
print(levels(dataSites$Site))
dataSites$Site = factor(dataSites$Site,levels(dataSites$Site)[c(1:3,6,5,4,8,7,9,11,10,12,13)])

### Decide on colours - 13 needed
# colors for grids with sg
# Expand palettes : https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colorCount <- length(dataSites$Site)
getPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))

# Site names
levels(dataSites$Site)

#sitenames <- c("Control1", "Control2", "Control3", "Control5", "Marine Park","Control4")

#levels(dataSites$Sites) <- sitenames


## Plot

theme_set(theme_bw())
p3<-ggplot(data=dataSites, aes(x=Site, y=mean, fill = Site)) +
  #geom_rect(xmin = as.numeric(dataSites$Sites[[5]])-0.5, ymin = 0, xmax= as.numeric(dataSites$Sites[[5]])+0.5, ymax=80, fill = "grey70")+
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color ="grey50") +
  #scale_fill_brewer(palette = "YlGnBu") +
  scale_fill_manual(values = getPalette(colorCount))+
  #scale_fill_manual(values = getPalette(colorCount), "AUV grids", guide=guide_legend(nrow=3, title.position = "top"))
  labs(title = "AUV survey sites", y = "Seagrass % cover", x = "Sites") +
  #scale_fill_manual(values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12, face = "bold"), axis.text = element_text(size=12), title = element_text(size=14, face="bold"),
        legend.position = "none") 
  geom_rect(xmin = as.numeric(dataSites$Sites[[5]])-0.5, ymin = 0, xmax= as.numeric(dataSites$Sites[[5]])+0.5, ymax=80, fill = "grey80")
  geom_rect(data = dataSites, aes(x =mean, fill = sites),
            xmin = dataSites$Sites[5], ymin = 0, xmax= dataSites$Sites[5], ymax=80, fill = "grey80")
 

p3

ggsave(paste(p.dir, "Auv_Sites.png", sep='/'), plot=p3, device = "png", scale = 1, dpi =300 )




### Plot Maps of data ----

### BRUV Maps ----

library(raster)
library(sp)
library(rgdal)
library(plyr)
library(maptools)
library(broom)

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
grey <- brocolors("crayons")["Timberwolf"] # "#dbd7d2"
green <- brocolors("crayons")["Spring Green"] # "#eceabe"
blue <- brocolors("crayons")["Sky Blue"] # "#80daeb"
green2 <- brocolors("crayons")["Sea Green"] # "#93dfb8"

## Read Geo bay shapefile

gb <- readOGR(paste(w.dir, "Data", "shapefiles", "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

## Read Bruv data
sp <- read.csv(paste(d.dir,"Bruvs2014_zoning.csv", sep='/'))
str(sp)

coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"

points(sp, pch = 21, bg = sp$ZoneName)

# back to data frame 
spdf <- as.data.frame(sp)
head(spdf)


## To plot the Geo Bay polygon ---
gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")
class(gb.df)

## plot ---

p0 <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=sp) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c( "#80daeb","#dbd7d2", "#93dfb8", "#eceabe"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - Stereo-BRUVS", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = ZoneName), data=spdf,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = ZoneName), data=spdf,alpha=1, size=2) +
  scale_colour_manual("Stereo-BRUVs", values = c("blue", "red","green",  "yellow"), guide=guide_legend(nrow=3, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p0)

ggsave(paste(p.dir, "Map-all-bruvs-zone.png", sep='/'), plot=p0, scale =1, device = "png", dpi =300)




## Towed Video maps ----

library(raster)
library(sp)
library(rgdal)
library(plyr)
library(maptools)
library(broom)

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
grey <- brocolors("crayons")["Timberwolf"] # "#dbd7d2"
green <- brocolors("crayons")["Spring Green"] # "#eceabe"
blue <- brocolors("crayons")["Sky Blue"] # "#80daeb"
green2 <- brocolors("crayons")["Sea Green"] # "#93dfb8"

## Read Geo bay shapefile

gb <- readOGR(paste(w.dir, "Data", "shapefiles", "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

## Read TV data
sp <- read.csv(paste(d.dir,"Tv_zoning.csv", sep='/'))
str(sp)

str(sp)

## Get the transect code form Sample number --
sp$Transect <- str_sub(sp$sample, 1,6)
sp$Transect <- as.factor(sp$Transect)
levels(sp$Transect)
str(sp)


coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"

spdf <- as.data.frame(sp)

#points(sp, pch = 21, bg = sp$ZoneName)



gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")

# change the names of the sites
levels(sp$Transect)
#sitenames <- c("LGBDT1", "LGBDT2", "LGBDT3" ,"LGBDT4" ,"LGBDT5", "LGBDT6" ,"LGBDT7", "LGBDT8", "LGBDT9")
#levels(sp$Transect) <- sitenames

# reorder levels
print(levels(sp$Transect))
sp$campaignid = factor(sp$campaignid,levels(sp$campaignid)[c(1:3,6,5,4)])    

p7 <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=sp) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c("#80daeb","#dbd7d2", "#93dfb8", "#eceabe"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - Towed Video", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = Transect), data=spdf,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = Transect), data=spdf,alpha=1, size=2) +
  #scale_colour_manual("Stereo-BRUVs", values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1"), guide=guide_legend(nrow=6, title.position = "top")) +  # change color scale
  scale_color_brewer("Transects", palette = "YlGnBu", guide=guide_legend(nrow=6, title.position = "top"))+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p7)

ggsave(paste(p.dir, "Map-all-tv-zone.png", sep='/'), plot=p7, scale =1, device = "png", dpi =300)  







## AUV maps ----

library(raster)
library(sp)
library(rgdal)
library(plyr)
library(maptools)
library(broom)
library(viridis)
library(RColorBrewer)

# Set working directory ####
w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
# Set data directory - to read the data from
d.dir <- paste(w.dir, "Data", "tidy", sep='/')
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')

### Load data ----

## Set file name --
filen <- "Auv_zoning.csv"

# Load data
df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)
any(is.na(df))

# wa-gb-auv-02 and wa-gb-auv-02-exp are the same, join them
levels(df$campaignid)[levels(df$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"
levels(df$campaignid) # 13 grids in total

# change the names of the sites
levels(df$campaignid)
sitenames <- c("AUV1", "AUV2", "AUV3", "AUV6", "AUV5", "AUV4", "AUV10", "AUV9", "AUV11", "AUV13", "AUV12", "AUV14", "AUV15")
levels(df$campaignid) <- sitenames

# reorder levels
print(levels(df$campaignid))
#sp$campaignid = factor(sp$campaignid,levels(sp$campaignid)[c(1:3,6,5,4)])
df$campaignid = factor(sp$campaignid,levels(sp$campaignid)[c(1:3,6,5,4,8,7,9,11,10,12,13)])


sp <- df

coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"


## Read Geo bay shapefile

gb <- readOGR(paste(w.dir, "Data", "shapefiles", "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

#points(sp, pch = 21, bg = sp$ZoneName)

#plot(gb@polygons[1])
#plot(gb[1,])
#plot(gb[2,])
#plot(gb[4,])
#MP <- (gb[3,])
#HPZ <-(gb[1,])
#GB <- (gb[4,])
#plot(HPZ)

#MP@data$id = rownames(MP@data)
#Mp.points = broom::tidy(MP)
#Mp.df = join(Mp.points, MP@data, by="id")

gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")
class(gb.df)



### Decide on colours - 13 needed
# colors for grids with sg
# Expand palettes : https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colorCount <- length(unique(df$campaignid))
getPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))

#display.brewer.pal(n = 8, name = 'YlGnBu')
#pal1 <- brewer.pal(n = 8, name = "YlGnBu")
# colors for grids without sg
#display.brewer.pal(n = 5, name = 'Oranges')
#pal2 <- brewer.pal(n = 5, name = "Oranges")
# Join both palettes 
#pal3 <- c(pal1, pal2)



p6 <- ggplot() +
  geom_polygon(data = gb.df, aes(x = long, y = lat, group = group, fill = ZoneName), color = "black") +
  #geom_point(aes(x=coords.x1, y=coords.x2), data=sp) +
  #geom_path(color = "white", size = 0.2) +
  #scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal(ratio= 1) +
  #xlab("Latitude") + ylab("Longitude") +
  scale_fill_manual("Zones", values = c("#eceabe", "#80daeb", "#93dfb8" , "#dbd7d2"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - AUV", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = campaignid), data=df,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = campaignid), data=df,alpha=1, size=2) +
  scale_color_manual(values = getPalette(colorCount), "AUV grids", guide=guide_legend(nrow=3, title.position = "top")) +
  #scale_color_brewer("AUV grids", palette = pal3, guide=guide_legend(nrow=4, title.position = "top"))+
  #scale_color_viridis(discrete=F,"AUV grids", palette = "YlGnBu", guide=guide_legend(nrow=6, title.position = "top")) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p6)

ggsave(paste(p.dir, "Map_all_auv_zone.png", sep='/'), plot=p6, scale =1, device = "png", dpi =300)  

               
