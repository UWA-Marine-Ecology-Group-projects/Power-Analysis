install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(doParallel)
library(rJava)
library(XLConnect)
library(epower)

library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png

setwd("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/Desktop")


# set directory

w.directory <- dirname("C:/Users/00093391/Dropbox/UWA/Research Associate/PowAn/Desktop/ ")


# load Bruv data used for power analysis ----
data <- read.csv(paste(w.directory, "Bruvs_sg.csv", sep='/'))
str(data)

CImean <- aggregate(data=data, Total.seagrass~CvI, FUN=mean)
CImean

CIsd <- aggregate(data=data, Total.seagrass~CvI, FUN=sd)
CIsd

CIn <- aggregate(data=data, Total.seagrass~CvI, FUN=length)
CIn

se <- function(x) sqrt(var(x)/length(x))

CIse <- aggregate(data=data, Total.seagrass~CvI, FUN=se)
CIse

datas <- cbind(CImean, CIsd$Total.seagrass, CIse$Total.seagrass)

datas
str(datas)
names <- c("CvI","mean", "sd", "se")
names(datas) <- names
names(datas)

#datas$semin <- datas$mean - datas$se
#datas$semax <- datas$mean + datas$se

library(ggplot2)

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
blue <- brocolors("crayons")["Turquoise Blue"] # "#77dde7"
green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

theme_set(theme_bw())
p<-ggplot(data=datas, aes(x=CvI, y=mean, fill = CvI)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#b2ec5d")) +
  labs(title = "Stereo-BRUVs", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold"))
p

ggsave(paste(w.directory, "bruv_CvI.png", sep='/'), plot=p, device = "png", scale = 1, dpi =300 )

## AUV ----

# load Bruv data used for power analysis ----
data <- read.csv(paste(w.directory, "Auv_sg.csv", sep='/'))
str(data)

# Control versus Impact plots ----
CImean <- aggregate(data=data, Total.seagrass~CvI, FUN=mean)
CImean

CIsd <- aggregate(data=data, Total.seagrass~CvI, FUN=sd)
CIsd

CIn <- aggregate(data=data, Total.seagrass~CvI, FUN=length)
CIn

#se <- function(x) sqrt(var(x)/length(x))

CIse <- aggregate(data=data, Total.seagrass~CvI, FUN=se)
CIse

datas <- cbind(CImean, CIsd$Total.seagrass, CIse$Total.seagrass)

datas
str(datas)
names <- c("CvI","mean", "sd", "se")
names(datas) <- names
names(datas)

#datas$semin <- datas$mean - datas$se
#datas$semax <- datas$mean + datas$se

library(ggplot2)



# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
blue <- brocolors("crayons")["Navy Blue"] # "#1974d2"
green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

theme_set(theme_bw())
p2<-ggplot(data=datas, aes(x=CvI, y=mean, fill = CvI)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#b2ec5d")) +
  labs(title = "AUV", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold"))
p2

ggsave(paste(w.directory, "Auv_CvI.png", sep='/'), plot=p2, device = "png", scale = 1, dpi =300 )

### Site plots ----

str(data)

Smean <- aggregate(data=data, Total.seagrass~Site, FUN=mean)
Smean

Ssd <- aggregate(data=data, Total.seagrass~Site, FUN=sd)
Ssd

Sn <- aggregate(data=data, Total.seagrass~Site, FUN=length)
Sn

#se <- function(x) sqrt(var(x)/length(x))

Sse <- aggregate(data=data, Total.seagrass~Site, FUN=se)
Sse

dataSites <- cbind(Smean, Ssd$Total.seagrass, Sse$Total.seagrass)

dataSites
str(dataSites)
names <- c("Sites","mean", "sd", "se")
names(dataSites) <- names
names(dataSites)

# change the names of the sites
levels(dataSites$Sites)
sitenames <- c("AUV1", "AUV2", "AUV3", "AUV6", "AUV5", "AUV4")
levels(dataSites$Sites) <- sitenames
levels(dataSites$Sites)

# reorder levels
print(levels(dataSites$Sites))
dataSites$Sites = factor(dataSites$Sites,levels(dataSites$Sites)[c(1:3,6,5,4)])

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
#blue <- brocolors("crayons")["Navy Blue"] # "#1974d2"
#green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

# Site names
levels(dataSites$Sites)

#sitenames <- c("Control1", "Control2", "Control3", "Control5", "Marine Park","Control4")

#levels(dataSites$Sites) <- sitenames


## Plot

theme_set(theme_bw())
p3<-ggplot(data=dataSites, aes(x=Sites, y=mean, fill = Sites)) +
  #geom_rect(xmin = as.numeric(dataSites$Sites[[5]])-0.5, ymin = 0, xmax= as.numeric(dataSites$Sites[[5]])+0.5, ymax=80, fill = "grey70")+
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color ="grey50") +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "AUV survey sites", y = "Seagrass % cover", x = "Sites") +
  #scale_fill_manual(values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12, face = "bold"), axis.text = element_text(size=12), title = element_text(size=14, face="bold"),
        legend.position = "none") 
  geom_rect(xmin = as.numeric(dataSites$Sites[[5]])-0.5, ymin = 0, xmax= as.numeric(dataSites$Sites[[5]])+0.5, ymax=80, fill = "grey80")
  geom_rect(data = dataSites, aes(x =mean, fill = sites),
            xmin = dataSites$Sites[5], ymin = 0, xmax= dataSites$Sites[5], ymax=80, fill = "grey80")
 

p3

ggsave(paste(w.directory, "Auv_Sites.png", sep='/'), plot=p3, device = "png", scale = 1, dpi =300 )


## Towed Video ----

# load Tv data used for power analysis ----
data <- read.csv(paste(w.directory, "Tv_sg.csv", sep='/'))
str(data)

# Control versus Impact plots ----
CImean <- aggregate(data=data, Total.seagrass~CvI, FUN=mean)
CImean

CIsd <- aggregate(data=data, Total.seagrass~CvI, FUN=sd)
CIsd

CIn <- aggregate(data=data, Total.seagrass~CvI, FUN=length)
CIn

#se <- function(x) sqrt(var(x)/length(x))

CIse <- aggregate(data=data, Total.seagrass~CvI, FUN=se)
CIse

datas <- cbind(CImean, CIsd$Total.seagrass, CIse$Total.seagrass)

datas
str(datas)
names <- c("CvI","mean", "sd", "se")
names(datas) <- names
names(datas)

#datas$semin <- datas$mean - datas$se
#datas$semax <- datas$mean + datas$se

library(ggplot2)
library(gghighlight)

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
#blue <- brocolors("crayons")["Navy Blue"] # "#1974d2"
#green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

theme_set(theme_bw())
p4<-ggplot(data=datas, aes(x=CvI, y=mean, fill = CvI)) +
  geom_bar(stat="identity", color = "black") +
  geom_errorbar(aes(ymax = mean-se, ymin = mean+se), width = 0.2, cex = 1) +
  geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  scale_fill_manual(values = c("#77dde7", "#b2ec5d")) +
  labs(title = "Towed Video", y = "Seagrass mean % cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size=12, face="bold"),
        title = element_text(size = 14, face= "bold")) 
p4

ggsave(paste(w.directory, "Tv_CvI.png", sep='/'), plot=p4, device = "png", scale = 1, dpi =300 )

### Site plots ----
str(data)

Smean <- aggregate(data=data, Total.seagrass~Transect, FUN=mean)
Smean

Ssd <- aggregate(data=data, Total.seagrass~Transect, FUN=sd)
Ssd

Sn <- aggregate(data=data, Total.seagrass~Transect, FUN=length)
Sn

#se <- function(x) sqrt(var(x)/length(x))

Sse <- aggregate(data=data, Total.seagrass~Transect, FUN=se)
Sse

dataSites <- cbind(Smean, Ssd$Total.seagrass, Sse$Total.seagrass)

dataSites
str(dataSites)
names <- c("Sites","mean", "sd", "se")
names(dataSites) <- names
names(dataSites)

# color# 
# for color list: https://kbroman.files.wordpress.com/2014/05/crayons.png
#blue <- brocolors("crayons")["Navy Blue"] # "#1974d2"
#green <- brocolors("crayons")["Inchworm"] # "#b2ec5d"

# Site names
levels(dataSites$Sites)

#sitenames <- c("LGBDT2", "Habitat Protection Zone", "LGBDT4", "LGBDT5", "LGBDT6","LGBDT7")


#levels(dataSites$Sites) <- sitenames


## Plot

theme_set(theme_bw())
p5<-ggplot(data=dataSites, aes(x=Sites, y=mean, fill = Sites)) +
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
p5

ggsave(paste(w.directory, "Tv_Transects.png", sep='/'), plot=p5, device = "png", scale = 1, dpi =300 )


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

gb <- readOGR(dsn="C:/Users/21933549/Dropbox/UWA/Research Associate/PowAn/Desktop/shapefiles/GeoBay.shp")
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

## Read Bruv data
sp <- read.csv("C:/Users/21933549/Dropbox/UWA/Research Associate/PowAn/DATA/Bruv.csv")
str(sp)

coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"

points(sp, pch = 21, bg = sp$ZoneName)

plot(gb@polygons[1])
plot(gb[1,])
plot(gb[2,])
plot(gb[4,])
MP <- (gb[3,])
HPZ <-(gb[1,])
GB <- (gb[4,])
plot(HPZ)

MP@data$id = rownames(MP@data)
Mp.points = broom::tidy(MP)
utah.df = join(Mp.points, MP@data, by="id")

gb@data$id = rownames(gb@data)
gb.points = broom::tidy(gb)
gb.df = join(gb.points, gb@data, by="id")

p0 <- ggplot() +
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
  labs(title = "Geographe Bay - Stereo-BRUVS", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = ZoneName), data=sp,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = ZoneName), data=sp,alpha=1, size=2) +
  scale_colour_manual("Stereo-BRUVs", values = c("yellow", "blue", "green"), guide=guide_legend(nrow=3, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p0)

ggsave(paste(w.directory, "Map_BRUVS_pa.png", sep='/'), plot=p0, scale =1, device = "png", dpi =300)


## AUV maps ----
sp <- read.csv("C:/Users/21933549/Dropbox/UWA/Research Associate/PowAn/DATA/Auv.csv")
str(sp)

coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"

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

# change the names of the sites
levels(sp$campaignid)
sitenames <- c("AUV1", "AUV2", "AUV3", "AUV6", "AUV5", "AUV4")
levels(sp$campaignid) <- sitenames

# reorder levels
print(levels(sp$campaignid))
sp$campaignid = factor(sp$campaignid,levels(sp$campaignid)[c(1:3,6,5,4)])    

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
  geom_point(aes(x=coords.x1, y=coords.x2, color = campaignid), data=sp,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = campaignid), data=sp,alpha=1, size=2) +
  #scale_colour_manual("Stereo-BRUVs", values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1"), guide=guide_legend(nrow=6, title.position = "top")) +  # change color scale
  scale_color_brewer("Stereo-BRUVs", palette = "YlGnBu", guide=guide_legend(nrow=6, title.position = "top"))+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p6)

ggsave(paste(w.directory, "Map_AUV_pa.png", sep='/'), plot=p6, scale =1, device = "png", dpi =300)  

               
## Towed Video maps ----
sp <- read.csv("C:/Users/21933549/Dropbox/UWA/Research Associate/PowAn/DATA/Tv.csv")
str(sp)

coordinates(sp) <- ~coords.x1+coords.x2
proj4string(sp) <- "+proj=longlat +ellps=GRS80 +no_defs"

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

# change the names of the sites
levels(sp$campaignid)
sitenames <- c("AUV1", "AUV2", "AUV3", "AUV6", "AUV5", "AUV4")
levels(sp$campaignid) <- sitenames

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
  scale_fill_manual("Zones", values = c("#eceabe", "#80daeb", "#93dfb8" , "#dbd7d2"), guide=guide_legend(nrow=4, title.position = 'top'))+
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank()) +
  #theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face="bold")) +
  labs(title = "Geographe Bay - Towed Video", x ="Latitude" , y ="Longitude") +
  xlab("Latitude") + ylab("Longitude") +
  geom_point(aes(x=coords.x1, y=coords.x2, color = Transect), data=sp,alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=coords.x1, y=coords.x2, color = Transect), data=sp,alpha=1, size=2) +
  #scale_colour_manual("Stereo-BRUVs", values = c("yellow", "dodgerblue", "dodgerblue4","red", "forestgreen", "darkorange1"), guide=guide_legend(nrow=6, title.position = "top")) +  # change color scale
  scale_color_brewer("Transects", palette = "YlGnBu", guide=guide_legend(nrow=6, title.position = "top"))+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p7)

ggsave(paste(w.directory, "Map_TV_pa.png", sep='/'), plot=p7, scale =1, device = "png", dpi =300)  


