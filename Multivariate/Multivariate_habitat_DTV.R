### Multivariate analysis of the benthic communities ###

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials

## libraries ----
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
library(sp)
library(raster)
library(rgdal)
library(pdist)
library(fields)
library(rgeos)
library(vegan)
library(FactoMineR)
library(factoextra)
library(tidyverse)
#devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan) # have to fix  Rtools before I can install this package
library(githubinstall)
library(RColorBrewer)
library(colorspace)

# clear environment ---
rm(list=ls()) #clear memory


# Study name----

study <- "DTV"

# Set work directory----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----
m.dir <- "Y:/Power-Analysis"
d.dir <- paste(w.dir,"data",sep="/")
p.dir <- paste(w.dir,"plots",sep="/")
s.dir <- "G:/My Drive/Anita/Shapefiles" 



# read gb polygon ----
gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)

# CLUSTER the transects ----

# read DTV data ----
dir(d.dir)
dtv <- read.csv(paste(d.dir, "DTV_detailed_habitat_percent.cover.csv", sep='/'))
dtv <- read.csv(paste(d.dir, "Dtv_CMR.csv", sep='/'))

head(dtv)
str(dtv)
dtv$Transect.id <- as.factor(dtv$Transect.id)
levels(dtv$Transect.id)
dtv$Zone <- as.factor(dtv$Zone)
levels(dtv$Zone)

# make spatial points --
dtvs <- dtv 
coordinates(dtvs) <- ~Longitude+Latitude
plot(gb)
points(dtvs, col = dtvs$Transect.id)
plot(gb)
# check where the transects are
length(levels(dtv$Transect.id))
levels(dtv$Transect.id)
points(dtvs[dtvs$Transect.id == "SPZ.1",]) # the coordinates of SPZ 2, 7 and 11 are wrong..

## make new clusters manually ----

cluster <- factor(c("NPZclust", "HPZclust", "shallow1", "shallow2", "shallow3", "deep", "mid1", "mid2")) # create factor
lt <- levels(dtvs$Transect.id)
lt

cluster <- c("HPZclust","HPZclust","HPZclust","HPZclust","HPZclust","HPZclust","HPZclust","HPZclust","HPZclust",
             "mid1", "mid2", "shallow2", "mid1" , "mid1",  "mid2",  "mid2", 
             "shallow2" , "shallow1" , "shallow1",  "shallow1",  
             "NPZclust","NPZclust","NPZclust","NPZclust","NPZclust","NPZclust","NPZclust","NPZclust","NPZclust",  
             "deep" , "mid2", "mid2", "shallow2", "deep" , "deep" , "mid2" , "shallow3" , "shallow3",
             "shallow3" , "shallow3",  "shallow3")


clusterf <- as.data.frame(cbind(lt, cluster))
class(clusterf)
head(clusterf)
names(dtv)
names(clusterf) <- c("Transect.id"  ,    "cluster")
names(clusterf)

dtv2 <- merge(dtv, clusterf, by = "Transect.id")
head(dtv2)
str(dtv2)
dtv2$cluster <- as.factor(dtv2$cluster)

dtv2s <- dtv2
coordinates(dtv2s) <- ~Longitude+Latitude

plot(gb)
points(dtv2s, col = dtv2s$cluster)

# save new df
#write.csv(dtv2, paste(d.dir, "GB_hab_DTV_cluster.csv", sep ='/'))


# better map ----

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

plot(gb@polygons[1])
plot(gb[1,])
plot(gb[2,])
plot(gb[4,])
MP <- (gb[3,])
HPZ <-(gb[1,])
GB <- (gb[4,])
plot(HPZ)


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
  geom_point(aes(x=Longitude, y=Latitude, color = cluster), data=dtv2, alpha=1, size=3, color="grey20")+ # to get outline
  geom_point(aes(x=Longitude, y=Latitude, color = cluster), data=dtv2, alpha=1, size=2) +
  scale_colour_manual("DTV transect clusters", values = bluepal(8), guide=guide_legend(nrow=3, title.position = "top")) +  # change color scale
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_text(face="bold")) +
  xlab("Latitude") + ylab("Longitude") 
print(p0)



# read clusters ----
hab <- read.csv(paste(d.dir, "GB_hab_DTV_cluster.csv", sep ='/'))

str(hab)
head(hab)

# test for NAs --
any(is.na(hab))

# join classes if needed ----
names(hab)
# remove epiphytes --
hab <- hab[,-c(6,8,10)]
names(hab)
# make new class of strap-like leaves that does not include posidonia, amphobolis or zostera --
hab$strap.like <- hab$total.seagrass-(hab$Amphibolis+hab$Posidonia)
# make new class of macroalgae leaves that does not include turf, branching or large canopy --
hab$macroalgaeother <- hab$total.Macroalgae-(hab$Turf.algae+hab$Erect.coarse.branching+hab$Erect.fine.branching)
# remove macroalgae, stralike.leaves and large canopy
names(hab)
hab <- hab[,-c(5,8)]
names(hab)


# save new df
#write.csv(hab, paste(d.dir, "GB_hab_DTV_cluster_multivar.csv", sep ='/'))






####    Multivariate    ----

# Load hab data ----

f <- read.csv(paste(d.dir, "GB_hab_DTV_cluster_multivar.csv", sep ='/'))


# Euclidian ----
names(f)
str(f)
head(f)

hab <- f[,c(6:13, 19, 20)]

# euclidean distance --
f.diste <- vegdist(hab, method = "euclidean", diag=FALSE, upper=FALSE, na.rm = FALSE)
f.diste
# NMDS --
fb.mds <- metaMDS(f.diste,  k=2, trymax = 20, trace=FALSE, plot =T)
fb.mds

# plot w ggplot ----

# pull points from MDS--
nmdsb1 <- fb.mds$points[,1]
nmdsb2 <- fb.mds$points[,2]
fb.plot<-cbind(f[-c(1:5,16)], nmdsb1, nmdsb2, cluster= f[,16])
fb.plot
fb.plot$cluster <- as.factor(fb.plot$cluster)

#hpca2 <- PCA(f[,-c(1:5)], quali.sup= 11, graph = T)
#hpca <- PCA(f.diste, graph = T)# plot ordination

hpca2 <- PCA(f[,c(6:13, 18, 19, 20)], quali.sup= 9, graph = T)


bluepal <- choose_palette()


fviz_pca_ind(hpca2,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$cluster, # color by groups
             #palette = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
                         #"#0000CC", "#FF66CC", "#660066", "#00FFFF", "#FFFF00", "#996666"),
             #palette = c( "#FF00CC", "#FF0000", "#00CC00", "#0000FF", "#000000", "#33FFFF","#99FF00","#FFFF00"),
             palette=bluepal(8),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)




theme_set(theme_bw())
pb1<-ggplot(fb.plot, aes(nmdsb1, nmdsb2, color=cluster))+
  geom_point(position=position_jitter(.1), shape=18)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  #scale_colour_manual(values = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
  #                              "#0000CC", "#FF66CC", "#660066", "#00FFFF"))+
  labs(x ="Dim1 (18.7%)", y="Dim2 (14.6%)") +
  scale_color_manual(values = bluepal(12)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14, face='bold'),
        axis.title = element_render(size = 14), axis.text = element_text(size=12)) 

pb1


# strap like leaves + Posidonia --
# Euclidian ----
names(f)
str(f)
head(f)
levels(f$campaignid)


# add strap like leves to Posidonia --
f$Posidonia <- f$Posidonia + f$strap.like
names(f)
# remove strap like --
f <- f[,-19]
names(f)

hab2 <-f[,c(6:13, 19)]
hab2

# euclidean distance --
f.diste <- vegdist(hab2, method = "euclidean", diag=FALSE, upper=FALSE, na.rm = FALSE)
f.diste
# NMDS --
fb.mds <- metaMDS(f.diste, k=2, trymax = 50, trace=FALSE, plot =T)
fb.mds

# plot w ggplot ----

# pull points from MDS--
nmdsb1 <- fb.mds$points[,1]
nmdsb2 <- fb.mds$points[,2]
fb.plot<-cbind(f[-c(1:5,16)], nmdsb1, nmdsb2, cluster= f[,16])
fb.plot
fb.plot$cluster <- as.factor(fb.plot$cluster)


hpca <- PCA(f[,c(6:13, 18, 19)], quali.sup= 9, graph = T)


fviz_pca_ind(hpca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$cluster, # color by groups
             #palette = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
              #           "#0000CC", "#FF66CC", "#660066", "#00FFFF", "#FFFF00", "#996666"),
             palette = c("#FF00CC", "#FF0000", "#00CC00", "#0000FF", "#000000", "#33FFFF","#99FF00","#FFFF00"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)




pb<-ggplot(fb.plot, aes(nmdsb1, nmdsb2, color=cluster))+
  geom_point(position=position_jitter(.1), shape=16)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  scale_colour_manual(values = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
                                 "#0000CC", "#FF66CC", "#660066", "#00FFFF"))+
  theme_minimal()
pb
