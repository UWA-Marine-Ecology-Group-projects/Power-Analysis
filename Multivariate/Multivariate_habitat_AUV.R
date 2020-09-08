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

# clear environment ---
rm(list=ls()) #clear memory


# Study name----

study <- "AUV"

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

# read coastal clusters ----

cc <- readOGR("Y:/Power-Analysis/Multivariate/spatial/BRUV_coastalclusters.shp")
cc
length(levels(cc$Sample)) # 145 samples

# read df of coastal clusters ----
# file name --
c <- "GB_fish_cluster.csv"
ccdf <- read.csv(paste(d.dir, "BRUV_coastalclusters.csv", sep ='/'))
ccdf <- read.csv(paste(d.dir, c, sep ='/')) %>%
  dplyr::select(Sample,clust) %>% # Select columns to keep
  dplyr::rename('sample' = 'Sample') %>%
  glimpse()

str(ccdf) # 135 obs
head(ccdf)

# test for NAs --
any(is.na(ccdf))
head(ccdf) 
str(ccdf) # 135 Samples
ccdf$clust <- as.factor(ccdf$clust)
summary(ccdf)
levels(ccdf$clust)

# plot --
plot(gb)
plot(cc, col = cc$clust, pch=20, add=T)
names(cc)


# read BRUV data for fish ----
dir(d.dir)

h <- "auv_detailed.percent.cover.csv"

hab <- read.csv(paste(d.dir, h, sep ='/')) 
  #%>% dplyr::select(sample,maxn,latitude, longitude, depth) %>% # Select columns to keep
  #glimpse()

str(hab) # 1752 obs
head(hab)

# test for NAs --
any(is.na(hab))

# join classes if needed ----
names(hab)
# remove epiphytes --
hab <- hab[,-c(6,8,10)]
names(hab)
# make new class of strap-like leaves that does not include posidonia, amphobolis or zostera --
hab$strap.like <- hab$Strap.like.leaves-(hab$Amphibolis+hab$Posidonia+hab$Zostera)
# make new class of macroalgae leaves that does not include turf, branching or large canopy --
hab$macroalgaeother <- hab$Macroalgae-(hab$Turf.algae+hab$Erect.coarse.branching+hab$Erect.fine.branching+hab$Large.canopy.forming)
# remove macroalgae, stralike.leaves and large canopy
names(hab)
hab <- hab[,-c(5,10,14)]
names(hab)
head(hab)
   

# AUV no clusters but grids ----

str(hab)
levels(hab$campaignid)

# join campaignid 2 and 2-exp
levels(hab$campaignid)[levels(hab$campaignid)=="r20150527_081513_wa-gb-auv-02-exp"] <- "r20150526_035926_wa-gb-auv-02"
levels(hab$campaignid) # 13 levels
str(hab)

# Rename all levels
levels(hab$campaignid) <- c("G1", "G2", "G3", "G6","G5","G4", "G10", "G9", "G11", "G13", "G12", "G14", "G15")
head(hab)

# Save hab data by campaignid/grid ----
#write.csv(hab, paste(d.dir, "GB_hab_AUV_grid.csv", sep='/'))

####    Multivariate    ----

# Load hab data ----

f <- read.csv(paste(d.dir, "GB_hab_AUV_grid.csv", sep='/'))


# Euclidian ----
names(f)
str(f)
head(f)

# remove grids from SPZ --
levels(f$campaignid)
f <- f[f$campaignid!="G9",]
f <- f[f$campaignid!="G10",]
f <- f[f$campaignid!="G11",]
f <- f[f$campaignid!="G12",]
f <- f[f$campaignid!="G13",]
f <- f[f$campaignid!="G14",]
f <- f[f$campaignid!="G15",]
f <- droplevels(f)

hab <- f[,-c(1:5)]
hab

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

hpca2 <- PCA(f[,-c(1,3:5)], quali.sup= 1, graph = T)


fviz_pca_ind(hpca2,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$campaignid, # color by groups
             #palette = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
                         #"#0000CC", "#FF66CC", "#660066", "#00FFFF", "#FFFF00", "#996666"),
             palette = c("#FFFF00", "#FF0000", "#00CC00", "#0000FF", "#000000", "#33FFFF"),
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
f <- f[,-13]
names(f)

hab2 <- f[,-c(1:5)]
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


hpca <- PCA(f[,-c(1,3:5)], quali.sup= 1, graph = T)


fviz_pca_ind(hpca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$campaignid, # color by groups
             #palette = c("#FF0000", "#000000", "#FF9900", "#990000", "#33FF00", "#009933", "#3399FF", 
              #           "#0000CC", "#FF66CC", "#660066", "#00FFFF", "#FFFF00", "#996666"),
             palette = c("#FFFF00", "#FF0000", "#00CC00", "#0000FF", "#000000", "#33FFFF"),
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
