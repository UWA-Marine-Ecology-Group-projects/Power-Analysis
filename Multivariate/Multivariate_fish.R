### Multivariate analysis of the fish communities ###

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

# clear environment ---
rm(list=ls()) #clear memory

# Study name----

study <- "stereo-BRUVs"

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
ccdf <- read.csv(paste(d.dir, "BRUV_coastalclusters.csv", sep ='/'))
head(ccdf) 
str(ccdf) # 145 Samples
ccdf$clust <- as.factor(ccdf$clust)

# plot --
plot(gb)
plot(cc, col = cc$clust, pch=20, add=T)
names(cc)


# read BRUV data for fish ----
dir(d.dir)

f <- read.csv(paste(d.dir, "2014-12_Geographe.Bay_stereoBRUVs.checked.maxn.csv", sep='/'))
head(f)

# make column with full names of sp
f$fishfullname <- paste(f$family, f$genus, f$species, sep='-')
head(f)
str(f)
f$fishfullname <- as.factor(f$fishfullname)
str(f) # 158 fish species 322 samples

# make df into wide format ----
library(reshape2)
fwide <- dcast(f, sample ~ fishfullname, value.var="maxn")
head(fwide)
str(fwide)

# replace NAs with Zeros
fwide[is.na(fwide)] <- 0
head(fwide)
str(fwide)

# rename sample column in one df so it is the same
names(fwide)[names(fwide) == "sample"] <- "Sample"

# match sample to cluster so each sample has a cluster number

# extract cluster and sample info --
clusters <- ccdf[,c(2,20)]
head(clusters)
str(clusters)
# remove duplicate obs --
duplicated(clusters)
clusters[duplicated(clusters),]
c <- clusters[!duplicated(clusters),]
str(c)
# merge fwide with c --
fclust <- merge(fwide, c, by = "Sample")
names(fclust)
str(fclust) # 135 obs

# Save fish data by clusters ----
#write.csv(fclust, paste(d.dir, "GB_fish_cluster.csv", sep='/'))


####    Multivariate    ----

# Load fish data ----
f <- read.csv(paste(d.dir, "GB_fish_cluster.csv", sep='/'))

# Bray Curtis ----
names(f)
str(f)

# standarization --
s.std <- decostand(f[-c(1,2,161)], method = 'total')

# bray curtis --
f.distb <- vegdist(s.std, method = "bray", diag=FALSE, upper=FALSE, na.rm = FALSE)
f.distb

# gower --
f.distg <- vegdist(s.std, method = "altGower", diag=FALSE, upper=FALSE, na.rm = FALSE)
f.distg

# NMDS bray --
fb.mds <- metaMDS(f.distb, distance="bray", k=2, trymax = 50, trace=FALSE, plot =T)
fb.mds

# NMDS gower --
fg.mds <- metaMDS(f.distg, distance="altGower", k=2, trymax = 50, trace=FALSE, plot =T)
fg.mds


# plot w ggplot - Bray ----

# pull points from MDS--
nmdsb1 <- fb.mds$points[,1]
nmdsb2 <- fb.mds$points[,2]
fb.plot<-cbind(f[-c(1,161)], nmdsb1, nmdsb2, cluster= f[,161])
fb.plot
fb.plot$cluster <- as.factor(fb.plot$cluster)

# plot ordination
pb<-ggplot(fb.plot, aes(nmdsb1, nmdsb2, color=cluster))+
  geom_point(position=position_jitter(.1), shape=16)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
pb


# plot w ggplot - Gower ----

# pull points from MDS--
nmdsg1 <- fg.mds$points[,1]
nmdsg2 <- fg.mds$points[,2]
fg.plot<-cbind(f[-c(1,161)], nmdsg1, nmdsg2, cluster= f[,161])
fg.plot
fg.plot$cluster <- as.factor(fg.plot$cluster)

# plot ordination
pg<-ggplot(fg.plot, aes(nmdsg1, nmdsg2, color=cluster))+
  geom_point(position=position_jitter(.1), shape=16)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
pg


a1 <- c()
