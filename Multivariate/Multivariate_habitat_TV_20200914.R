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

study <- "TV"

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

h <- "towed_detailed.percent.cover_20200914.csv"

hab <- read.csv(paste(d.dir, h, sep ='/')) 
  #%>% dplyr::select(sample,maxn,latitude, longitude, depth) %>% # Select columns to keep
  #glimpse()

str(hab) # 2962 obs
head(hab)

# test for NAs --
any(is.na(hab))

# remove data outside of CMR ----
crs1 <- "+proj=longlat +ellps=GRS80 +no_defs"

habs <- hab
coordinates(habs) <- ~ longitude + latitude
points(habs)

proj4string(habs) <- crs1


habs2 <- raster::extract(gb, habs, df=T)
head(habs2)
str(habs2) # 2963 obs
head(hab)
# habs2 has a replicated row, remove it 
habs3 <- habs2[-2509,]

# join both dfs
hab3 <- cbind(hab, habs3)
str(hab3) # 2962 obs.

# remove obs not in CMR ----
# rows with NAs in Zone name --
head(hab3)

# set function 
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# run function
hab4 <- completeFun(hab3, 'ZoneName')
head(hab4)
str(hab4) # 1141 obs

# check
hab4s <- hab4
coordinates(hab4s) <- ~ longitude + latitude
plot(gb)
points(hab4s)

# remove unwanted columns
names(hab4)
hab5 <- hab4[,-c(16:26)]
names(hab5)

# join classes if needed ----
names(hab5)
# remove epiphytes --
hab5 <- hab5[,-c(5,7,9)]
names(hab5)
# make new class of strap-like leaves that does not include posidonia, amphobolis or zostera --
hab5$strap.like <- hab5$Strap.like.leaves-(hab5$Amphibolis+hab5$Posidonia+hab5$Thalassodendrum)
# make new class of macroalgae leaves that does not include turf, branching or large canopy --
hab5$macroalgaeother <- hab5$Macroalgae-(hab5$Turf.algae+hab5$Erect.coarse.branching+hab5$Erect.fine.branching)
# remove macroalgae, stralike.leaves 
names(hab5)
hab5 <- hab5[,-c(4,8)]
names(hab5)
head(hab5)
str(hab5)
   

# TV no clusters but transects ----

str(hab5)

# Get transect number from sample ----

hab5 <- hab5 %>% 
  separate(sample, c("transect", "sample"), "_")

head(hab5)
str(hab5)
hab5$transect <- as.factor(hab5$transect)
str(hab5)
head(hab5)

levels(hab5$transect)

# Rename all levels
levels(hab5$transect) <- c("T2", "T3", "T4", "T5", "T6", "T7")
head(hab5)
str(hab5) # 1141 obs

# Save hab data by campaignid/grid ----
#write.csv(hab5, paste(d.dir, "GB_hab_TV_transect_20200914.csv", sep='/'))

####    Multivariate    ----

# Load hab data ----

f <- read.csv(paste(d.dir, "GB_hab_TV_transect_20200914.csv", sep='/'))


# Euclidian ----
names(f)
str(f)
head(f)

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
fb.plot<-cbind(f[-c(1:5,16)], nmdsb1, nmdsb2, cluster= f[,2])
fb.plot
fb.plot$cluster <- as.factor(fb.plot$cluster)

#hpca2 <- PCA(f[,-c(1:5)], quali.sup= 11, graph = T)
#hpca <- PCA(f.diste, graph = T)# plot ordination

hpca2 <- PCA(f[,-c(1:5)], quali.sup= 2, graph = T)


fviz_pca_ind(hpca2,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$transect, # color by groups
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
fb.mds <- metaMDS(f.diste, k=2, trymax = 20, trace=FALSE, plot =T)
fb.mds

# plot w ggplot ----

# pull points from MDS--
nmdsb1 <- fb.mds$points[,1]
nmdsb2 <- fb.mds$points[,2]
fb.plot<-cbind(f[-c(1:5,2)], nmdsb1, nmdsb2, cluster= f[,2])
fb.plot
fb.plot$cluster <- as.factor(fb.plot$cluster)


hpca <- PCA(f[,-c(1,3:5)], quali.sup= 1, graph = T)


fviz_pca_ind(hpca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = f$transect, # color by groups
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
