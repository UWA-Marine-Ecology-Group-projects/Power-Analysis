
###
# Project: Geographe Bay
# Data:    BOSS
# Task:    Cleaning boss habitat data and adding spatial covariates
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(reshape2)
library(dplyr)
# library(googlesheets4)
library(raster)
library(sp)
library(rgdal)

# get metadata from survey tracker googlesheet
# bossd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
#                                   sheet = "2021-03_Geographe_BOSS"))
# saveRDS(bossd, 'data/2103_Geographe_BOSS_metadata.rds')

# read in data
bosmet <- readRDS('data/raw/2103_Geographe_BOSS_metadata.rds')                  # boss metadata
habdat <- read.table('data/raw/2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat annotations
downdat <- read.table('data/raw/2021-03_Geographe_BOSS_Downwards_Habitat_Dot Point Measurements.txt', 
                      skip = 5, sep = "\t")                                     # downwards habitat annotations
geobay <- readOGR("Data/shapefiles/GeoBay.shp")                                 # GEO bay Marine Park zones
proj4string(geobay) <- CRS("+proj=longlat +ellps=GRS80 +no_defs")

# clean all and merge to combine with metadata
bosmet <- bosmet[, colnames(bosmet) %in% c("Date", "Time.bottom", "Latitude",
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]                             # only cols of interest
summary(habdat)
habdat <- habdat[ , c(1, 4, 5, 18:21, 23, 26)]                                  # omit bare columns
colnames(habdat) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")            # fix colnames
habdat$Site      <- gsub(".jpg", "", habdat$Filename)
downdat <- downdat[ , c(1, 4, 5, 18:21, 23, 26)]                                # omit bare columns
colnames(downdat) <- c("Filename", "Image row", "Image col", "Broad", 
                       "Morphology", "Type", "FOV", "CODE", "Radius")           # fix colnames
downdat$Site      <- gsub(".jpg", "", downdat$Filename)

allhab <- merge(bosmet, habdat,by.x = "Sample", by.y = "Site")
allhab$pa <- c(1)
head(allhab)

alldwn <- merge(bosmet, downdat,by.x = "Sample", by.y = "Site")
alldwn$pa <- c(1)
head(alldwn)

# convert to wide percent cover data
allhabw <- dcast(allhab, Sample + Latitude + Longitude + Depth ~ Broad, 
                 value.var = "pa", fun.aggregate = sum, drop = TRUE)
allhabw$totpts <- rowSums(allhabw[, 6:14]) - (allhabw$Unknown + allhabw$Var.5) 
head(allhabw)

allhabw_pc <- allhabw
allhabw_pc[6:14] <- sapply(allhabw_pc[, 6:14], 
                           FUN = function(x){
                             round((x/allhabw_pc$totpts)*100, 2)})              # calculate percent cover

alldwnw <- dcast(alldwn, Sample + Latitude + Longitude + Depth ~ Broad, 
                 value.var = "pa", fun.aggregate = sum, drop = TRUE)
alldwnw$totpts <- rowSums(alldwnw[, 5:12]) - (alldwnw$Unscorable + alldwnw$Var.5) 
alldwnw_pc <- alldwnw
alldwnw_pc[6:11] <- sapply(alldwnw_pc[, 6:11], 
                           FUN = function(x){
                             round((x/alldwnw_pc$totpts)*100, 2)})              # calculate percent cover


# get zone information from shapefile
allhab_sp <- SpatialPointsDataFrame(coords = cbind(allhabw_pc$Longitude, 
                                                   allhabw_pc$Latitude), 
                                    data = allhabw_pc)
proj4string(allhab_sp) <- proj4string(geobay)
alldwn_sp <- SpatialPointsDataFrame(coords = cbind(alldwnw_pc$Longitude, 
                                                   alldwnw_pc$Latitude), 
                                    data = alldwnw_pc, 
                                    proj4string = proj4string(geobay))
proj4string(alldwn_sp) <- proj4string(geobay)
plot(geobay)
plot(allhab_sp, add = T)

# get zone names
allhab <- cbind(allhabw_pc, over(allhab_sp, geobay))
head(allhab)
summary(allhab)

alldwn <- cbind(alldwnw_pc, over(alldwn_sp, geobay))
head(alldwn)
summary(alldwn)

# # if we want presence/absence at the image scale
# allhab[, 6:14] <- sapply(allhab[, 6:14], 
#                            FUN = function(x){ifelse(x > 1, 1, 0)})              # convert to binomial
# alldwn[, 6:11] <- sapply(alldwn[, 6:11], 
#                          FUN = function(x){ifelse(x > 1, 1, 0)})              # convert to binomial
# 

saveRDS(allhab, "data/tidy/2103_Geographe_BOSS_habitat.rds")
saveRDS(alldwn, "data/tidy/2103_Geographe_BOSS_downwardshabitat.rds")








