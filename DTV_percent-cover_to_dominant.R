
#####    DTV percent cover to dominant    #####

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
library(tidyverse)
library(RColorBrewer)


# clear environment ---
rm(list=ls()) #clear memory


# Study name----

study <- "DTV"

# Set work directory----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to where this script is saved (DON't MOVE)
setwd(w.dir)

# Set sub directories----

d.dir <- paste(w.dir,"Data/raw",sep="/")
p.dir <- paste(w.dir,"Plots",sep="/")
s.dir <- "G:/My Drive/Anita/Shapefiles" 

# file name
dir(d.dir)
f <- "DTV_detailed_habitat_percent.cover_for_class.csv"

# read data ----

df <- read.csv(paste(d.dir, f, sep='/'))
head(df)
str(df) # 4847 obs

# which class has max percent cover ----
df2 <-df
names(df2)
df2$Macroalgae <- df2$total.Macroalgae-df2$Turf.algae # Macroalgae - turf
names(df2)
df3 <- df2[,c(3,10,13,14,15,21,22,23)] # remove unwanted cols
head(df3)
str(df3)

df3$dominant <- colnames(df3)[apply(df3,1,which.max)]
head(df3)

# check for NAs
any(is.na(df3))
str(df3) # 4847 obs

# save data ----
#write.csv(df3, paste(d.dir, "DTV_detailed_habitat_dominant.csv", sep ='/'))
