#### Script to set up the data for Power analysis and to run Power Analysis ####

library(stringr)


## BRUVS ##
# Habitat Protection zone = impact = transect 3
#  Multiple purpose zone as controls = transects 4, 6 and 7

# Clear memory ----
rm(list=ls())

# Set working directory ####
#w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "H:/GitHub/Power-Analysis"
w.dir
# Set data directory - to read the data from
d.dir <- paste(w.dir, "Data", "tidy", sep='/')

## Set file name --
filen <- "TV_zoning.csv"

# Load data

df <- read.csv(paste(d.dir, filen, sep='/'))
str(df)
names(df)

# Remove NA's - data from state waters
df <- na.omit(df)
str(df)

### split the data by zone name
dfz <- split(df, df$ZoneName)
str(dfz)
# Remove the Special purpose zone
dfn <- dfz[-3]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
dfn
str(dfn)
names(dfn)

## Get the transect code form Sample number --
dfn$Transect <- str_sub(dfn$sample, 1,6)
dfn$Transect <- as.factor(dfn$Transect)
levels(dfn$Transect)
str(dfn)

# Remove transects 2 and 5
dfn <- dfn[dfn$Transect!="LGBDT2",]
dfn <- dfn[dfn$Transect!="LGBDT5",]
dfn <- droplevels(dfn)
levels(dfn$Transect)

# Make Period Column
dfn$Period <- "Before"
names(dfn)

# Make control impact column
levels(dfn$ZoneName)
dfn$CvI <- ifelse(dfn$ZoneName=="Habitat Protection Zone", "Impact", "Control")
head(dfn)

# Make a time column
dfn$Time <- "T1"
str(dfn)

# Make a replicate df to account for T2
dfn2 <- dfn
# change T1 to T2 in new df
str(dfn2)
dfn2$Time[dfn2$Time == "T1"] <- "T2"
head(dfn2)

# Combine two data frames
df <- rbind(dfn, dfn2)
head(dfn)

# Remove unnecessary columns
names(df)
df <- df[,c(2,6,13,21:24)] 
head(df)
str(df)
df$Period <- as.factor(df$Period)
df$CvI <- as.factor(df$CvI)
df$Time <- as.factor(df$Time)
str(df)
summary(df)

# Write if you want
#write.csv(dfMP, "G:/My Drive/Anita/Power Analysis/Tv/Tv-sg-MP.csv" )


### POWER ANALYSIS ####


#devtools::install_github("bmtglobal/epower")
require(epower)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#library(doParallel)
#library(rJava)
#library(XLConnect)


# Set work directory----
setwd("G:/My Drive/Anita/Power Analysis/Tv")

dat <- read.csv("G:/My Drive/Anita/Power Analysis/Tv/Tv-sg-MP.csv")

summary(dat)

### Set design
names(df)
str(df)
#dat<-read.csv(paste(working.dir, "Data/raw", "BRUV-sg-test1.csv", sep='/'))

dat <- df

str(dat)

# Set design

dataComponents<-supplyData(
  dat=dat,
  variableType="gaussian",
  design.matrix=list(
    Response="Seagrasses",
    Trials=NA,
    Location="ZoneName",
    sublocation="Transect",
    Time="Time",
    "subtime"=NA,
    BvA="Period",
    CvI="CvI"),
  levels.dat=list(
    Before="Before",
    Control="Control",
    After="After",
    Impact="Impact"),
  scenario.data=list(
    Number.of.iterations=5,
    filename="Tv-test1",
    Number.of.Impact.Locations=1,
    Number.of.Control.Locations=1,
    Number.of.sublocations.within.Location="2;3",
    Number.of.sample.times.Before=2,
    Number.of.sample.times.After="1;2;3",
    Number.of.subtimes.within.Time=NA,
    Number.of.trials=NA,
    Number.of.replicate.measurements=100),
  effect.info=list(
    Multiplicative=1,
    Fixed.change=0,
    Effect.values="0;-0.2;-0.4;-0.6,-0.8"), ncores = 2)


#require(INLA,quietly=TRUE)
scenarioParams<-powerScenario(inputData=dataComponents) # the equivalent of fitData()
assessPower()



