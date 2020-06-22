#### Script to set up the data for Power analysis and to run Power Analysis ####

library(stringr)
library(dplyr)

## BRUVS ##
# Habitat Protection zone = impact = transect 3
#  Multiple purpose zone as controls = transects 4, 6 and 7

# Clear memory ----
rm(list=ls())

# Set working directory ####
#w.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "~/Power_Analysis"
w.dir
# Set data directory - to read the data from
setwd(paste(w.dir, "outputs", sep='/'))


## Set file name --
filen <- "TV_zoning.csv"

# Load data

df <- read.csv(paste(w.dir, filen, sep='/'))
str(df)
names(df)

# Remove NA's - data from state waters
df <- na.omit(df)
str(df)


summary(df)

### split the data by zone name
dfz <- split(df, df$ZoneName)
str(dfz)
# Remove the Special purpose zone
dfn <- dfz[-3]
str(dfn)

# new listo into data frame
dfn <- do.call(rbind.data.frame, dfn)
str(dfn)
names(dfn)
head(dfn)
summary(dfn)
## Get the transect code form Sample number --
#dfn$Transect <- str_sub(dfn$sample, 1,6)
#dfn$Transect <- as.factor(dfn$Transect)
#levels(dfn$Transect)
#str(dfn)

levels(dfn$campaignid)
levels(dfn$ZoneName)
dfn <- droplevels(dfn)

## Get the transect code form Sample number --
dfn$Transect <- str_sub(dfn$sample, 1,6)
dfn$Transect <- as.factor(dfn$Transect)
levels(dfn$Transect)
str(dfn)

# Remove grids 11-15 if wanted
#dfn <- dfn[dfn$campaignid!="r20150528_044923_wa-gb-auv-11b",]
#dfn <- dfn[dfn$campaignid!="r20150529_013035_wa-gb-auv-12c",]
#dfn <- dfn[dfn$campaignid!="r20150528_082632_wa-gb-auv-13b",]
#dfn <- dfn[dfn$campaignid!="r20150529_031600_wa-gb-auv-14c",]
#dfn <- dfn[dfn$campaignid!="r20150529_042328_wa-gb-auv-15c",]

levels(dfn$ZoneName)

## Make T2 part of HPZ and  Make T6 and T7 part of another zone
trans <- dfn$Transect
levels(trans)
dfn$zone.sim <- factor(trans, labels = c("HPZ", "HPZ", "MUZ", "MUZ", "MUZ2", "MUZ2"))
summary(dfn)

levels(dfn$zone.sim)


#subsample dat to n=50 to improve speed

dfn <-as.data.frame(dfn %>% group_by(Transect) %>% sample_n(size = 50))
str(dfn)
dfn <- droplevels(dfn)
str(dfn)

# Make Period Column
dfn$Period <- "Before"
names(dfn)

# Make control impact column
levels(dfn$zone.sim)
dfn$CvI <- ifelse(dfn$zone.sim=="HPZ", "Impact", "Control")
head(dfn)

# Make a time column
dfn$Time <- "T1"
str(dfn)



### T2 ####

# Make a replicate df to account for T2
dfn2 <- dfn
# change T1 to T2 in new df
str(dfn2)
dfn2$Time[dfn2$Time == "T1"] <- "T2"
head(dfn2)

# Change the names of the AUV images
names(dfn2)
dfn2$sample <- sub("^", "T2", dfn2$sample) # the ^ represents the point just before the first character
head(dfn2)

# Change the names of the AUV campaigns
#names(dfn2)
#dfn2$campaignid <- sub("^", "T2", dfn2$campaignid) # the ^ represents the point just before the first character
#head(dfn2)

# Change Before for After
#dfn2$Period <- sub("Before", "After", dfn2$Period)
# currenty only using before times : 3 before times

### T3 ####

# Make a replicate df to account for T2
dfn3 <- dfn
# change T1 to T2 in new df
str(dfn3)
dfn3$Time[dfn3$Time == "T1"] <- "T3"
head(dfn3)

# Change the names of the AUV images
names(dfn3)
dfn3$sample <- sub("^", "T3", dfn3$sample) # the ^ represents the point just before the first character
head(dfn3)

# Change Before for After
#dfn3$Period <- sub("Before", "After", dfn3$Period)
#head(dfn3)

# Change the names
#names(dfn3)
#dfn3$campaignid <- sub("^", "T3", dfn3$campaignid) # the ^ represents the point just before the first character
#head(dfn3)

# Change Before for After
#dfn3$Period <- sub("Before", "After", dfn3$Period) # do not change period for T3



# Combine two data frames
df <- rbind(dfn, dfn2, dfn3)
#df <- rbind(dfn, dfn2)
head(df)

# Remove unnecessary columns
names(df)
df <- df[,c(2,6,21:25)]
head(df)
names(df)
str(df)
df$Period <- as.factor(df$Period)
df$CvI <- as.factor(df$CvI)
df$Time <- as.factor(df$Time)
str(df)
summary(df)

# Write if you want
#write.csv(dfMP, "G:/My Drive/Anita/Power Analysis/Tv/Tv-sg-MP.csv" )


### POWER ANALYSIS ####


#install.packages(rJava)
#library(rJava)

#devtools::install_github("bmtglobal/epower",dep=TRUE, force =TRUE)

#require(dplyr)
#library(dplyr)
#library(rlang)
#require(epower)
#library(rJava)
library(epower)
packageVersion("epower")
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA, verbose=TRUE)
#library(doParallel)
#library(rJava)
#library(XLConnect)


# Set work directory----
#setwd("C:/Users/21933549/Dropbox/UWA/Research Associate/NewPowAn")

#dat <- read.csv("G:/My Drive/Anita/Power Analysis/Tv/Tv-sg-MP.csv")

#summary(dat)

### Set design
names(df)
str(df)
summary(df)
#dat<-read.csv(paste(working.dir, "Data/raw", "BRUV-sg-test1.csv", sep='/'))

dat <- as.data.frame(df)
summary(dat)

str(dat)


# Set design

dataComponents<-supplyData(
  dat=dat,
  variableType="gaussian",
  design.matrix=list(
    Response="Seagrasses",
    Trials=NA,
    Location="zone.sim",
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
    Number.of.iterations=100,
    filename="TV-HPZ-100it_50replicates_20datapoints",
    Number.of.Impact.Locations=1,
    Number.of.Control.Locations=2,
    Number.of.sublocations.within.Location="1;2;3",
    Number.of.sample.times.Before=3,  # this number needs to be higher than 2 (replicates)
    Number.of.sample.times.After="1,2,3",
    #Number.of.sample.times.After=1,
    #Number.of.sample.times.After=2,
    #Number.of.sample.times.After=3,
    Number.of.subtimes.within.Time=NA,
    Number.of.trials=NA,
    Number.of.replicate.measurements=20),
  effect.info=list(
    Multiplicative=1,
    Fixed.change=0,
    Effect.values="-0.2;-0.4;-0.6;-0.8"),
  ncores = 30)

### The supply data function is not reading the ncores
dataComponents$ncores <- 30

require(INLA,quietly=TRUE)
scenarioParams<<-do.call(powerScenario,list(inputData=dataComponents)) # from Kye's script

# CHECK!! that scenario params has times after
scenarioParams$times.after <- c(1,2,3)

#scenarioParams<-powerScenario(inputData=dataComponents) # the equivalent of fitData()
assessPower()


# clear memory
gc()
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())