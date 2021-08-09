#### Script to set up the data for Power analysis and to run Power Analysis ####

library(stringr)

#devtools::install_github("bmtglobal/epower")
require(epower)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#library(doParallel)
#library(rJava)
#library(XLConnect)


## BOSS ##
# Habitat Protection zone = impact = transect 3
#  Multiple purpose zone as controls = transects 4, 6 and 7

# Clear memory ----
rm(list=ls())

# Load data

dwndat <- readRDS("data/tidy/2103_Geographe_BOSS_downwardshabitat.rds")

# Remove the Special purpose zone
dwndat <- dwndat[dwndat$ZoneName != "Special Purpose Zone (Mining Exclusion)", ]

# setup for power analysis
dwndat$Period <- "Before"                                                       # Make Period Column
dwndat$CI     <- ifelse(dwndat$ZoneName == "Habitat Protection Zone", 
                        "Impact", "Control")                                    # Make control impact column
# dwndat$CI[dwndat$ZoneName == "Multiple Use Zone"] <- c("Control2")
dwndat <- rbind(dwndat, dwndat, dwndat)
dwndat$Time <- c(rep("T1", nrow(dwndat)/3), 
                 rep("T2", nrow(dwndat)/3), 
                 rep("T3", nrow(dwndat)/3))                                     # dummy times

str(dwndat)
dwndat$Period <- as.factor(dwndat$Period)
dwndat$CI     <- as.factor(dwndat$CI)
dwndat$Time   <- as.factor(dwndat$Time)
summary(dwndat)

# # Remove unnecessary columns
# names(df)
# df <- df[,c(2,6,13,21:24)] 
# head(df)

### POWER ANALYSIS ####
### Set design

dataComponents<-supplyData(
  dat=dwndat,
  variableType="gaussian",
  design.matrix=list(
    Response="Seagrasses",
    Trials=NA,
    Location="ZoneName",
    sublocation=NA,
    Time="Time",
    "subtime"=NA,
    BvA="Period",
    CvI="CI"),
  levels.dat=list(
    Before="Before",
    Control="Control",
    After="After",
    Impact="Impact"),
  scenario.data=list(
    Number.of.iterations=5,
    filename="Boss-test1",
    Number.of.Impact.Locations=1,
    Number.of.Control.Locations=2,
    Number.of.sublocations.within.Location=NA,
    Number.of.sample.times.Before=2,
    Number.of.sample.times.After="1;2;3",
    Number.of.subtimes.within.Time=NA,
    Number.of.trials=NA,
    Number.of.replicate.measurements=100),
  effect.info=list(
    Multiplicative=1,
    Fixed.change=0,
    Effect.values="0;-0.2;-0.4;-0.6,-0.8"), ncores = 8)


#require(INLA,quietly=TRUE)
scenarioParams<-powerScenario(inputData=dataComponents) # the equivalent of fitData()
assessPower()


