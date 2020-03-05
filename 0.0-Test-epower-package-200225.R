
# Clear memory ----
rm(list=ls())

#devtools::install_github("bmtglobal/epower")
require(epower)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(doParallel)
library(rJava)
library(XLConnect)


# Set work directory----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path)# sets working directory to where this script is saved (DON't MOVE THE SCRIPT)


setwd(paste(working.dir, "test", "data-out", sep='/'))

# create new directories
#dir.create("Data")
#dir.create("Plots")
#dir.create(paste(working.dir, "Data/raw", sep='/'))
#dir.create(paste(working.dir, "Data/tidy", sep='/'))


# Set sub directories----
raw.dir = paste(working.dir,"data/raw",sep="/")
tidy.dir = paste(working.dir,"data/tidy",sep="/")



### example using only R to interface with the package

dat<-read.csv(paste(working.dir, "Data/raw", "BRUV-sg-test1.csv", sep='/'))

dataComponents<-supplyData(
  dat=dat,
  variableType="gaussian",
  design.matrix=list(
    Response="Total.seagrass",
    Trials=NA,
    Location="Location",
    sublocation="Bruv",
    Time=NA,
    "subtime"=NA,
    BvA="BvA",
    CvI="CvI"),
  levels.dat=list(
    Before="Before",
    Control="Control",
    After="After",
    Impact="Impact"),
  scenario.data=list(
    Number.of.iterations=5,
    filename="bruv-test6",
    Number.of.Impact.Locations=1,
    Number.of.Control.Locations=3,
    Number.of.sublocations.within.Location=1,
    Number.of.sample.times.Before=1,
    Number.of.sample.times.After=1,
    Number.of.subtimes.within.Time=NA,
    Number.of.trials=NA,
    Number.of.replicate.measurements="10;20"),
  effect.info=list(
    Multiplicative=1,
    Fixed.change=0,
    Effect.values="-0.2;-0.5;-0.9"), ncores = 2)


#require(INLA,quietly=TRUE)
scenarioParams<-powerScenario(inputData=dataComponents) # the equivalent of fitData()
assessPower()


