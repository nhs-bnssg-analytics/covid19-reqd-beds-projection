
setwd("My documents")

source("coronasimr.R")

cases<-read.csv("hospitalisations.csv")

coronasimr(cases=cases,los_median=3,los_95=30)



