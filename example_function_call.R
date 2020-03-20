
setwd("My documents")

source("covid19_reqd_beds_projection.R")

cases<-read.csv("hospitalisations.csv")

coronasimr(cases=cases,los_median=3,los_95=30)



