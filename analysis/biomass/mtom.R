

rm(list=ls())
setwd("C:/Documents/forest")
data <- read.csv("mtom.csv", sep=",", head=TRUE)

#View(data)
head(data)
summary(data)
summary(data$SPECIES)
spp<-as.factor(data$SPECIES)
levels(spp)
summary(spp)

spps <- as.data.frame(summary(spp))
spps
View(spps)

