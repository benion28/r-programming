

# install.packages('lmfor')
# install.packages('ForestFit')



###################################   final 

rm(list=ls())
setwd("C:/Benion/Benion Programmings/R/analysis/diameter distribution")

NData <- read.csv("NData.csv", sep=",", head=TRUE)
#View(NData)
head(NData)
diameters <- NData$DBH

plot(diameters, pch=20)
library(ggplot2)
ggplot(data=NData) + geom_histogram(mapping=aes(x=DBH ),bins=10, col="black", fill="grey")

#install.packages("fitdistrplus")
library(fitdistrplus)
#?fitdist


#Cullen and Frey graph : Assess the data in terms of skewness (+ve or -ve skew) and kurtosis 
#(sharpness of the peak of the curve)
descdist(diameters, discrete=FALSE, boot=500)

#fit distributions 
fitg <- fitdist(diameters, "gamma")
fitW <- fitdist(diameters, "weibull")
fitln <- fitdist(diameters, "lnorm")
fitcauchy <- fitdist(diameters, "cauchy")
fitnorm <- fitdist(diameters, "norm")
fitexp <- fitdist(diameters, "exp")
fitLlG <- fitdist(diameters, "logis")
summary(fitW)
summary(fitg)
summary(fitln) #(meanlog = location. sdlog =scale)
summary(fitcauchy)
summary(fitnorm) #(mean = location. sd =scale)
summary(fitexp)
summary(fitLlG)

#goodness of fit statistics
gofstat(list(fitW, fitg, fitln, fitnorm, fitcauchy, fitexp, fitLlG), 
         fitnames=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"))

#plots
head(NData)

hist(diameters)

par(mfrow=c(2,2))
denscomp(fitW, legendtext="Weibull",main = "",xlab = "Dbh (cm)")
denscomp(fitg, legendtext="Gamma",main = "",xlab = "Dbh (cm)")
denscomp(fitln, legendtext="Lognormal",main = "",xlab = "Dbh (cm)")
denscomp(fitnorm, legendtext="Normal",main = "",xlab = "Dbh (cm)")
denscomp(fitcauchy, legendtext="Cauchy",main = "",xlab = "Dbh (cm)")
denscomp(fitexp, legendtext="Exponential",main = "",xlab = "Dbh (cm)")
denscomp(fitLlG, legendtext="Logistics",main = "",xlab = "Dbh (cm)")
par(mfrow=c(1,1))

denscomp(list(fitW, fitg, fitln, fitnorm, fitcauchy, fitexp, fitLlG), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "",xlab = "Dbh (cm)")

cdfcomp(list(fitW, fitg, fitln, fitnorm, fitcauchy, fitexp, fitLlG), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "", ylab = "Cumulative Distribution Function",
        xlab = "Dbh (cm)")

qqcomp(list(fitW, fitg, fitln, fitnorm, fitcauchy, fitexp, fitLlG), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "")

ppcomp(list(fitW, fitg, fitln, fitnorm, fitcauchy, fitexp, fitLlG), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "")


