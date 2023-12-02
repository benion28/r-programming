
knitr::opts_chunk$set(echo = TRUE)

# Read data

setwd("/Users/kppoudel/Desktop/TAG NG/DataAnalysis")
df = read.csv("NayBormann.csv")

head(df)
summary(df)

# Plotting the data
library(ggplot2)
library(gridExtra)

stm = ggplot(data=df, aes(x=dbh, y=stm)) + geom_point() +
  xlab("DBH (cm)") + ylab("Stemwood biomass (kg)") +
  theme_bw()

brk = ggplot(data=df, aes(x=dbh, y=brk)) + geom_point() +
  xlab("DBH (cm)") + ylab("Stembark biomass (kg)") +
  theme_bw()

bch = ggplot(data=df, aes(x=dbh, y=bch)) + geom_point() +
  xlab("DBH (cm)") + ylab("Branchwood biomass (kg)") +
  theme_bw()

fol = ggplot(data=df, aes(x=dbh, y=fol)) + geom_point() +
  xlab("DBH (cm)") + ylab("Foliage biomass (kg)") +
  theme_bw()

grid.arrange(stm, brk, bch, fol)

# Logarithmic Transformation
df$lndbh = log(df$dbh)
df$lnlcl = log(df$lcl)
df$lntht = log(df$tht)
df$lnstm = log(df$stm)
df$lnbrk = log(df$brk)
df$lnbch = log(df$bch)
df$lnfol = log(df$fol)
df$lntab = log(df$tab)

library(systemfit)

stm = lnstm ~ lndbh + lntht
brk = lnbrk ~ lndbh + lntht
bch = lnbch ~ lndbh + lntht
fol = lnfol ~ lndbh + lntht
system = list(Stemwood.Model = stm,
              Stembark.Model = brk,
              Branch.Model = bch,
              Foliage.Model = fol)

# OLS fit
fitOLS = systemfit(system, "OLS", data = df)
summary(fitOLS)

fitfol = lm(lnfol ~ lndbh + lnlcl, data = df)
summary(fitfol)

fitsur = systemfit(system, "SUR", data = df)
summary(fitsur)
