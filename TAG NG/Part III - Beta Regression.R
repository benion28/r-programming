
knitr::opts_chunk$set(echo = TRUE)

# Read data

setwd("/Users/kppoudel/Desktop/TAG NG/DataAnalysis")
df = read.csv("NayBormann.csv")

head(df)
summary(df)

# Plotting the data
library(ggplot2)
library(gridExtra)
library(reshape)

df = melt(df, id.varse=c("id", "dbh", "tht", "lcl", "tab"))
colnames(df)[6:7] = c("comp", "mass")
df$percent = df$mass/df$tab
head(df)

# Beta Regression

## Foliage
library(betareg)

fol = subset(df, comp == "fol")

lm_fol = lm(percent ~ dbh + lcl, data = fol)
summary(lm_fol)

beta_fol = betareg(percent ~ dbh + lcl, data = fol)
summary(beta_fol)

# Compare fitted lines

newdat = data.frame(lcl=mean(fol$lcl), dbh=fol$dbh, percent=fol$percent)

newdat$pred1 = predict(lm_fol, newdata=newdat)
newdat$pred2 = predict(beta_fol, newdata=newdat)

ggplot(newdat, aes(x=dbh, y=percent)) + geom_point() +
  geom_line(aes(x=dbh, y=pred1, color="OLS"), lwd=1) +
  geom_line(aes(x=dbh, y=pred2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of foliage biomass")


## Stem bark
brk = subset(df, comp == "brk")

lm_brk = lm(percent ~ dbh + lcl, data = brk)
summary(lm_brk)

beta_brk = betareg(percent ~ dbh + lcl, data = brk)
summary(beta_brk)

# Compare fitted lines

newdat_brk = data.frame(dbh=mean(brk$dbh), lcl=brk$lcl, percent=brk$percent)

newdat_brk$pred1 = predict(lm_brk, newdata=newdat_brk)
newdat_brk$pred2 = predict(beta_brk, newdata=newdat_brk)

ggplot(newdat_brk, aes(x=lcl, y=percent)) + geom_point() +
  geom_line(aes(x=lcl, y=pred1, color="OLS"), lwd=1) +
  geom_line(aes(x=lcl, y=pred2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of stembark biomass")


## Branch
bch = subset(df, comp == "bch")

lm_bch = lm(percent ~ dbh + lcl, data = bch)
summary(lm_bch)

beta_bch = betareg(percent ~ dbh + lcl, data = bch)
summary(beta_bch)

# Compare fitted lines

newdat_bch = data.frame(dbh=mean(bch$dbh), lcl=bch$lcl, percent=bch$percent)

newdat_bch$pred1 = predict(lm_bch, newdata=newdat_bch)
newdat_bch$pred2 = predict(beta_bch, newdata=newdat_bch)

ggplot(newdat_bch, aes(x=lcl, y=percent)) + geom_point() +
  geom_line(aes(x=lcl, y=pred1, color="OLS"), lwd=1) +
  geom_line(aes(x=lcl, y=pred2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of branch biomass")



## Stemwood
stm = subset(df, comp == "stm")

lm_stm = lm(percent ~ dbh + lcl, data = stm)
summary(lm_bch)

beta_stm = betareg(percent ~ dbh + lcl, data = stm)
summary(beta_stm)

# Compare fitted lines

newdat_stm = data.frame(dbh=mean(stm$dbh), lcl=stm$lcl, percent=stm$percent)

newdat_stm$pred1 = predict(lm_stm, newdata=newdat_stm)
newdat_stm$pred2 = predict(beta_stm, newdata=newdat_stm)

ggplot(newdat_stm, aes(x=lcl, y=percent)) + geom_point() +
  geom_line(aes(x=lcl, y=pred1, color="OLS"), lwd=1) +
  geom_line(aes(x=lcl, y=pred2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of stemwood biomass")
