
knitr::opts_chunk$set(echo = TRUE)

# Read data

setwd("/Users/kppoudel/Desktop/TAG NG/DataAnalysis")
df = read.csv("NayBormann.csv")

head(df)
summary(df)

# Plotting the data
library(ggplot2)
library(gridExtra)

df$fol = df$fol/df$tab
df$brk = df$brk/df$tab
df$bch = df$bch/df$tab
df$stm = df$stm/df$tab

# Plot component proportion vs dbh
ggplot(df) +
  geom_point(aes(x=dbh, y=fol, color="Foliage")) +
  geom_smooth(aes(x=dbh, y=fol, color="Foliage"), se=F) +
  geom_point(aes(x=dbh, y=bch, color="Branch")) +
  geom_smooth(aes(x=dbh, y=bch, color="Branch"), se=F) +
  geom_point(aes(x=dbh, y=brk, color="Stembark")) +
  geom_smooth(aes(x=dbh, y=brk, color="Stembark"), se=F) +
  geom_point(aes(x=dbh, y=stm, color="Stemwood")) +
  geom_smooth(aes(x=dbh, y=stm, color="Stemwood"), se=F) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("dbh (cm)") +
  ylab("Component Proportion")

# Plot component proportion vs lcl
ggplot(df) +
  geom_point(aes(x=lcl, y=fol, color="Foliage")) +
  geom_smooth(aes(x=lcl, y=fol, color="Foliage"), se=F) +
  geom_point(aes(x=lcl, y=bch, color="Branch")) +
  geom_smooth(aes(x=lcl, y=bch, color="Branch"), se=F) +
  geom_point(aes(x=lcl, y=brk, color="Stembark")) +
  geom_smooth(aes(x=lcl, y=brk, color="Stembark"), se=F) +
  geom_point(aes(x=lcl, y=stm, color="Stemwood")) +
  geom_smooth(aes(x=lcl, y=stm, color="Stemwood"), se=F) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Live crown length (m)") +
  ylab("Component Proportion")

# Dirichlet regression
library(DirichletReg)
df$Y = DR_data(df[, 5:8])

diri0 = DirichReg(Y ~ dbh + lcl, data=df, model="alternative")
summary(diri0)

diri1 = DirichReg(Y ~ dbh + lcl, data=df, model="alternative", base=4)
summary(diri1)

# Fitted values
df$pfol = fitted(diri1)[, "fol"]
df$pbrk = fitted(diri1)[, "brk"]
df$pbch = fitted(diri1)[, "bch"]
df$pstm = fitted(diri1)[, "stm"]

# Observed vs Predicted
op = par(oma=c(3,3,0,0), mar=c(3,3,2,2), mfrow=c(2,2))
plot(df$fol, df$pfol, font.main=1,
     main="Foliage Biomass", pch=20); abline(0,1)
plot(df$brk, df$pbrk, font.main=1,
     main="Bark Biomass", pch=20); abline(0,1)
plot(df$bch, df$pbch, font.main=1,
     main="Branch Biomass", pch=20); abline(0,1)
plot(df$stm, df$pstm, font.main=1,
     main="Stemwood Biomass", pch=20); abline(0,1)
mtext(text="Observed Proportion", side=1, line=0, outer=TRUE)
mtext(text="Predicted Proportion", side=2, line=0, outer=TRUE)
par(op)

newdat1 = df
newdat1$lcl = mean(df$lcl)

newdat1$fol1 = predict(diri1, newdata=newdat1)[, 1]
newdat1$bch1 = predict(diri1, newdata=newdat1)[, 2]
newdat1$brk1 = predict(diri1, newdata=newdat1)[, 3]
newdat1$stm1 = predict(diri1, newdata=newdat1)[, 4]

# Plot component proportion vs lcl
ggplot(newdat1) +
  geom_point(aes(x=dbh, y=fol, color="Foliage")) +
  geom_line(aes(x=dbh, y=fol1, color="Foliage")) +
  geom_point(aes(x=dbh, y=bch, color="Branch")) +
  geom_line(aes(x=dbh, y=bch1, color="Branch")) +
  geom_point(aes(x=dbh, y=brk, color="Stembark")) +
  geom_line(aes(x=dbh, y=brk1, color="Stembark")) +
  geom_point(aes(x=dbh, y=stm, color="Stemwood")) +
  geom_line(aes(x=dbh, y=stm1, color="Stemwood")) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Live crown length (m)") +
  ylab("Component Proportion")

# Diagnostic plots
op = par(oma=c(3,3,0,0), mar=c(3,3,2,2), mfrow=c(2,2))
plot(fitted(diri1)[, 1],
     residuals(diri1)[, 1],
     main="Foliage Biomass")
plot(fitted(diri1)[, 2],
     residuals(diri1, type="standardized")[, 2],
     main="Branch Biomass")
plot(fitted(diri1)[, 3],
     residuals(diri1, type="standardized")[, 3],
     main="Stembark Biomass")
plot(fitted(diri1)[, 4],
     residuals(diri1, type="standardized")[, 1],
     main="Stemwood Biomass")
mtext(text="Fitted values", side=1, line=0, outer=TRUE)
mtext(text="Standardized residuals", side=2, line=0, outer=TRUE)
par(op)
