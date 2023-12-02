
knitr::opts_chunk$set(echo = TRUE)

# Read data

setwd("/Users/kppoudel/Desktop/TAG NG/DataAnalysis")
df = read.csv("NayBormann.csv")

head(df)
summary(df)

# Plotting the data
library(ggplot2)

p1 = ggplot(data=df, aes(x=dbh, y=tab)) + geom_point() +
  xlab("DBH (cm)") + ylab("Total aboveground biomass (kg)") +
  theme_bw()
p1

p2 = ggplot(data=df, aes(x=tht, y=tab)) + geom_point() +
  xlab("Total Tree Height (m)") + ylab("Total aboveground biomass (kg)") +
  theme_bw()
p2

library(gridExtra)

grid.arrange(p1, p2)
grid.arrange(p1, p2, nrow=1)

# Fit model
mod1 = lm(tab ~ dbh + tht, data=df)
summary(mod1)

# Fitted values
df$tab_pred = fitted(mod1)
head(df, n=5)

# Residuals
r1 = ggplot() +
  aes(fitted(mod1), residuals(mod1)) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot") +
  theme(plot.title = element_text(hjust = 0.5))
r1


mod2 = lm(lntab ~ lndbh + lntht, data=df)
summary(mod2)

summary(mod1)$adj.r.squared #un-transformed model
summary(mod2)$adj.r.squared #log-transformed model

r2 = ggplot() +
  aes(fitted(mod2), residuals(mod2)) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot - Transformes Model") +
  theme(plot.title = element_text(hjust = 0.5))
r2

grid.arrange(r1, r2)
