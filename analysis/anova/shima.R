

##############MOR analysis
rm(list=ls())
setwd("C:/Benion/Benion Programmings/R/analysis/anova")
shima <- read.csv("shima.csv", sep=",", head=TRUE) #bring in data set to be analysed
View(shima)

#box plot  suggests a difference of means
names(shima)
boxplot(D ~ TRT, data = shima, ylab = " Seedling diameter", xlab = "Treatments", col = "gray") 
boxplot(H ~ TRT, data = shima, ylab = " Seedling height", xlab = "Treatments", col = "gray")
boxplot(NL ~ TRT, data = shima, ylab = "Number of leaves", xlab = "Treatments", col = "gray")


# shapiro wilks test MOR
names(shima)
shapiro.test(shima$D)
shapiro.test(shima$H)
shapiro.test(shima$NL)

shapiro.s<-cbind(D =(shapiro.test(shima$D)$statistic), 
                     H =(shapiro.test(shima$H)$statistic),
                     NL =(shapiro.test(shima$NL)$statistic))
shapiro.s

shapiro.P<-cbind(D =(shapiro.test(shima$D)$p.value), 
                     H =(shapiro.test(shima$H)$p.value),
                     NL =(shapiro.test(shima$NL)$p.value))
shapiro.P

rbind(shapiro.s,shapiro.P)

#Homogeneity of variances#check assumption of homoscedasticity 
library(car)
leveneTest(D ~ TRT, data = shima)
leveneTest(H ~ TRT, data = shima)
leveneTest(NL ~ TRT, data = shima)


#Anova test
D_aov <- aov(D ~ TRT, data = shima)
H_aov = aov(H ~ TRT, data = shima)
NL_aov = aov(NL ~ TRT, data = shima)

summary(D_aov)
summary(H_aov) 
summary(NL_aov)

#NO SIG DIFF AMONG THE MEANS