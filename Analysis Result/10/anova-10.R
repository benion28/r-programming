

##############MOR analysis
rm(list=ls())
setwd("C:/Benion/Benion Programmings/R/Analysis Result/10")
dataset <- read.csv("data-5.csv", sep=",", head=TRUE) #bring in data set to be analysed
View(dataset)

#box plot  suggests a difference of means
names(dataset)
boxplot(D ~ TRT, data = dataset, ylab = " Seedling diameter", xlab = "Treatments", col = "gray") 
boxplot(H ~ TRT, data = dataset, ylab = " Seedling height", xlab = "Treatments", col = "gray")
boxplot(NL ~ TRT, data = dataset, ylab = "Number of leaves", xlab = "Treatments", col = "gray")


# shapiro wilks test MOR
names(dataset)
shapiro.test(dataset$D)
shapiro.test(dataset$H)
shapiro.test(dataset$NL)

shapiro.s<-cbind(D =(shapiro.test(dataset$D)$statistic), 
                     H =(shapiro.test(dataset$H)$statistic),
                     NL =(shapiro.test(dataset$NL)$statistic))

shapiro.s<-cbind(D =(shapiro.test(dataset$D)$statistic), 
                 NL =(shapiro.test(dataset$NL)$statistic))
shapiro.s

shapiro.P<-cbind(D =(shapiro.test(dataset$D)$p.value), 
                     H =(shapiro.test(dataset$H)$p.value),
                     NL =(shapiro.test(dataset$NL)$p.value))

shapiro.P<-cbind(D =(shapiro.test(dataset$D)$p.value), 
                 NL =(shapiro.test(dataset$NL)$p.value))
shapiro.P

rbind(shapiro.s,shapiro.P)

#Homogeneity of variances#check assumption of homoscedasticity 
library(car)
leveneTest(D ~ TRT, data = dataset)
leveneTest(H ~ TRT, data = dataset)
leveneTest(NL ~ TRT, data = dataset)


#Anova test
D_aov <- aov(D ~ TRT, data = dataset)
H_aov = aov(H ~ TRT, data = dataset)
NL_aov = aov(NL ~ TRT, data = dataset)

summary(D_aov)
summary(H_aov) 
summary(NL_aov)#NO SIG DIFF AMONG THE MEANS


#post hoc
#Tukey multiple comparisons of means
TukeyHSD(D_aov)

Htukey<-TukeyHSD(D_aov)
as.data.frame(Htukey$TRT)

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(D_aov, "TRT"))  



