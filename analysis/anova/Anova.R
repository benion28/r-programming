###########################################   patience

rm(list=ls())
setwd("C:/Documents/Anova")
pat <- read.csv("patience.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(pat)  
names(pat)

### analysis for differences in seedling height

#Check that the data meets the assumptions of Anova
#To check whether the response variable (H) fits a normal distribution (bell curve), we can 
#make a histogram of the response variable: 
hist(pat$H)
hist(pat$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "", col = "gray")

shapiro.test(pat$H)

#box plot  
boxplot(H ~ TRT, data = pat)
boxplot(H ~ TRT, data = pat,
        ylab = "Seedling height",
        xlab = "Treatments",
        notch = FALSE,
        varwidth = TRUE)
#abline(col="black")

#Anova test
H_aov = aov(H ~ TRT, data = pat)
summary(H_aov)

#Homogeneity of variances#check assumption of homoscedasticity 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = pat)

#From the output we can see that the p-value is not less than the significance level of
#0.05. This means that there is no evidence to suggest that the variance across groups is 
#statistically significantly different. Therefore, we can assume the homogeneity of variances 
#in the different treatment groups.

#Non-parametric alternative to one-way ANOVA test
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test,
#which can be used when ANNOVA assumptions are not met.

kruskal.test(H ~ TRT, data = pat)



############### analysis for differences in seedling diameter
#Check that the data meets the assumptions of Anova
hist(pat$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")

shapiro.test(pat$D)
#box plot  suggests a difference of means
boxplot(D ~ TRT, data = pat, ylab = " Seedling diameter(mm)", xlab = "Treatments", col = "gray")

#Anova test

D_aov = aov(D ~ TRT, data = pat)
summary(D_aov)

#Homogeneity of variances#check assumption of homoscedasticity 
leveneTest(D ~ TRT, data = pat)


############### analysis for differences in number of leaves

#Check that the data meets the assumptions of Anova
hist(pat$NL, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")
shapiro.test(pat$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = pat, ylab = " Number of Leaves", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = pat)
summary(NL_aov)

#post hoc
#Tukey multiple comparisons of means
TukeyHSD(NL_aov)

Htukey<-TukeyHSD(NL_aov)
as.data.frame(Htukey$TRT)

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(NL_aov, "TRT"))  

leveneTest(NL ~ TRT, data = pat)


############### analysis for differences in leaf area
names(pat)
#Check that the data meets the assumptions of Anova
hist(pat$LA, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")
shapiro.test(pat$LA)

#box plot  suggests a difference of means
boxplot(LA ~ TRT, data = pat, ylab = " Leaf Area", xlab = "Treatments", col = "gray")

#Anova test
LA_aov = aov(LA ~ TRT, data = pat)
summary(LA_aov)

#post hoc
#Tukey multiple comparisons of means
TukeyHSD(LA_aov)

Htukey<-TukeyHSD(LA_aov)
as.data.frame(Htukey$TRT)

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(LA_aov, "TRT"))  

leveneTest(LA ~ TRT, data = pat)

######################### Pius #################################

rm(list=ls())
setwd("C:/Documents/Anova")
Pius <- read.csv("Pius.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(Pius)  
names(Pius)

#The following commands will install FSA package if it is not already installed:
#if(!require(FSA)){install.packages("FSA")}

############### analysis for differences in seedling height

#Check that the data meets the assumptions of Anova
#To check whether the response variable (H) fits a normal distribution (bell curve), we can 
#make a histogram of the response variable: 
hist(Pius$H)
hist(Pius$H, ylab = "Frequency",xlab = "Seedling height (cm)")
hist(Pius$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "")
hist(Pius$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "", col = "gray")

shapiro.test(Pius$H)

library(rcompanion)
plotNormalHistogram(Pius$H, ylab = "Frequency",xlab = "Seedling height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(Pius$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling height")
abline(mean(y), sd(y))

#The response variable, ‘H’, follows a bell curve, with most observations grouped toward the 
#middle of the distribution and fewer on the tails, so we can proceed with analysis.

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = Pius)
boxplot(H ~ TRT, data = Pius,
        ylab = "Seedling height",
        xlab = "Treatments",
        notch = FALSE,
        varwidth = TRUE)
#abline(col="black")

#Anova test
H_aov = aov(H ~ TRT, data = Pius)
summary(H_aov)

# created a dataframe with a row for each Treatment. By predicting on this dataframe, we obtain the
#sample means of each Treatment. (group).
TRTs = data.frame(TRT = unique(Pius$TRT))
data.frame(TRTs, meanH = predict(H_aov, TRTs))

#or
mean(Pius$H[Pius$TRT=="T1"])
rbind(T1=(mean(Pius$H[Pius$TRT=="T1"])), T2=(mean(Pius$H[Pius$TRT=="T2"])),
      T3=(mean(Pius$H[Pius$TRT=="T3"])))

#or Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = Pius,digits=3)

#post hoc
#Pairwise comparisons using t tests with pooled SD
with(Pius, pairwise.t.test(H, TRT, p.adj = "none"))

#P value adjustment using bonferroni method 
with(Pius, pairwise.t.test(H, TRT, p.adj = "bonferroni")) 

#Tukey multiple comparisons of means
TukeyHSD(H_aov, conf.level = 0.95)
TukeyHSD(H_aov) #same results as above
#Based on these results, note which pairwise comparisons are significant. If you return to the 
#original boxplot, these results should not be surprising.

Htukey<-TukeyHSD(H_aov)
Htukey<- as.data.frame(Htukey$TRT)
#write.csv(Htukey, "C:/Documents/Anova/Htukey.csv")

#Also we can easily produce a plot of these confidence intervals.
plot(TukeyHSD(H_aov, conf.level = 0.95)) #95% family-wise confidence level
# tukey.plot<-TukeyHSD(H_aov)
# plot(tukey.plot, las = 1)
plot((TukeyHSD(H_aov, conf.level = 0.95)),las = 1) #better axis labels

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(H_aov, "TRT"))  



#Homogeneity of variances#check assumption of homoscedasticity 
#plot(H_aov, 1)
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted H (cm)", ylab = "Standardized Residuals") #standardized residuals
par(main = "")
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = Pius)

bartlett.test(H ~ TRT, data = Pius)

#From the output we can see that the p-value is not less than the significance level of
#0.05. This means that there is no evidence to suggest that the variance across groups is 
#statistically significantly different. Therefore, we can assume the homogeneity of variances 
#in the different treatment groups.

#Non-parametric alternative to one-way ANOVA test
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test,
#which can be used when ANNOVA assumptions are not met.

kruskal.test(H ~ TRT, data = Pius)



############### analysis for differences in seedling diameter
#Check that the data meets the assumptions of Anova
hist(Pius$D)
hist(Pius$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")

#library(rcompanion)
plotNormalHistogram(Pius$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)")

library(rcompanion)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(Pius$D)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Diameter")
abline(mean(y), sd(y))

shapiro.test(Pius$D)

#box plot  suggests a difference of means
boxplot(D ~ TRT, data = Pius, ylab = " seedling diameter(mm)", xlab = "treatments", col = "gray")

#Anova test
D_aov = aov(D ~ TRT, data = Pius)
summary(D_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(D ~ TRT, data = Pius,digits=3)


#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(D_aov), rstandard(D_aov),
     xlab = "Fitted D (mm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(D ~ TRT, data = Pius)


############### analysis for differences in number of leaves

#Check that the data meets the assumptions of Anova
hist(Pius$NL)
hist(Pius$NL, breaks=5, main="")
library(rcompanion)
plotNormalHistogram(Pius$NL, ylab = "Frequency",xlab = "Number of leaves", breaks=5)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(Pius$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Leaves")
abline(mean(y), sd(y))

shapiro.test(Pius$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = Pius, ylab = " Number of Leaves", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = Pius)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = Pius,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Leaves", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = Pius)






#########################Kenneth#################################

rm(list=ls())
setwd("C:/Documents/Anova")
ken <- read.csv("ken.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(ken)  
names(ken)

#The following commands will install FSA package if it is not already installed:
#if(!require(FSA)){install.packages("FSA")}

############### analysis for differences in seedling height

#Check that the data meets the assumptions of Anova
#To check whether the response variable (H) fits a normal distribution (bell curve), we can 
#make a histogram of the response variable: 
hist(ken$H)
hist(ken$H, ylab = "Frequency",xlab = "Seedling height (cm)")
hist(ken$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "")
hist(ken$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "", col = "gray")

shapiro.test(ken$H)

library(rcompanion)
plotNormalHistogram(ken$H, ylab = "Frequency",xlab = "Seedling height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(ken$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling height")
abline(mean(y), sd(y))

#The response variable, ‘H’, follows a bell curve, with most observations grouped toward the 
#middle of the distribution and fewer on the tails, so we can proceed with analysis.

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = ken)
boxplot(H ~ TRT, data = ken,
        ylab = "Seedling height",
        xlab = "Treatments",
        notch = FALSE,
        varwidth = TRUE)
#abline(col="black")

#Anova test
H_aov = aov(H ~ TRT, data = ken)
summary(H_aov)

# created a dataframe with a row for each Treatment. By predicting on this dataframe, we obtain the
#sample means of each Treatment. (group).
TRTs = data.frame(TRT = unique(ken$TRT))
data.frame(TRTs, meanH = predict(H_aov, TRTs))

#or
mean(ken$H[ken$TRT=="T1"])
rbind(T1=(mean(ken$H[ken$TRT=="T1"])), T2=(mean(ken$H[ken$TRT=="T2"])),
      T3=(mean(ken$H[ken$TRT=="T3"])))

#or Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = ken,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
#plot(H_aov, 1)
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted H (cm)", ylab = "Standardized Residuals") #standardized residuals
par(main = "")
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = ken)

bartlett.test(H ~ TRT, data = ken)

#From the output we can see that the p-value is not less than the significance level of
#0.05. This means that there is no evidence to suggest that the variance across groups is 
#statistically significantly different. Therefore, we can assume the homogeneity of variances 
#in the different treatment groups.

#Non-parametric alternative to one-way ANOVA test
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test,
#which can be used when ANNOVA assumptions are not met.

kruskal.test(H ~ TRT, data = ken)



############### analysis for differences in seedling diameter
#Check that the data meets the assumptions of Anova
hist(ken$D)
hist(ken$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")

#library(rcompanion)
plotNormalHistogram(ken$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)")

library(rcompanion)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(ken$D)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Diameter")
abline(mean(y), sd(y))

shapiro.test(ken$D)

#box plot  suggests a difference of means
boxplot(D ~ TRT, data = ken, ylab = " seedling diameter(mm)", xlab = "treatments", col = "gray")

#Anova test
D_aov = aov(D ~ TRT, data = ken)
summary(D_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(D ~ TRT, data = ken,digits=3)

#post hoc
#Pairwise comparisons using t tests with pooled SD
with(ken, pairwise.t.test(D, TRT, p.adj = "none"))

#P value adjustment using bonferroni method 
with(ken, pairwise.t.test(D, TRT, p.adj = "bonferroni")) 

#Tukey multiple comparisons of means
TukeyHSD(D_aov, conf.level = 0.95)
TukeyHSD(D_aov) #same results as above
#Based on these results, note which pairwise comparisons are significant. If you return to the 
#original boxplot, these results should not be surprising.

Htukey<-TukeyHSD(D_aov)
Htukey<- as.data.frame(Htukey$TRT)
#write.csv(Htukey, "C:/Documents/Anova/Htukey.csv")

#Also we can easily produce a plot of these confidence intervals.
plot(TukeyHSD(D_aov, conf.level = 0.95)) #95% family-wise confidence level
# tukey.plot<-TukeyHSD(H_aov)
# plot(tukey.plot, las = 1)
plot((TukeyHSD(D_aov, conf.level = 0.95)),las = 1) #better axis labels

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(D_aov, "TRT"))  

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(D_aov), rstandard(D_aov),
     xlab = "Fitted D (mm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(D ~ TRT, data = ken)


############### analysis for differences in number of leaves

#Check that the data meets the assumptions of Anova
hist(ken$NL)
hist(ken$NL, breaks=5, main="")
library(rcompanion)
plotNormalHistogram(ken$NL, ylab = "Frequency",xlab = "Number of leaves", breaks=5)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(ken$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Leaves")
abline(mean(y), sd(y))

shapiro.test(ken$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = ken, ylab = " Number of Leaves", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = ken)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = ken,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Leaves", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = ken)





############ see book Environmental and Ecological Statistics in R, second edition, page 52

#A method for checking the normality assumption is the normal quantile-quantile (or Q-Q) plot.
#If the sample is from a normal distribution, a straight line will form when plotting the 
#quantiles from the data against the same quantiles from the standard normal distribution. 
#The line should have an intercept of μ and a slope of σ.The respective quantiles of the 
#standard normal distribution can be estimated by using qnorm:
y <- rnorm(japh$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
abline(mean(y), sd(y))

#The calculated data quantiles are approximations. As a result, a normal QQ plot may not show 
#a perfect straight line even when the data are from a normal distribution. Instead oftyping 
#these lines, we can put them into a function:

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(japh$H)
my.qqnorm(rnorm(japh$H))












############## Francis ##############################

rm(list=ls())
setwd("C:/Documents/Anova")
fran <- read.csv("francis.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(fran)  
names(fran)

#The following commands will install FSA package if it is not already installed:
#if(!require(FSA)){install.packages("FSA")}

############### analysis for differences in seedling height

#Check that the data meets the assumptions of Anova
#To check whether the response variable (H) fits a normal distribution (bell curve), we can 
#make a histogram of the response variable: 
hist(fran$H)
hist(fran$H, ylab = "Frequency",xlab = "Seedling height (cm)")
hist(fran$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "")
hist(fran$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "", col = "gray")

shapiro.test(fran$H)

library(rcompanion)
plotNormalHistogram(fran$H, ylab = "Frequency",xlab = "Seedling height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(fran$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling height")
abline(mean(y), sd(y))

#The response variable, ‘H’, follows a bell curve, with most observations grouped toward the 
#middle of the distribution and fewer on the tails, so we can proceed with analysis.

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = fran)
boxplot(H ~ TRT, data = fran,
        ylab = "Seedling height",
        xlab = "Treatments",
        notch = FALSE,
        varwidth = TRUE)
#abline(col="black")

#Anova test
H_aov = aov(H ~ TRT, data = fran)
summary(H_aov)

# created a dataframe with a row for each Treatment. By predicting on this dataframe, we obtain the
#sample means of each Treatment. (group).
TRTs = data.frame(TRT = unique(fran$TRT))
data.frame(TRTs, meanH = predict(H_aov, TRTs))

#or
mean(fran$H[fran$TRT=="T1"])
rbind(T1=(mean(fran$H[fran$TRT=="T1"])), T2=(mean(fran$H[fran$TRT=="T2"])),
      T3=(mean(fran$H[fran$TRT=="T3"])),T4=(mean(fran$H[fran$TRT=="T4"])))

#or Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = fran,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
#plot(H_aov, 1)
plot(fitted(H_aov), residuals(H_aov, type="pearson"),
     xlab = "Fitted H (cm)", ylab = "Residuals")
abline(h=0, col="black")

plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted H (cm)", ylab = "Standardized Residuals") #standardized residuals
par(main = "")
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = fran)

bartlett.test(H ~ TRT, data = fran)

#From the output we can see that the p-value is not less than the significance level of
#0.05. This means that there is no evidence to suggest that the variance across groups is 
#statistically significantly different. Therefore, we can assume the homogeneity of variances 
#in the different treatment groups.

#Non-parametric alternative to one-way ANOVA test
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test,
#which can be used when ANNOVA assumptions are not met.

kruskal.test(H ~ TRT, data = fran)



############### analysis for differences in seedling diameter
#Check that the data meets the assumptions of Anova
hist(fran$D)
hist(fran$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")

#library(rcompanion)
plotNormalHistogram(fran$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)")

library(rcompanion)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(fran$D)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Diameter")
abline(mean(y), sd(y))

shapiro.test(fran$D)

#box plot  suggests a difference of means
boxplot(D ~ TRT, data = fran, ylab = " seedling diameter(mm)", xlab = "treatments", col = "gray")

#Anova test
D_aov = aov(D ~ TRT, data = fran)
summary(D_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(D ~ TRT, data = fran,digits=3)

#post hoc is not required for seedlind diameter based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(D_aov), rstandard(D_aov),
     xlab = "Fitted D (mm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(D ~ TRT, data = fran)




############### analysis for differences in number of leaves

#Check that the data meets the assumptions of Anova
hist(fran$NL)
hist(fran$NL, breaks=5, main="")
library(rcompanion)
plotNormalHistogram(fran$NL, ylab = "Frequency",xlab = "Number of leaves", breaks=5)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(fran$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Data")
abline(mean(y), sd(y))

shapiro.test(fran$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = fran, ylab = " Number of leaves", xlab = "treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = fran)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = fran,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Leaves", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = fran)





############ see book Environmental and Ecological Statistics in R, second edition, page 52

#A method for checking the normality assumption is the normal quantile-quantile (or Q-Q) plot.
#If the sample is from a normal distribution, a straight line will form when plotting the 
#quantiles from the data against the same quantiles from the standard normal distribution. 
#The line should have an intercept of μ and a slope of σ.The respective quantiles of the 
#standard normal distribution can be estimated by using qnorm:
y <- rnorm(japh$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
abline(mean(y), sd(y))

#The calculated data quantiles are approximations. As a result, a normal QQ plot may not show 
#a perfect straight line even when the data are from a normal distribution. Instead oftyping 
#these lines, we can put them into a function:

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(japh$H)
my.qqnorm(rnorm(japh$H))











############################ Adansonia digitata ##################################
rm(list=ls())
setwd("C:/Documents/Anova")
japh <- read.csv("japhat.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(japh)  
names(japh)

#The following commands will install FSA package if it is not already installed:
#if(!require(FSA)){install.packages("FSA")}

############### analysis for differences in seedling height

#Check that the data meets the assumptions of Anova
#To check whether the response variable (H) fits a normal distribution (bell curve), we can 
#make a histogram of the response variable: 
hist(japh$H)
hist(japh$H, ylab = "Frequency",xlab = "Seedling height (cm)")
hist(japh$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "")
hist(japh$H, ylab = "Frequency",xlab = "Seedling height (cm)", main = "", col = "gray")

shapiro.test(japh$H)

library(rcompanion)
plotNormalHistogram(japh$H, ylab = "Frequency",xlab = "Seedling height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(japh$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling height")
abline(mean(y), sd(y))

#The response variable, ‘H’, follows a bell curve, with most observations grouped toward the 
#middle of the distribution and fewer on the tails, so we can proceed with analysis.

#box plot  suggests a difference of means
plot(H ~ TRT, data = japh, col = 2:7) # col = 2:7 meams selecting colours 2 to 7 for the boxes
plot(H ~ TRT, data = japh) #without colours
plot(H ~ TRT, data = japh, ylab = " seedling height", xlab = "treatments", col = "gray") 


#or
boxplot(H ~ TRT, data = japh)
boxplot(H ~ TRT, data = japh,
        ylab = "Seedling height",
        xlab = "Treatments",
        notch = FALSE,
        varwidth = TRUE)
#abline(col="black")

#Anova test
H_aov = aov(H ~ TRT, data = japh)
summary(H_aov)

# created a dataframe with a row for each Treatment. By predicting on this dataframe, we obtain the
#sample means of each Treatment. (group).
TRTs = data.frame(TRT = unique(japh$TRT))
data.frame(TRTs, meanH = predict(H_aov, TRTs))

#or
mean(japh$H[japh$TRT=="T1"])
rbind(T1=(mean(japh$H[japh$TRT=="T1"])), T2=(mean(japh$H[japh$TRT=="T2"])),
      T3=(mean(japh$H[japh$TRT=="T3"])),T4=(mean(japh$H[japh$TRT=="T4"])), 
      T5=(mean(japh$H[japh$TRT=="T5"])),T6=(mean(japh$H[japh$TRT=="T6"])))

#or Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = japh,digits=3)

#post hoc
#Pairwise comparisons using t tests with pooled SD
with(japh, pairwise.t.test(H, TRT, p.adj = "none"))

#P value adjustment using bonferroni method 
with(japh, pairwise.t.test(H, TRT, p.adj = "bonferroni")) 

#Tukey multiple comparisons of means
TukeyHSD(H_aov, conf.level = 0.95)
TukeyHSD(H_aov) #same results as above
#Based on these results, note which pairwise comparisons are significant. If you return to the 
#original boxplot, these results should not be surprising.

Htukey<-TukeyHSD(H_aov)
Htukey<- as.data.frame(Htukey$TRT)
#write.csv(Htukey, "C:/Documents/Anova/Htukey.csv")

#Also we can easily produce a plot of these confidence intervals.
plot(TukeyHSD(H_aov, conf.level = 0.95)) #95% family-wise confidence level
# tukey.plot<-TukeyHSD(H_aov)
# plot(tukey.plot, las = 1)
plot((TukeyHSD(H_aov, conf.level = 0.95)),las = 1) #better axis labels

#Homogeneity of variances#check assumption of homoscedasticity 
#plot(H_aov, 1)
plot(fitted(H_aov), residuals(H_aov, type="pearson"),
     xlab = "Fitted H (cm)", ylab = "Residuals")
abline(h=0, col="black")

plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted H (cm)", ylab = "Standardized Residuals") #standardized residuals
par(main = "")
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = japh)

bartlett.test(H ~ TRT, data = japh)

#From the output we can see that the p-value is not less than the significance level of
#0.05. This means that there is no evidence to suggest that the variance across groups is 
#statistically significantly different. Therefore, we can assume the homogeneity of variances 
#in the different treatment groups.

# # we can the normal Q-Q plot either on the residuals or on the observed data to 
# #check assumption of homoscedasticity Typically, using residuals is more efficient as the three assumptions 
# #apply to residuals as well.
# qqnorm(residuals(H_aov, type="pearson"), main = "",
#        xlab = "Normal Quantiles", ylab = "Residuals")
# 
# qqnorm(residuals(H_aov, type="pearson"))
# qqline(residuals(H_aov, type="pearson"))
# shapiro.test(residuals(H_aov))

#box plot of residuals # not neccessary# just checking
# boxplot(residuals(H_aov, type = "pearson", level = 1)~ japh$TRT,
#         ylab = "Residuals",
#         xlab = "Plots",
#         notch = FALSE,
#         varwidth = TRUE)
# abline(h=0, col="black")


#Non-parametric alternative to one-way ANOVA test
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test,
#which can be used when ANNOVA assumptions are not met.

kruskal.test(H ~ TRT, data = japh)




############### analysis for differences in seedling diameter
#Check that the data meets the assumptions of Anova
hist(japh$D)
# h <- hist(japh$D)
# text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5)) #return values for labels using text()
hist(japh$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)", main = "", col = "gray")
#library(rcompanion)
plotNormalHistogram(japh$D, ylab = "Frequency",xlab = "Seedling Diameter(mm)",
                    breaks = c(9,10,11,12,13,14))
# ftable(japh$D)
# gg<-as.data.frame(ftable(japh$D))
# barplot(gg$Freq)

library(rcompanion)
plotNormalHistogram(japh$D, ylab = "Frequency",xlab = "Seedling height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(japh$D)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Diameter")
abline(mean(y), sd(y))

shapiro.test(japh$D)

#box plot  suggests a difference of means
boxplot(D ~ TRT, data = japh, ylab = " seedling diameter(mm)", xlab = "treatments", col = "gray")

#boxplot(japh$D[japh$TRT=="T6"])
# library(ggplot2)
# ggplot(japh, aes(x=TRT, y=D)) + geom_boxplot()

#Anova test
D_aov = aov(D ~ TRT, data = japh)
summary(D_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(D ~ TRT, data = japh,digits=3)

#post hoc is not required for seedlind diameter based on the anova results
#Tukey multiple comparisons of means
# TukeyHSD(D_aov, conf.level = 0.95)
# TukeyHSD(D_aov) #same results as above
# Dtukey<-TukeyHSD(D_aov)
# Dtukey<- as.data.frame(Dtukey$TRT)
#write.csv(Dtukey, "C:/Documents/Anova/Dtukey.csv")

#Also we can easily produce a plot of these confidence intervals.
plot((TukeyHSD(D_aov, conf.level = 0.95)),las = 1) 

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(D_aov), rstandard(D_aov),
     xlab = "Fitted D (mm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(D ~ TRT, data = japh)




############### analysis for differences in number of leaves

#Check that the data meets the assumptions of Anova
hist(japh$NL)
hist(japh$NL, breaks=5, main="")
library(rcompanion)
plotNormalHistogram(japh$NL, ylab = "Frequency",xlab = "Number of leaves", breaks=5)

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(japh$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Data")
abline(mean(y), sd(y))

shapiro.test(japh$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = japh, ylab = " Number of leaves", xlab = "treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = japh)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = japh,digits=3)

#post hoc not reqiured for number of leaves based on the anova results
#Tukey multiple comparisons of means
# TukeyHSD(NL_aov, conf.level = 0.95)
# TukeyHSD(NL_aov) #same results as above
# NLtukey<-TukeyHSD(NL_aov)
# NLtukey<- as.data.frame(NLtukey$TRT)
#write.csv(NLtukey, "C:/Documents/Anova/Dtukey.csv")
#Also we can easily produce a plot of these confidence intervals.
#plot((TukeyHSD(NL_aov, conf.level = 0.95)),las = 1) 

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted NL", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = japh)





############ see book Environmental and Ecological Statistics in R, second edition, page 52

#A method for checking the normality assumption is the normal quantile-quantile (or Q-Q) plot.
#If the sample is from a normal distribution, a straight line will form when plotting the 
#quantiles from the data against the same quantiles from the standard normal distribution. 
#The line should have an intercept of μ and a slope of σ.The respective quantiles of the 
#standard normal distribution can be estimated by using qnorm:
y <- rnorm(japh$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
abline(mean(y), sd(y))

#The calculated data quantiles are approximations. As a result, a normal QQ plot may not show 
#a perfect straight line even when the data are from a normal distribution. Instead oftyping 
#these lines, we can put them into a function:

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(japh$H)
my.qqnorm(rnorm(japh$H))


###################################     Acacia nilotica    ####################################

rm(list=ls())
setwd("C:/Documents/Anova")
corn <- read.csv("cornelius.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(corn)  
names(corn)

############### analysis for differences in seedling Height
#Check that the data meets the assumptions of Anova
hist(corn$H)
hist(corn$H, ylab = "Frequency",xlab = "Seedling Height(cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn$H, ylab = "Frequency",xlab = "Seedling Height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Height")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn$H)
my.qqnorm(rnorm(corn$H))

shapiro.test(corn$H)

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = corn, ylab = " seedling Height(cm)", xlab = "treatments", col = "gray")

#Anova test
H_aov = aov(H ~ TRT, data = corn)
summary(H_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = corn,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted seedling Height (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = corn)



###################   analysis for differences in Seedling Crown Width
#Check that the data meets the assumptions of Anova
hist(corn$CW)
hist(corn$CW, ylab = "Frequency",xlab = "Seedling Crown Width (cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn$CW, ylab = "Frequency",xlab = "Seedling Crown Width (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn$CW)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Crown Width")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn$CW)
my.qqnorm(rnorm(corn$CW))

shapiro.test(corn$CW)

#box plot  suggests a difference of means
boxplot(CW ~ TRT, data = corn, ylab = " Seedling Crown Width(cm)", xlab = "treatments", col = "gray")

#Anova test
CW_aov = aov(CW ~ TRT, data = corn)
summary(CW_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(CW ~ TRT, data = corn,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(CW_aov), rstandard(CW_aov),
     xlab = "Fitted Seedling Crown Width (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(CW ~ TRT, data = corn)



###################   analysis for differences in Number of branches
#Check that the data meets the assumptions of Anova
hist(corn$NL)
hist(corn$NL, ylab = "Frequency",xlab = "Number of leaves", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn$NL, ylab = "Frequency",xlab = "Number of leaves")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Branches")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn$NL)
my.qqnorm(rnorm(corn$NL))

shapiro.test(corn$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = corn, ylab = " Number of Branches", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = corn)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = corn,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Branches", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = corn)





###################################     Acacia seyal    ####################################

rm(list=ls())
setwd("C:/Documents/Anova")
corn2 <- read.csv("cornelius2.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(corn2)  
names(corn2)

############### analysis for differences in seedling Height
#Check that the data meets the assumptions of Anova
hist(corn2$H)
hist(corn2$H, ylab = "Frequency",xlab = "Seedling Height(cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn2$H, ylab = "Frequency",xlab = "Seedling Height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn2$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Height")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn2$H)
my.qqnorm(rnorm(corn2$H))

shapiro.test(corn2$H)

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = corn2, ylab = " seedling Height(cm)", xlab = "Treatments", col = "gray")

#Anova test
H_aov = aov(H ~ TRT, data = corn2)
summary(H_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = corn2,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted seedling Height (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = corn2)



###################   analysis for differences in Seedling Crown Width
#Check that the data meets the assumptions of Anova
hist(corn2$CW)
hist(corn2$CW, ylab = "Frequency",xlab = "Seedling Crown Width (cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn2$CW, ylab = "Frequency",xlab = "Seedling Crown Width (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn2$CW)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Crown Width")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn2$CW)
my.qqnorm(rnorm(corn2$CW))

shapiro.test(corn2$CW)

#box plot  suggests a difference of means
boxplot(CW ~ TRT, data = corn2, ylab = " Seedling Crown Width(cm)", xlab = "treatments", col = "gray")

#Anova test
CW_aov = aov(CW ~ TRT, data = corn2)
summary(CW_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(CW ~ TRT, data = corn2,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(CW_aov), rstandard(CW_aov),
     xlab = "Fitted Seedling Crown Width (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(CW ~ TRT, data = corn2)



###################   analysis for differences in Number of branches
#Check that the data meets the assumptions of Anova
hist(corn2$NL)
hist(corn2$NL, ylab = "Frequency",xlab = "Number of leaves", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(corn2$NL, ylab = "Frequency",xlab = "Number of Branches")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(corn2$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Branches")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(corn2$NL)
my.qqnorm(rnorm(corn2$NL))

shapiro.test(corn2$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = corn2, ylab = " Number of Branches", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = corn2)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = corn2,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Branches", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = corn2)





########################################## EVARISTUS RESULTS ####################
rm(list=ls())
setwd("C:/Documents/Anova")
Aki <- read.csv("Evaristus.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(Aki)  
names(Aki)

############### analysis for differences in seedling Height
#Check that the data meets the assumptions of Anova
hist(Aki$H)
hist(Aki$H, ylab = "Frequency",xlab = "Seedling Height(cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(Aki$H, ylab = "Frequency",xlab = "Seedling Height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(Aki$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Height")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(Aki$H)
my.qqnorm(rnorm(Aki$H))

shapiro.test(Aki$H)

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = Aki, ylab = " seedling Height(cm)", xlab = "treatments", col = "gray")

#Anova test
H_aov = aov(H ~ TRT, data = Aki)
summary(H_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = Aki,digits=3)

#post hoc
#Pairwise comparisons using t tests with pooled SD
with(Aki, pairwise.t.test(H, TRT, p.adj = "none"))

#P value adjustment using bonferroni method 
with(Aki, pairwise.t.test(H, TRT, p.adj = "bonferroni")) 

#Tukey multiple comparisons of means
TukeyHSD(H_aov, conf.level = 0.95)
TukeyHSD(H_aov) #same results as above
#Based on these results, note which pairwise comparisons are significant. If you return to the 
#original boxplot, these results should not be surprising.

Htukey<-TukeyHSD(H_aov)
Htukey<- as.data.frame(Htukey$TRT)
#write.csv(Htukey, "C:/Documents/Anova/AkiHtukey.csv")

#Also we can easily produce a plot of these confidence intervals.
#plot(TukeyHSD(H_aov, conf.level = 0.95)) #95% family-wise confidence level
# tukey.plot<-TukeyHSD(H_aov)
# plot(tukey.plot, las = 1)
plot((TukeyHSD(H_aov, conf.level = 0.95)),las = 1) #better axis labels

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted seedling Height (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = Aki)


###################   analysis for differences in Number of leaves
#Check that the data meets the assumptions of Anova
hist(Aki$NL)
hist(Aki$NL, ylab = "Frequency",xlab = "Number of leaves", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(Aki$NL, ylab = "Frequency",xlab = "Number of leaves")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(Aki$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Leaves")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(Aki$NL)
my.qqnorm(rnorm(Aki$NL))

shapiro.test(Aki$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = Aki, ylab = " Number of Leaves", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = Aki)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = Aki,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Leaves", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = Aki)







#############################Tamarind

rm(list=ls())
setwd("C:/Documents/Anova")
oge <- read.csv("ogechi.csv", sep=",", head=TRUE) #bring in data set to be analysed

View(oge)  
names(oge)

############### analysis for differences in seedling Height
#Check that the data meets the assumptions of Anova
hist(oge$H)
hist(oge$H, ylab = "Frequency",xlab = "Seedling Height(cm)", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(oge$H, ylab = "Frequency",xlab = "Seedling Height (cm)")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(oge$H)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Seedling Height")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(oge$H)
my.qqnorm(rnorm(oge$H))

shapiro.test(oge$H)

#box plot  suggests a difference of means
boxplot(H ~ TRT, data = oge, ylab = " seedling Height(cm)", xlab = "treatments", col = "gray")

#Anova test
H_aov = aov(H ~ TRT, data = oge)
summary(H_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(H ~ TRT, data = oge,digits=3)

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(H_aov), rstandard(H_aov),
     xlab = "Fitted seedling Height (cm)", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(H ~ TRT, data = oge)


###################   analysis for differences in Number of leaves
#Check that the data meets the assumptions of Anova
hist(oge$NL)
hist(oge$NL, ylab = "Frequency",xlab = "Number of leaves", main = "", col = "gray")

library(rcompanion)
plotNormalHistogram(oge$NL, ylab = "Frequency",xlab = "Number of leaves")

#Or using normal quantile-quantile (or Q-Q) plot #see book Environmental and Ecological Statistics in R, second edition, page 52
y <- rnorm(oge$NL)
n <- length(y)
yq <- ((1:n) - 0.5)/n
zq <- qnorm(yq, mean=0, sd=1)
#plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Data")
plot(zq, sort(y), xlab="Standard Normal Quantile", ylab="Standardized Number of Leaves")
abline(mean(y), sd(y))

my.qqnorm <- function (y=rnorm(100)){
  n <- length(y)
  yq <- ((1:n) - 0.5)/n
  zq <- qnorm(yq, mean=0, sd=1)
  plot(zq,sort(y),xlab="Standard Normal Quantile",ylab="Data")
  abline(mean(y), sd(y))
  invisible()
}
my.qqnorm(oge$NL)
my.qqnorm(rnorm(oge$NL))

shapiro.test(oge$NL)

#box plot  suggests a difference of means
boxplot(NL ~ TRT, data = oge, ylab = " Number of Leaves", xlab = "Treatments", col = "gray")

#Anova test
NL_aov = aov(NL ~ TRT, data = oge)
summary(NL_aov)

#Summarize data by group using the FSA package
library(FSA)
Summarize(NL ~ TRT, data = oge,digits=3)

#post hoc is not required for seedling height based on the anova results

#Homogeneity of variances#check assumption of homoscedasticity 
plot(fitted(NL_aov), rstandard(NL_aov),
     xlab = "Fitted Number of Leaves", ylab = "Standardized Residuals") #standardized residuals
abline(h=0, col="black")

#It’s also possible to use Bartlett’s test or Levene’s test to check the homogeneity of variances.
#We recommend Levene’s test, which is less sensitive to departures from normal distribution. 
#The function leveneTest() [in car package] will be used:
library(car) #this package is required to run this levene's test
leveneTest(NL ~ TRT, data = oge)




