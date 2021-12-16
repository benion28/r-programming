


plot(fitted(model_logistics), residuals(model_logistics, type = "pearson"),
     xlab = "Predicted H (m)", ylab = "Residuals")
abline(h=0)
#mtext("Logistic Model")
title ("Logistic Model", line = 1, adj = 0.5)

qqnorm(residuals(model_logistics, type = "pearson"), main = "", 
                 xlab = "Normal Quantiles", ylab = "Residuals")
qqline(residuals(model_logistics,type = "pearson")) 

y <- rnorm(resid(model_logistics))
n <- length(y)
yq <-((1:n) - 0.5)/n
zq <-qnorm(yq, mean = 0, sd=1)
plot(zq, sort(y), xlab = "Normal Quantiles", ylab = "Residuals")
abline(mean(y), sd(y))

#shapiro.test(residuals(model_logistics))
#hist(residuals(model_logistics))


#Validation


svr <- predictions_svr
ann <- predictions_ann
pwr <- predictions_power
lgs <- predictions_logistics
chpm <- predictions_chapman
web <- predictions_weibull

#validation predictions
VP <- cbind(as.data.frame(svr), as.data.frame(ann),as.data.frame(pwr),
            as.data.frame(lgs),as.data.frame(chpm),as.data.frame(web))
head(VP)
head(testing)
names(testing)

#removing unrequired columns
testing <- testing[,-c(1:4)]

#merging predictions and Height data
Vhd <- cbind(H = testing, VP)
head(Vhd)

#computing error/bias in height predictions
library(dplyr)
VS <- mutate(Vhd, Rsvr = H - svr, RANN = H - V1, Rpwr = H - pwr, Rlgs = H - lgs,
             Rchpm = H - chpm, Rweb = H - web)


head(VS)
RVS <- VS[,-c(1:7)] # removing columns 1 t0 7

head(RVS)
# where 2 specify that it should be applied to column
apply (RVS, 2 ,sd) # standard deviation #RMSE
apply (RVS, 2 ,mean) #bias
sapply(RVS, function(x)sd(x)/sqrt(length(x))) #Standard error

Val_stats <- rbind(sd = (apply(RVS, 2 ,sd)), bias = (apply (RVS, 2 ,mean)), 
      se = (sapply(RVS, function(x)sd(x)/sqrt(length(x)))))
      
mean (residuals(model_power))            
sd(residuals(model_power))             



#training models             
m_svr <- residuals(model_svr)
m_ann <- residuals(model_ann)
m_pwr <- residuals(model_power)
m_lgs <- residuals(model_logistics)
m_chpm <- residuals(model_chapman)
m_web <- residuals(model_weibull)

#model evaluation stats
ME <- cbind(as.data.frame(m_svr),as.data.frame(m_pwr),
      as.data.frame(m_lgs),as.data.frame(m_chpm),as.data.frame(m_web))
head (ME)
dim (ME)

rbind(sd = (apply(ME, 2 ,sd)), bias = (apply (ME, 2 ,mean)), 
      se = (sapply(ME, function(x)sd(x)/sqrt(length(x)))))

#R squares
1-(var(residuals(model_power))/var(training$HEIGHT))




