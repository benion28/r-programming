# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics)

rev <- c(17906.4, 5303.72, 2700.58, 1696.77, 947.53, 362.03)
weeks <- c(1:6)

mydf <- data.frame("rev"=rev, "weeks"=weeks)

linear <- lm(formula=rev~weeks, data=mydf)
summary(linear)

para <- lm(log10(rev) ~ log10(weeks))
summary(para)

a_intercept <- para[["coefficients"]][["(Intercept)"]]
b_slope <- para[["coefficients"]][["log10(weeks)"]]

model <- nls(formula=rev ~ a*weeks^b, data=mydf, start=list(a=exp(a_intercept), b=b_slope))
summary <- summary(model)
length <- length(summary(model)$coef[,1])
newData <- data.frame(weeks=c(1:6))
predictions <- predict(model, newData)
predict_frame <- data.frame("Real Rev"=mydf$rev, "Predicted Rev"=predictions)


# SvR Model Fitting
model_svr <-  svm(formula =rev ~ weeks, data = mydf, type = "eps-regression")
summary(model_svr)
summary_model_svr <- summary(model_svr)
# Making Predictions
predictions_svr = predict(model_svr, newData)
# Calculate RSCORE
RSQUARED_svr = cor(mydf$rev, predictions_svr) # Correlation Coefficient
Accuracy1_svr <- 100 - (mean(abs((mydf$rev - predictions_svr)/mydf$rev)) * 100)
Accuracy2_svr <- accuracy(mydf$rev, predictions_svr)

# Root Mean Squared Errors
RMSE1 <- rmse(mydf$rev, predictions)
RMSE2 <- sqrt(sum((predictions -mydf$rev)^2)/(nrow(mydf)-length-1))

# Calculate RSCORE
RSQUARED = cor(mydf$rev, predictions) # Correlation Coefficient
Accuracy1 <- 100 - (mean(abs((mydf$rev - predictions)/mydf$rev)) * 100)
Accuracy2 <- accuracy(mydf$rev, predictions)

# Mean Absolute Percent Error
MAPE <- mape(mydf$rev, predictions)

# BIAS
BIAS1 <- sum(mydf$rev - predictions)/nrow(mydf)
BIAS2 <- bias(mydf$rev, predictions)

# Mean Absolute Error
MAE1 <- sum(abs(mydf$rev - predictions))/nrow(mydf)
MAE2 <- mae(mydf$rev, predictions)


# Calculate AIC
AIC <- AIC(model)

# calculate BIC
BIC <- BIC(model)

# Scattered Plot (Linear Regression)
ggplot() +
  geom_point(aes(x = mydf$weeks, y = mydf$rev, colour = "Points")) +
  geom_line(aes(x = mydf$weeks, y = predictions, colour = "NLS")) +
  geom_line(aes(x = mydf$weeks, y = mydf$rev, colour = "SVR")) +
  labs(x = "Weeks", y = "Rev", title = "NLS  Regression Line", color= "Legend") +
  theme(legend.position = c(0.95, 0.90), legend.justification = c("right", "top")) +
  scale_color_manual(values = c("Points"="red", "NLS"="green", "SVR"="blue")) 

# Export Tables
write.table(mydf, file = "mydf.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(mydf, file = "mydf.csv", sep = "", quote = FALSE, row.names = TRUE)


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
