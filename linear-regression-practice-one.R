# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics)


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\example-2.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
description <- describe(dataset)

# Improved Dataset
dbh_ht <- dataset$dbh.cm. * dataset$ht.m.
vol_y <- dataset$vol.m.3.
x2 <- dbh_ht ^2
y2 <- dataset$vol.m.3. ^2
xy <- dbh_ht * dataset$vol.m.3.
improved_dataset <- data.frame(
  "Tree No"=c(dataset$Tree.No, "Totals"),
  "x"=c(round(dbh_ht, 2), round(sum(dbh_ht), 2)),
  "y"=c(round(vol_y, 4), round(sum(vol_y), 4)),
  "x2"=c(round(x2, 2), round(sum(x2), 2)),
  "y2"=c(round(y2, 4), round(sum(y2), 4)),
  "xy"=c(round(xy, 2), round(sum(xy), 2))
  )


# EXploratory Data Analysis
ggplot() +
  geom_point(aes(x = improved_dataset$x, y = improved_dataset$y), colour = "red") +
  ggtitle("Volume and Height-Diameter Relationship") +
  labs(x = "x = (dbh * ht)", y = "y = vol(cubic meters)")

# Mean
x_mean <- mean(dbh_ht)
y_mean <- mean(vol_y)

# Parameters
n <- length(dataset$Tree.No)
x <- improved_dataset$x
y <- improved_dataset$y


# Sum of Squares
SPXY <- (sum(xy)) - ((sum(dbh_ht) * sum(vol_y))/n)
SSX <- (sum(x2)) - ((sum(dbh_ht) ^2)/n)

# Slope
b1 <- SPXY/SSX

# Intercept
b0 <- y_mean - (b1 * x_mean)

# Sum of Squares
SSY <- (sum(y2)) - ((sum(vol_y) ^2)/n)
SSR <- b1 * SPXY
SSE_m <- SSY - SSR


# R-Squared
R2_1 <- SSR/SSY
R2_2 <- (SPXY/(SSX * SSY) ^ 0.5) ^ 2

# Errors
MSE_m <- ((SSE_m)/(n - 2)) 
SE_m <-  MSE_m ^ 0.5

# Volume Estimation
predict_frame <- data.frame("dbh.cm."=18.2, "ht.m."=13.65)
y_m <- b0 + (b1 * (predict_frame$dbh * predict_frame$ht))

# ML
# Data Splicing
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)

# Model
model <- lm(formula = vol.m.3. ~ dbh.cm.+ht.m., data = training)
summary(model)

# Predictions
predictions <- predict(model, testing)
y_predict <- predict(model, predict_frame)

# Errors
MSE_l <- mse(testing$vol.m.3., predictions)
RMSE_l <- rmse(testing$vol.m.3., predictions)
MAPE_l = mean(abs((testing$vol.m.3. - predictions)/testing$vol.m.3.)) * 100
Accuracy_l <- 100 - MAPE_l
SE_l <- se(testing$vol.m.3., predictions)
R2_l <- cor(testing$vol.m.3., predictions)

# Prediction Frame
prediction_frame <- data.frame(
  "Tree No"=c(testing$Tree.No),
  "Real Vol"=c(testing$vol.m.3.),
  "Predicted Vol"=c(predictions)
)

# Regression Line
ggplot() +
  geom_point(aes(x = testing$ht.m.*testing$dbh.cm., y = testing$vol.m.3.), colour = "red") +
  geom_line(aes(x = testing$ht.m.*testing$dbh.cm., y = predictions), colour = "blue") +
  ggtitle("Volume and Height-Diameter Relationship") +
  labs(x = "x = (dbh * ht)", y = "y = vol(cubic meters)")
