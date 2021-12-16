# Load Libraries
pacman::p_load(pacman, caTools, e1071, hydroGOF, ggplot2)

# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\tree-diameter-height.csv", head=TRUE, sep=",")
head(dataset, n = 10)
View(dataset)

# EXploratory Data Analysis
ggplot() +
  geom_point(aes(x = dataset$Diameter, y = dataset$Height), colour = "red") +
  ggtitle("Tree Height-Diameter Relationship") +
  xlab("Diameter") +
  ylab("Height")

# Data Splicing
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)

# Model Fitting
model <-  svm(formula = Height ~ Diameter, data = training, type = "eps-regression")
summary(model)

# Linear Regression
lmm <- lm(formula = Height ~ Diameter, data = training)
summary(lmm)
y_predict = predict(lmm, testing)
RMSE2=rmse(y_predict,testing$Height)
RSQUARE2 = 0.003898 * 100

# Making Predictions
predictions = predict(model, testing)

#Calculate RMSE 
RMSE=rmse(predictions,testing$Height)

# Calculate RSCORE
RSQUARE = cor(testing$Height, predictions)
MAPE = mean(abs((testing$Height - predictions)/testing$Height)) * 100
Accuracy = 100 - MAPE

# Table of Real Height vs Predicted Height
data_frame <- data.frame("Real Height"=testing$Height, "Predicted Height"=predictions)

# EXploratory Data Analysis (Regression Line)
# Scattered Plot
ggplot() +
  geom_point(aes(x = testing$Diameter, y = testing$Height), colour = "red") +
  geom_line(aes(x = testing$Diameter, y = predictions), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Regression Line)") +
  xlab("Diameter") +
  ylab("Height")

# Regression Line
ggplot() +
  geom_line(aes(x = testing$Diameter, y = predictions), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Regression Line)") +
  xlab("Diameter") +
  ylab("Height")

# Histogram (Real Height)
ggplot(data_frame, aes(x = testing$Height)) +
  geom_histogram(bins = 30, colour = "red") +
  labs(x = "", y = "Real Height")

# Histogram (Predicted Height)
ggplot(data_frame, aes(x = predictions)) +
  geom_histogram(bins = 30, colour = "green") +
  labs(x = "", y = "Predicted Height")

# Model Comparison Frame
prediction_frame <- data.frame("Real Values"=testing$Height,"SVR_Predict"=predictions, "LM_Predict"=y_predict)

