# Load Libraries
pacman::p_load(pacman, e1071, hydroGOF)

## Prepare scatter plot

#Read data from .csv file
data=read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\tree-diameter-height.csv", header=T)
head(data)

#Scatter Plot
plot(data, main ="Scatter Plot")


## Add best-fit line to the scatter plot

#Fit linear model using OLS
Y <- data$Height
X <- data$Diameter
model=lm(Y~X,data)

#Overlay best-fit line on scatter plot
abline(model)


## Scatter plot displaying actual values and predicted values 

#Scatter Plot
plot (data, pch=16)

#Predict Y using Linear Model
predY <- predict (model, data)

#Overlay Predictions on Scatter Plot
points (data$Diameter, predY, col = "blue", pch=16)


## RMSE Calculation for linear model

#Calculate RMSE 
RMSE=rmse(predY,data$Diameter)



#Scatter Plot
plot(data)

#Regression with SVM
modelsvm = svm(Y~X,data)

#Predict using SVM regression
predYsvm = predict(modelsvm, data)

#Overlay SVM Predictions on Scatter Plot
points(data$Diameter, predYsvm, col = "red", pch=16)


##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho


## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,data$Height)


## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Y~X, data=data,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)


## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,data)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,data$Height)


##Calculate parameters of the Best SVR model

#Find value of W
W = t(BstModel$coefs) %*% BstModel$SV

#Find value of b
b = BstModel$rho


## Plotting SVR Model and Tuned Model in same plot

plot(data, pch=16)
points(data$X, predYsvm, col = "blue", pch=3)
points(data$X, PredYBst, col = "red", pch=4)
points(data$X, predYsvm, col = "blue", pch=3, type="l")
points(data$X, PredYBst, col = "red", pch=4, type="l")

