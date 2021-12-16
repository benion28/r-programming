# Load Libraries
pacman::p_load(pacman, e1071, ggplot2)


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\tree-diameter-height.csv")
dataset <-  dataset[2:3]
head(dataset, n = 10)


# Polynomial Regression
# taking care of missing values
# Test for missing values
sum(is.na(dataset$Height))
sum(is.na(dataset$Diameter))


# fitting the regression model to the dataset
regressor <-  svm(formula = Height ~ ., data = dataset, type = "eps-regression")
# for svm classification you use c-classification
summary(regressor)


# predicting a new result 
y_pred = predict(regressor, data.frame(Diameter = 11.313417))
y_pred


# Fitting the  regression model
# add a new column in the dataframe
# Plot
ggplot() +
  geom_point(aes(x = dataset$Diameter, y = dataset$Height), colour = "red") +
  geom_line(aes(x = dataset$Diameter, y = predict(regressor, newdata = dataset)), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Regression Model)") +
  xlab("Diameter") +
  ylab("Height")


# visualizing polynomial regression
x_grid = seq(min(dataset$Diameter), max(dataset$Diameter), 0.1 )
ggplot() +
  geom_point(aes(x = dataset$Diameter, y = dataset$Height), colour = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Diameter = x_grid))), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Regression Model - Polynomial)") +
  xlab("Diameter") +
  ylab("Height")



# p_unload(all)
