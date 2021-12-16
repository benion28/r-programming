library(MASS)
data("Boston")

# For data description
?Boston

# we will split the data into training and testing sets
set.seed(2)
library(caTools) # sample.split function is present in this package

split <- sample.split(Boston$medv, SplitRatio = 0.7)
# we divided the data with the ratio 0.7
split

training_data <- subset(Boston, split == "TRUE")
testing_data <- subset(Boston, split == "FALSE")

# to view the correlation of variables
plot(Boston$crim, Boston$medv, cex = 0.5, xlab = "Crime_Rate", ylab = "Price")
correlation_of_variables <- cor(Boston)

# creating scatterplot matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6, 14)], groups = NULL, data = Boston, axis.line.tck = 0, axis.text.alpha = 0)
splom(~Boston[c(7:14)], groups = NULL, data = Boston, axis.line.tck = 0, axis.text.alpha = 0)

# studying rm and medv
plot(rm, medv)
abline(lm(medv ~ rm), col = "red") # Regression fit line

# we can use corplot to visualize
library(corrplot)

corrplot(correlation_of_variables, type = "lower")
corrplot(correlation_of_variables, method = "number")

# finding multicollinearity
library(caret)

# to exclude medv(output)
Boston_no_medv <- subset(Boston, select = -c(medv))
numericData <- Boston_no_medv[sapply(Boston_no_medv, is.numeric)]
describe_correlation <- cor(numericData)

# vif
library(car)
model <- lm(medv ~ ., data = training_data)
vif(model)

# now to create the model we will use all columns
model <- lm(medv ~ ., data = training_data)

# for description of the model
summary(model)

# model after removing insignificant variables "age and indus"
model <- lm(medv ~ . -age -indus, data = training_data)
summary(model)

# now we can use this model to predict the output of our test set
predictions <- predict(model, testing_data)

# to compare predicted values and actual values, we can use plots
plot(testing_data$medv, type = "l", lty = 1.8,  col = "green")
lines(predictions, type = "l", col = "blue")

# now we can use this model to predict the output of sample dataset
