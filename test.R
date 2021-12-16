#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics, neuralnet, lmfor, dplyr, nlme)

# Set Working Directory
setwd("C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project")


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\benion-tree-height-diameter-dataset.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))


#------------------------------ Data Splicing -------------------------------#
split <- sample.split(dataset, SplitRatio = 0.8)

# Training or Fitting Data
training <- subset(dataset, split==TRUE)
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- subset(dataset, split==FALSE)
summary(testing)
testing_description <- describe(testing)


#-------------------------------- ANN Model ------------------------------#
# ANN Model Fitting
model_ann <-  neuralnet(formula = HEIGHT ~ DIAMETER, data = training, hidden = 2, linear.output = TRUE)
summary(model_ann)
summary_model_ann <- summary(model_ann)

# Making Predictions
predictions_ann = predict(model_ann, testing)



start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_logistics <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull
