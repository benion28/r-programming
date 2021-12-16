# Load Libraries
pacman::p_load(pacman, drc, nlme, ggplot2)

# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\tree-diameter-height.csv", head=TRUE, sep=",")
head(dataset, n = 10)

# Technvidan Michaelis-Menten kinetics
MM.model <- drm(Height ~ Diameter, data = dataset, fct=MM.2())
summary(MM.model)
predictions <- predict(MM.model, dataset)
mm_df = data.frame("Actual Diameter"=dataset$Diameter, "Actual Height"=dataset$Height, "Predicted Height"=predictions)
