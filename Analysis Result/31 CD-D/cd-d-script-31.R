#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, caTools, ggplot2, psych, tidyverse, Metrics, lmfor, dplyr, nlme)


# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//31 CD-D")


# Import data set
dataset <-  read.csv("full-data-13.csv", head=TRUE, sep=",")

# Filter Dataset (Azadirachta indica)
dataset <- filter(dataset, dataset$SPECIES == "Azadirachta indica")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
r_dataset_value <- cor(dataset$DIAMETER, dataset$CROWN.DIAMETER)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset

#-------------------- EXploratory Data Analysis ----------------------------#

# All Species Distribution Plot
ggplot() +
  geom_point(aes(x = dataset$DIAMETER, y = dataset$CROWN.DIAMETER), colour = "#a32e2e") +
  ggtitle("Tree Crown Diameter vs Diameter (Data Distribution)") +
  xlab("Diameter") +
  ylab("Crown Diameter")

#------------------------------ Frequency Distribution -----------------------------#

# Species
species_list <- c()
for (variable in unique_species_dataset$Unique.Species) {
  item <- species_list
  species_list <- append(item, variable)
}

# Frequency
species_frequency <- c()
for (variable in unique_species_dataset$Unique.Species) {
  item <- species_frequency
  total <- length(dataset$SPECIES[dataset$SPECIES == variable])
  species_frequency <- append(item, total)
}

# Frequency Table
frequency_table <- data.frame(
  "Species" = species_list,
  "Frequency" = species_frequency
)
frequency_table <- rbind(frequency_table, "Total" = c("Total", sum(frequency_table$Frequency)))

# Training or Fitting Data
training <- read.csv("training-data-13.csv", head=TRUE, sep=",")
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- read.csv("testing-data-13.csv", head=TRUE, sep=",")
summary(testing)
testing_description <- describe(testing)

# Descriptive Statistic Table
descriptive_statistcic_table <- data.frame(
  "Training_Diameter" = c(round(mean(training$DIAMETER), 2), round(min(training$DIAMETER), 2), round(max(training$DIAMETER, 2)), round(SD(training$DIAMETER), 2)),
  "Testing_Diameter" = c(round(mean(testing$DIAMETER), 2), round(min(testing$DIAMETER), 2), round(max(testing$DIAMETER), 2), round(SD(testing$DIAMETER), 2)),
  "All_Diameter" = c(round(mean(dataset$DIAMETER), 2), round(min(dataset$DIAMETER), 2), round(max(dataset$DIAMETER), 2), round(SD(dataset$DIAMETER), 2)),
  "Training_Crown_Diameter" = c(round(mean(training$CROWN.DIAMETER), 2), round(min(training$CROWN.DIAMETER), 2), round(max(training$CROWN.DIAMETER), 2), round(SD(training$CROWN.DIAMETER), 2)),
  "Testing_Crown_Diameter" = c(round(mean(testing$CROWN.DIAMETER), 2), round(min(testing$CROWN.DIAMETER), 2), round(max(testing$CROWN.DIAMETER), 2), round(SD(testing$CROWN.DIAMETER), 2)),
  "All_Crown_Diameter" = c(round(mean(dataset$CROWN.DIAMETER), 2), round(min(dataset$CROWN.DIAMETER), 2), round(max(dataset$CROWN.DIAMETER), 2), round(SD(dataset$CROWN.DIAMETER), 2))
)
descriptive_statistcic_table <- rbind(descriptive_statistcic_table, c(
  length(training$DIAMETER), length(testing$DIAMETER), length(dataset$DIAMETER), length(training$CROWN.DIAMETER), length(testing$CROWN.DIAMETER), length(dataset$CROWN.DIAMETER)))
rownames(descriptive_statistcic_table) <- c("Mean", "Minimum", "Maximum", "Standard_Deviation", "Obsersavations(n)")

# Starting Values 
init_values <- list(a = 1, b = 1, c = 1)


# ----------------------------- Model 1 ----------------------------- #
# Model 1 Fitting
model_1 <- nls(formula = CROWN.DIAMETER ~ (a * (DIAMETER^b) + c), 
               data = training, 
               start = list(a = init_values$a, b = init_values$b, c = init_values$c))
summary_model_1 <- summary(model_1)
length_model_1 <- length(summary(model_1)$coef[,1])
model_1


# Making Predictions
predictions_model_1_training = predict(model_1, training)
predictions_model_1_testing = predict(model_1, testing)


# ----------------------------- Model 2 ----------------------------- #
# Model 2 Fitting
model_2 <- nls(formula = CROWN.DIAMETER ~ ((a + (b * (DIAMETER^2)))), 
               data = training, 
               start = list(a = init_values$a, b = init_values$b))
summary_model_2 <- summary(model_2)
length_model_2 <- length(summary(model_2)$coef[,1])
model_2

# Making Predictions
predictions_model_2_training = predict(model_2, training)
predictions_model_2_testing = predict(model_2, testing)


# ----------------------------- Model 3 ----------------------------- #
# Model 3 Fitting
model_3 <- nls(formula = CROWN.DIAMETER ~ (a + b * (log(DIAMETER))), 
               data = training, 
               start = list(a = init_values$a, b = init_values$b))
summary_model_3 <- summary(model_3)
length_model_3 <- length(summary(model_3)$coef[,1])
model_3

# Making Predictions
predictions_model_3_training = predict(model_3, training)
predictions_model_3_testing = predict(model_3, testing)


# ----------------------------- Model 4 ----------------------------- #
# Model 4 Fitting
model_4 <- nls(formula = CROWN.DIAMETER ~ (a + b * (DIAMETER) + c * (DIAMETER^2)), 
               data = training, 
               start = list(a = init_values$a, b = init_values$b, c = init_values$c))
summary_model_4 <- summary(model_4)
length_model_4 <- length(summary(model_4)$coef[,1])
model_4

# Making Predictions
predictions_model_4_training = predict(model_4, training)
predictions_model_4_testing = predict(model_4, testing)


# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$CROWN.DIAMETER, 
  "Model 1_Predictions"=round(predictions_model_1_training, 1),
  "Model 2_Predictions"=round(predictions_model_2_training, 1),
  "Model 3_Predictions"=round(predictions_model_3_training, 1),
  "Model 4_Predictions"=round(predictions_model_4_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$CROWN.DIAMETER, 
  "Model 1_Predictions"=round(predictions_model_1_testing, 1),
  "Model 2_Predictions"=round(predictions_model_2_testing, 1),
  "Model 3_Predictions"=round(predictions_model_3_testing, 1),
  "Model 4_Predictions"=round(predictions_model_4_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()

# ---------------------- Manual Predictions -------------------------------#
predictor_variable <- data.frame(DIAMETER = 21.6)

# Model 1
predict_model_1 <- predict(model_1, predictor_variable)

# Model 2
predict_model_2 <- predict(model_2, predictor_variable)

# Model 3
predict_model_3 <- predict(model_3, predictor_variable)

# Model 4
predict_model_4 <- predict(model_4, predictor_variable)

# Predict Table
predict_table <- data.frame(
  "DIAMETER" = predictor_variable$DIAMETER,
  "CROWN.DIAMETER" = 13.2,
  "Model 1" = round(predict_model_1, 1),
  "Model 2" = round(predict_model_2, 1),
  "Model 3" = round(predict_model_3, 1),
  "Model 4" = round(predict_model_4, 1)
)


#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(
    round(summary_model_1$parameters[1], 2), round(summary_model_2$parameters[1], 2), 
    round(summary_model_3$parameters[1], 2), round(summary_model_4$parameters[1], 2)
  ),
  "b" = c(
    round(summary_model_1$parameters[2], 2), round(summary_model_2$parameters[2], 2), 
    round(summary_model_3$parameters[2], 2), round(summary_model_4$parameters[2], 2)
  ),
  "c" = c(
    round(summary_model_1$parameters[3], 3), "-",
    "-", round(summary_model_4$parameters[3], 3)
  )
)
rownames(parameters_nls) <- c("Model 1", "Model 2", "Model 3", "Model 4")



#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_model_1_training <- rmse(training$CROWN.DIAMETER, predictions_model_1_training)
RMSE_model_2_training <- rmse(training$CROWN.DIAMETER, predictions_model_2_training)
RMSE_model_3_training <- rmse(training$CROWN.DIAMETER, predictions_model_3_training)
RMSE_model_4_training <- rmse(training$CROWN.DIAMETER, predictions_model_4_training)
RMSE_training <- c(
  round(RMSE_model_1_training, 4), 
  round(RMSE_model_2_training, 4), 
  round(RMSE_model_3_training, 4), 
  round(RMSE_model_4_training, 4)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_model_1_testing <- rmse(testing$CROWN.DIAMETER, predictions_model_1_testing)
RMSE_model_2_testing <- rmse(testing$CROWN.DIAMETER, predictions_model_2_testing)
RMSE_model_3_testing <- rmse(testing$CROWN.DIAMETER, predictions_model_3_testing)
RMSE_model_4_testing <- rmse(testing$CROWN.DIAMETER, predictions_model_4_testing)
RMSE_testing <- c(
  round(RMSE_model_1_testing, 3), 
  round(RMSE_model_2_testing, 3), 
  round(RMSE_model_3_testing, 3), 
  round(RMSE_model_4_testing, 3)
)
summary_table_testing <- data.frame("RMSE" = RMSE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_model_1_training <- cor(training$CROWN.DIAMETER, predictions_model_1_training)^2 # Correlation Coefficient
RSQUARED_model_2_training <- cor(training$CROWN.DIAMETER, predictions_model_2_training)^2 # Correlation Coefficient
RSQUARED_model_3_training <- cor(training$CROWN.DIAMETER, predictions_model_3_training)^2 # Correlation Coefficient
RSQUARED_model_4_training <- cor(training$CROWN.DIAMETER, predictions_model_4_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_model_1_training, 4), 
  round(RSQUARED_model_2_training, 4), 
  round(RSQUARED_model_3_training, 4), 
  round(RSQUARED_model_4_training, 4)
)
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_model_1_testing <- cor(testing$CROWN.DIAMETER, predictions_model_1_testing)^2 # Correlation Coefficient
RSQUARED_model_2_testing <- cor(testing$CROWN.DIAMETER, predictions_model_2_testing)^2 # Correlation Coefficient
RSQUARED_model_3_testing <- cor(testing$CROWN.DIAMETER, predictions_model_3_testing)^2 # Correlation Coefficient
RSQUARED_model_4_testing <- cor(testing$CROWN.DIAMETER, predictions_model_4_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_model_1_testing, 4), 
  round(RSQUARED_model_2_testing, 4), 
  round(RSQUARED_model_3_testing, 4), 
  round(RSQUARED_model_4_testing, 4)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Training
MAPE_model_1_training <- mape(training$CROWN.DIAMETER, predictions_model_1_training)
MAPE_model_2_training <- mape(training$CROWN.DIAMETER, predictions_model_2_training)
MAPE_model_3_training <- mape(training$CROWN.DIAMETER, predictions_model_3_training)
MAPE_model_4_training <- mape(training$CROWN.DIAMETER, predictions_model_4_training)
MAPE_training <- c(
  round(MAPE_model_1_training, 3),
  round(MAPE_model_2_training, 3),
  round(MAPE_model_3_training, 3),
  round(MAPE_model_4_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAPE" = MAPE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Testing
MAPE_model_1_testing <- mape(testing$CROWN.DIAMETER, predictions_model_1_testing)
MAPE_model_2_testing <- mape(testing$CROWN.DIAMETER, predictions_model_2_testing)
MAPE_model_3_testing <- mape(testing$CROWN.DIAMETER, predictions_model_3_testing)
MAPE_model_4_testing <- mape(testing$CROWN.DIAMETER, predictions_model_4_testing)
MAPE_testing <- c(
  round(MAPE_model_1_testing, 3),
  round(MAPE_model_2_testing, 3),
  round(MAPE_model_3_testing, 3),
  round(MAPE_model_4_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAPE" = MAPE_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_model_1_training <- bias(training$CROWN.DIAMETER, predictions_model_1_training)
bias_model_2_training <- bias(training$CROWN.DIAMETER, predictions_model_2_training)
bias_model_3_training <- bias(training$CROWN.DIAMETER, predictions_model_3_training)
bias_model_4_training <- bias(training$CROWN.DIAMETER, predictions_model_4_training)
BIAS_training <- c(
  round(bias_model_1_training, 3), 
  round(bias_model_2_training, 3), 
  round(bias_model_3_training, 3), 
  round(bias_model_4_training, 3)
)
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_model_1_testing <- bias(testing$CROWN.DIAMETER, predictions_model_1_testing)
bias_model_2_testing <- bias(testing$CROWN.DIAMETER, predictions_model_2_testing)
bias_model_3_testing <- bias(testing$CROWN.DIAMETER, predictions_model_3_testing)
bias_model_4_testing <- bias(testing$CROWN.DIAMETER, predictions_model_4_testing)
BIAS_testing <- c(
  round(bias_model_1_testing, 3), 
  round(bias_model_2_testing, 3), 
  round(bias_model_3_testing, 3), 
  round(bias_model_4_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Error (MAE)
# Training
MAE_model_1_training <- mae(training$CROWN.DIAMETER, predictions_model_1_training)
MAE_model_2_training <- mae(training$CROWN.DIAMETER, predictions_model_2_training)
MAE_model_3_training <- mae(training$CROWN.DIAMETER, predictions_model_3_training)
MAE_model_4_training <- mae(training$CROWN.DIAMETER, predictions_model_4_training)
MAE_training <- c(
  round(MAE_model_1_training, 3), 
  round(MAE_model_2_training, 3), 
  round(MAE_model_3_training, 3), 
  round(MAE_model_4_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAE" = MAE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Error (MAE)
# Testing
MAE_model_1_testing <- mae(testing$CROWN.DIAMETER, predictions_model_1_testing)
MAE_model_2_testing <- mae(testing$CROWN.DIAMETER, predictions_model_2_testing)
MAE_model_3_testing <- mae(testing$CROWN.DIAMETER, predictions_model_3_testing)
MAE_model_4_testing <- mae(testing$CROWN.DIAMETER, predictions_model_4_testing)
MAE_testing <- c(
  round(MAE_model_1_testing, 3), 
  round(MAE_model_2_testing, 3), 
  round(MAE_model_3_testing, 3), 
  round(MAE_model_4_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAE" = MAE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_model_1 <- AIC(model_1)
AIC_model_2 <- AIC(model_2)
AIC_model_3 <- AIC(model_3)
AIC_model_4 <- AIC(model_4)
AIC <- c(
  round(AIC_model_1, 3),
  round(AIC_model_2, 3),
  round(AIC_model_3, 3),
  round(AIC_model_4, 3)
)
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)


# calculate BIC
BIC_model_1 <- BIC(model_1)
BIC_model_2 <- BIC(model_2)
BIC_model_3 <- BIC(model_3)
BIC_model_4 <- BIC(model_4)
BIC <- c(
  round(BIC_model_1, 2),
  round(BIC_model_2, 2),
  round(BIC_model_3, 2),
  round(BIC_model_4, 2)
)
summary_table_training <- cbind(summary_table_training, BIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, BIC)

#----------------------------- Model Evaluation -----------------------------#
# Accuracy
# Training
accuracy_model_1_training <- accuracy(training$CROWN.DIAMETER, prediction_frame_training$Model.1_Predictions)
accuracy_model_2_training <- accuracy(training$CROWN.DIAMETER, prediction_frame_training$Model.2_Predictions)
accuracy_model_3_training <- accuracy(training$CROWN.DIAMETER, prediction_frame_training$Model.3_Predictions)
accuracy_model_4_training <- accuracy(training$CROWN.DIAMETER, prediction_frame_training$Model.4_Predictions)
accuracy_training <- c(
  round((accuracy_model_1_training * 100), 2), 
  round((accuracy_model_2_training * 100), 2), 
  round((accuracy_model_3_training * 100), 2), 
  round((accuracy_model_4_training * 100), 2)
)
summary_table_training <- cbind(summary_table_training, "Accuracy(%)" = accuracy_training)

#----------------------------- Model Validation -----------------------------#
# Accuracy
# Testing
accuracy_model_1_testing <- accuracy(testing$CROWN.DIAMETER, prediction_frame_testing$Model.1_Predictions)
accuracy_model_2_testing <- accuracy(testing$CROWN.DIAMETER, prediction_frame_testing$Model.2_Predictions)
accuracy_model_3_testing <- accuracy(testing$CROWN.DIAMETER, prediction_frame_testing$Model.3_Predictions)
accuracy_model_4_testing <- accuracy(testing$CROWN.DIAMETER, prediction_frame_testing$Model.4_Predictions)
accuracy_testing <- c(
  round((accuracy_model_1_testing * 100), 2), 
  round((accuracy_model_2_testing * 100), 2), 
  round((accuracy_model_3_testing * 100), 2), 
  round((accuracy_model_4_testing * 100), 2)
)
summary_table_testing <- cbind(summary_table_testing, "Accuracy(%)" = accuracy_testing)

rownames(summary_table_training) <- c("Model 1", "Model 2", "Model 3", "Model 4")
rownames(summary_table_testing) <- c("Model 1", "Model 2", "Model 3", "Model 4")

# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$CROWN.DIAMETER, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_1_training, colour = "Model 1")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_2_training, colour = "Model 2")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_3_training, colour = "Model 3")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_4_training, colour = "Model 4")) +
  labs(x = "Diameter (cm)", y = "Crown Diameter (m)", title = "Tree Crown Diameter - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Model 1"="#0f0f10",
    "Model 2"="#eb7de3",
    "Model 3"="#eb1919",
    "Model 4"="#c9b424"
  )) 

# Model Residuals Table
residuals_table <- data.frame(
  "Model 1" = residuals(model_1),
  "Model 2" = residuals(model_2),
  "Model 3" = residuals(model_3),
  "Model 4" = residuals(model_4)
)

# Scattered Plot (Model Performance Line) -- Model 1
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$CROWN.DIAMETER, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_1_training, colour = "Model 1")) +
  labs(x = "Diameter (cm)", y = "Crown Diameter (m)", title = "Tree Crown Diameter - Diameter Model Line (Model 1)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 1"="#120c4c")) 


# Scattered Plot (Model Performance Curve) -- Model 2
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$CROWN.DIAMETER, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_2_training, colour = "Model 2")) +
  labs(x = "Diameter (cm)", y = "Crown Diameter (m)", title = "Tree Crown Diameter - Diameter Model Curve (Model 2)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 2"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Model 3
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$CROWN.DIAMETER, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_3_training, colour = "Model 3")) +
  labs(x = "Diameter (cm)", y = "Crown Diameter (m)", title = "Tree Crown Diameter - Diameter Model Curve (Model 3)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 3"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Model 4
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$CROWN.DIAMETER, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_4_training, colour = "Model 4")) +
  labs(x = "Diameter (cm)", y = "Crown Diameter (m)", title = "Tree Crown Diameter - Diameter Model Curve (Model 4)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 4"="#120c4c")) 

# Scattered Plot (Fitted VS Residuals Values) -- Model 1
ggplot() +
  geom_point(aes(x = fitted(model_1), y = residuals(model_1), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Model 1")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Crown Diameter - Diameter -- Fitted vs Residuals (Model 1)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 1"="#4954ed")) 

plot(model_1, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Model 1",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Fitted VS Residuals Values) -- Model 2
ggplot() +
  geom_point(aes(x = fitted(model_2), y = residuals(model_2), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Model 2")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Crown Diameter - Diameter -- Fitted vs Residuals (Model 2)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Model 2"="#4954ed")) 

plot(model_2, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Model 2",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Model 1
ggplot(residuals_table, aes(sample = Model.1)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree CD-D Standardized Normal Quantile vs Residuals (Model 1)")

qqnorm(residuals(model_1, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Model 1", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_1,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Model 2
ggplot(residuals_table, aes(sample = Model.2)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree CD-D Standardized Normal Quantile vs Residuals (Model 2)")

qqnorm(residuals(model_2, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Model 2", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_2,type = "pearson"))

#------------------------------ Number of Trees per Hectar --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 7
expansion_factor <- one_ha_m2 / (area_of_plot_m2 * number_of_plots)

# Diameter Classes
class_1_5_dbh <- dataset %>% filter(DIAMETER > 1 & DIAMETER <= 5)
class_5_10_dbh <- dataset %>% filter(DIAMETER > 5 & DIAMETER <= 10)
class_10_15_dbh <- dataset %>% filter(DIAMETER > 10 & DIAMETER <= 15)
class_15_20_dbh <- dataset %>% filter(DIAMETER > 15 & DIAMETER <= 20)
class_20_25_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 25)
class_25_dbh <- dataset %>% filter(DIAMETER > 25)

# Crown Diameter Classes
class_1_5_cd <- dataset %>% filter(CROWN.DIAMETER > 1 & CROWN.DIAMETER <= 5)
class_5_10_cd <- dataset %>% filter(CROWN.DIAMETER > 5 & CROWN.DIAMETER <= 10)
class_10_15_cd <- dataset %>% filter(CROWN.DIAMETER > 10 & CROWN.DIAMETER <= 15)
class_15_20_cd <- dataset %>% filter(CROWN.DIAMETER > 15 & CROWN.DIAMETER <= 20)
class_20_25_cd <- dataset %>% filter(CROWN.DIAMETER > 20 & CROWN.DIAMETER <= 25)
class_25_cd <- dataset %>% filter(CROWN.DIAMETER > 25)

# Summary Variables Class Table
density <- area_of_plot_ha * number_of_plots
# DBH
class_1_5_dbh_density <- length(class_1_5_dbh$DIAMETER)
class_5_10_dbh_density <- length(class_5_10_dbh$DIAMETER)
class_10_15_dbh_density <- length(class_10_15_dbh$DIAMETER)
class_15_20_dbh_density <- length(class_15_20_dbh$DIAMETER)
class_20_25_dbh_density <- length(class_20_25_dbh$DIAMETER)
class_25_dbh_density <- length(class_25_dbh$DIAMETER)

# CD
class_1_5_cd_density <- length(class_1_5_cd$CROWN.DIAMETER)
class_5_10_cd_density <- length(class_5_10_cd$CROWN.DIAMETER)
class_10_15_cd_density <- length(class_10_15_cd$CROWN.DIAMETER)
class_15_20_cd_density <- length(class_15_20_cd$CROWN.DIAMETER)
class_20_25_cd_density <- length(class_20_25_cd$CROWN.DIAMETER)
class_25_cd_density <- length(class_25_cd$CROWN.DIAMETER)

summary_variable_classes_table <- data.frame(
  "DBH.density" = c(class_1_5_dbh_density, class_5_10_dbh_density, class_10_15_dbh_density, class_15_20_dbh_density, class_20_25_dbh_density, class_25_dbh_density),
  "DBH.trees.ha" = c((class_1_5_dbh_density/density), (class_5_10_dbh_density/density), (class_10_15_dbh_density/density), (class_15_20_dbh_density/density), (class_20_25_dbh_density/density), (class_25_dbh_density/density)),
  "CD.density" = c(class_1_5_cd_density, class_5_10_cd_density, class_10_15_cd_density, class_15_20_cd_density, class_20_25_cd_density, class_25_cd_density),
  "CD.trees.ha" = c((class_1_5_cd_density/density), (class_5_10_cd_density/density), (class_10_15_cd_density/density), (class_15_20_cd_density/density), (class_20_25_cd_density/density), (class_25_cd_density/density))
)
summary_variable_classes_table <- rbind(summary_variable_classes_table, 
                                        c(sum(summary_variable_classes_table$DBH.density), sum(summary_variable_classes_table$DBH.trees.ha), 
                                          sum(summary_variable_classes_table$CD.density), sum(summary_variable_classes_table$CD.trees.ha)
                                        )
)
rownames(summary_variable_classes_table) <- c("1 - 5", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "25>", "Total")


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
