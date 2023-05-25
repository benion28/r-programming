#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics, neuralnet, lmfor, dplyr, nlme)
	
# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//1 H-D")

# Import data set
dataset <-  read.csv("C://Benion//Benion Programmings//R//Analysis Result//1//full-data-1.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
r_dataset_value <- cor(dataset$DIAMETER, dataset$HEIGHT)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset

#-------------------- EXploratory Data Analysis ----------------------------#

# All Species Distribution Plot
ggplot() +
  geom_point(aes(x = dataset$DIAMETER, y = dataset$HEIGHT), colour = "#a32e2e") +
  ggtitle("Tree Height vs Diameter (Data Distribution)") +
  xlab("Diameter") +
  ylab("Height")

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
training <- read.csv("C://Benion//Benion Programmings//R//Analysis Result//1//training-data-1.csv", head=TRUE, sep=",")
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- read.csv("C://Benion//Benion Programmings//R//Analysis Result//1//testing-data-1.csv", head=TRUE, sep=",")
summary(testing)
testing_description <- describe(testing)

# Descriptive Statistic Table
descriptive_statistcic_table <- data.frame(
  "Training_Diameter" = c(round(mean(training$DIAMETER), 2), round(min(training$DIAMETER), 2), round(max(training$DIAMETER, 2)), round(SD(training$DIAMETER), 2)),
  "Testing_Diameter" = c(round(mean(testing$DIAMETER), 2), round(min(testing$DIAMETER), 2), round(max(testing$DIAMETER), 2), round(SD(testing$DIAMETER), 2)),
  "All_Diameter" = c(round(mean(dataset$DIAMETER), 2), round(min(dataset$DIAMETER), 2), round(max(dataset$DIAMETER), 2), round(SD(dataset$DIAMETER), 2)),
  "Training_Height" = c(round(mean(training$HEIGHT), 2), round(min(training$HEIGHT), 2), round(max(training$HEIGHT), 2), round(SD(training$HEIGHT), 2)),
  "Testing_Height" = c(round(mean(testing$HEIGHT), 2), round(min(testing$HEIGHT), 2), round(max(testing$HEIGHT), 2), round(SD(testing$HEIGHT), 2)),
  "All_Height" = c(round(mean(dataset$HEIGHT), 2), round(min(dataset$HEIGHT), 2), round(max(dataset$HEIGHT), 2), round(SD(dataset$DIAMETER), 2))
)
descriptive_statistcic_table <- rbind(descriptive_statistcic_table, c(
  length(training$DIAMETER), length(testing$DIAMETER), length(dataset$DIAMETER), length(training$HEIGHT), length(testing$HEIGHT), length(dataset$HEIGHT)))
rownames(descriptive_statistcic_table) <- c("Mean", "Minimum", "Maximum", "Standard_Deviation", "Obsersavations(n)")

# Starting Values 
start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_curtis <- startHDcurtis(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_logistics <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_naslund <- startHDnaslund(training$DIAMETER, training$HEIGHT, bh=1.3)

# Starting Values Table
starting_values_tables <- data.frame(
  "a" = c(
    round(start_values_power["a"], 2), 
    round(start_values_curtis["a"], 2), 
    round(start_values_logistics["a"], 2), 
    round(start_values_naslund["a"], 2)
  ),
  "b" = c(
    round(start_values_power["b"], 2), 
    round(start_values_curtis["b"], 2), 
    round(start_values_logistics["b"], 2), 
    round(start_values_naslund["b"], 2)
  ),
  "c" = c(
    "-",
    "-",
    round(start_values_logistics["c"], 2), 
    "-"
  )
)
rownames(starting_values_tables) <- c("Power", "Curtis", "Logistics", "Naslund")

# ----------------------------- Power Model----------------------------- #
# Power Model Fitting
model_power <- nls(formula = HEIGHT ~ (1.3 + (a * DIAMETER^b)), 
                   data = training, start=list(
                     a=start_values_power["a"], 
                     b=start_values_power["b"]))
summary_model_power <- summary(model_power)
length_model_power <- length(summary(model_power)$coef[,1])
model_power


# Making Predictions
predictions_power_training = predict(model_power, training)
predictions_power_testing = predict(model_power, testing)


# ----------------------------- Curtis ----------------------------- #
# Curtis Model Fitting
model_curtis <- nls(formula = HEIGHT ~ (1.3 + (a + b * log(DIAMETER))), 
                     data = training, start=list(
                       a=start_values_curtis["a"], 
                       b=start_values_curtis["b"]))
summary_model_curtis <- summary(model_curtis)
length_model_curtis <- length(summary(model_curtis)$coef[,1])
model_curtis

# Making Predictions
predictions_curtis_training = predict(model_curtis, training)
predictions_curtis_testing = predict(model_curtis, testing)


# ----------------------------- Logistics ----------------------------- #
# Logistics Model Fitting
model_logistics <- nls(formula = HEIGHT ~ (1.3 + (a / (1 + b * exp(-c * DIAMETER)))),
                       data = training, start=list(a=start_values_logistics["a"],
                                                   b=start_values_logistics["b"],
                                                   c=start_values_logistics["c"]))
summary_model_logistics <- summary(model_logistics)
length_model_logistics <- length(summary(model_logistics)$coef[,1])
model_logistics

# Making Predictions
predictions_logistics_training = predict(model_logistics, training)
predictions_logistics_testing = predict(model_logistics, testing)


# ----------------------------- Naslund ----------------------------- #
# Naslund Model Fitting
model_naslund <- nls(formula = HEIGHT ~ (1.3 + (a - b * (DIAMETER^2))), 
                     data = training, start=list(a=start_values_naslund["a"], 
                                                 b=start_values_naslund["b"]))
summary_model_naslund <- summary(model_naslund)
length_model_naslund <- length(summary(model_naslund)$coef[,1])
model_naslund

# Making Predictions
predictions_naslund_training = predict(model_naslund, training)
predictions_naslund_testing = predict(model_naslund, testing)

# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$HEIGHT, 
  "Power_Predictions"=round(predictions_power_training, 1),
  "Curtis_Predictions"=round(predictions_curtis_training, 1),
  "Logistics_Predictions"=round(predictions_logistics_training, 1),
  "Naslund_Predictions"=round(predictions_naslund_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "Power_Predictions"=round(predictions_power_testing, 1),
  "Curtis_Predictions"=round(predictions_curtis_testing, 1),
  "Logistics_Predictions"=round(predictions_logistics_testing, 1),
  "Naslund_Predictions"=round(predictions_naslund_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()

# ---------------------- Manual Predictions -------------------------------#
predictor_variable <- data.frame(DIAMETER = 20.4)

# Power
predict_power <- predict(model_power, predictor_variable)

# Curtis
predict_curtis <- predict(model_curtis, predictor_variable)

# Logistics
predict_logistics <- predict(model_logistics, predictor_variable)

# Naslund
predict_naslund <- predict(model_naslund, predictor_variable)

# Predict Table
predict_table <- data.frame(
  "DIAMETER" = predictor_variable$DIAMETER,
  "HEIGHT" = 7.5,
  "Power_Predict" = round(predict_power, 1),
  "Curtis_Predict" = round(predict_curtis, 1),
  "Logistics_Predict" = round(predict_logistics, 1),
  "Naslund_Predict" = round(predict_naslund, 1)
)


#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(round(summary_model_power[["parameters"]][1], 5), round(summary_model_curtis[["parameters"]][1], 5), round(summary_model_logistics[["parameters"]][1], 5), round(summary_model_naslund[["parameters"]][1], 5)),
  "b" = c(round(summary_model_power[["parameters"]][2], 5), round(summary_model_curtis[["parameters"]][2], 5), round(summary_model_logistics[["parameters"]][2], 5), round(summary_model_naslund[["parameters"]][2], 5)),
  "c" = c("-", "-", round(summary_model_logistics[["parameters"]][3], 5), "-")
)
rownames(parameters_nls) <- c("Power", "Curtis", "Logistics", "Naslund")



#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_power_training <- rmse(training$HEIGHT, predictions_power_training)
RMSE_curtis_training <- rmse(training$HEIGHT, predictions_curtis_training)
RMSE_logistics_training <- rmse(training$HEIGHT, predictions_logistics_training)
RMSE_naslund_training <- rmse(training$HEIGHT, predictions_naslund_training)
RMSE_training <- c(
  round(RMSE_power_training, 3), 
  round(RMSE_curtis_training, 3), 
  round(RMSE_logistics_training, 3), 
  round(RMSE_naslund_training, 3)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_power_testing <- rmse(testing$HEIGHT, predictions_power_testing)
RMSE_curtis_testing <- rmse(testing$HEIGHT, predictions_curtis_testing)
RMSE_logistics_testing <- rmse(testing$HEIGHT, predictions_logistics_testing)
RMSE_naslund_testing <- rmse(testing$HEIGHT, predictions_naslund_testing)
RMSE_testing <- c(
  round(RMSE_power_testing, 3), 
  round(RMSE_curtis_testing, 3), 
  round(RMSE_logistics_testing, 3), 
  round(RMSE_naslund_testing, 3)
)
summary_table_testing <- data.frame("RMSE" = RMSE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_power_training <- cor(training$HEIGHT, predictions_power_training)^2 # Correlation Coefficient
RSQUARED_curtis_training <- cor(training$HEIGHT, predictions_curtis_training)^2 # Correlation Coefficient
RSQUARED_logistics_training <- cor(training$HEIGHT, predictions_logistics_training)^2 # Correlation Coefficient
RSQUARED_naslund_training <- cor(training$HEIGHT, predictions_naslund_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_power_training, 3), 
  round(RSQUARED_curtis_training, 3), 
  round(RSQUARED_logistics_training, 3), 
  round(RSQUARED_naslund_training, 3)
)
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_power_testing <- cor(testing$HEIGHT, predictions_power_testing)^2 # Correlation Coefficient
RSQUARED_curtis_testing <- cor(testing$HEIGHT, predictions_curtis_testing)^2 # Correlation Coefficient
RSQUARED_logistics_testing <- cor(testing$HEIGHT, predictions_logistics_testing)^2 # Correlation Coefficient
RSQUARED_naslund_testing <- cor(testing$HEIGHT, predictions_naslund_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_power_testing, 3), 
  round(RSQUARED_curtis_testing, 3), 
  round(RSQUARED_logistics_testing, 3), 
  round(RSQUARED_naslund_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Training
MAPE_power_training <- mape(training$HEIGHT, predictions_power_training)
MAPE_curtis_training <- mape(training$HEIGHT, predictions_curtis_training)
MAPE_logistics_training <- mape(training$HEIGHT, predictions_logistics_training)
MAPE_naslund_training <- mape(training$HEIGHT, predictions_naslund_training)
MAPE_training <- c(
  round(MAPE_power_training, 3),
  round(MAPE_curtis_training, 3),
  round(MAPE_logistics_training, 3),
  round(MAPE_naslund_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAPE" = MAPE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Testing
MAPE_power_testing <- mape(testing$HEIGHT, predictions_power_testing)
MAPE_curtis_testing <- mape(testing$HEIGHT, predictions_curtis_testing)
MAPE_logistics_testing <- mape(testing$HEIGHT, predictions_logistics_testing)
MAPE_naslund_testing <- mape(testing$HEIGHT, predictions_naslund_testing)
MAPE_testing <- c(
  round(MAPE_power_testing, 3),
  round(MAPE_curtis_testing, 3),
  round(MAPE_logistics_testing, 3),
  round(MAPE_naslund_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAPE" = MAPE_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_power_training <- bias(training$HEIGHT, predictions_power_training)
bias_curtis_training <- bias(training$HEIGHT, predictions_curtis_training)
bias_logistics_training <- bias(training$HEIGHT, predictions_logistics_training)
bias_naslund_training <- bias(training$HEIGHT, predictions_naslund_training)
BIAS_training <- c(
  round(bias_power_training, 3), 
  round(bias_curtis_training, 3), 
  round(bias_logistics_training, 3), 
  round(bias_naslund_training, 3)
)
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_power_testing <- bias(testing$HEIGHT, predictions_power_testing)
bias_curtis_testing <- bias(testing$HEIGHT, predictions_curtis_testing)
bias_logistics_testing <- bias(testing$HEIGHT, predictions_logistics_testing)
bias_naslund_testing <- bias(testing$HEIGHT, predictions_naslund_testing)
BIAS_testing <- c(
  round(bias_power_testing, 3), 
  round(bias_curtis_testing, 3), 
  round(bias_logistics_testing, 3), 
  round(bias_naslund_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Error (MAE)
# Training
MAE_power_training <- mae(training$HEIGHT, predictions_power_training)
MAE_curtis_training <- mae(training$HEIGHT, predictions_curtis_training)
MAE_logistics_training <- mae(training$HEIGHT, predictions_logistics_training)
MAE_naslund_training <- mae(training$HEIGHT, predictions_naslund_training)
MAE_training <- c(
  round(MAE_power_training, 3), 
  round(MAE_curtis_training, 3), 
  round(MAE_logistics_training, 3), 
  round(MAE_naslund_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAE" = MAE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Error (MAE)
# Testing
MAE_power_testing <- mae(testing$HEIGHT, predictions_power_testing)
MAE_curtis_testing <- mae(testing$HEIGHT, predictions_curtis_testing)
MAE_logistics_testing <- mae(testing$HEIGHT, predictions_logistics_testing)
MAE_naslund_testing <- mae(testing$HEIGHT, predictions_naslund_testing)
MAE_testing <- c(
  round(MAE_power_testing, 3), 
  round(MAE_curtis_testing, 3), 
  round(MAE_logistics_testing, 3), 
  round(MAE_naslund_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAE" = MAE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_power <- AIC(model_power)
AIC_curtis <- AIC(model_curtis)
AIC_logistics <- AIC(model_logistics)
AIC_naslund <- AIC(model_naslund)
AIC <- c(
  round(AIC_power, 3),
  round(AIC_curtis, 3),
  round(AIC_logistics, 3),
  round(AIC_naslund, 3)
)
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)


# calculate BIC
BIC_power <- BIC(model_power)
BIC_curtis <- BIC(model_curtis)
BIC_logistics <- BIC(model_logistics)
BIC_naslund <- BIC(model_naslund)
BIC <- c(
  round(BIC_power, 2),
  round(BIC_curtis, 2),
  round(BIC_logistics, 2),
  round(BIC_naslund, 2)
)
summary_table_training <- cbind(summary_table_training, BIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, BIC)

#----------------------------- Model Evaluation -----------------------------#
# Accuracy
# Training
accuracy_power_training <- accuracy(training$HEIGHT, prediction_frame_training$Power_Predictions)
accuracy_curtis_training <- accuracy(training$HEIGHT, prediction_frame_training$Curtis_Predictions)
accuracy_logistics_training <- accuracy(training$HEIGHT, prediction_frame_training$Logistics_Predictions)
accuracy_naslund_training <- accuracy(training$HEIGHT, prediction_frame_training$Naslund_Predictions)
accuracy_training <- c(
  round((accuracy_power_training * 100), 2), 
  round((accuracy_curtis_training * 100), 2), 
  round((accuracy_logistics_training * 100), 2), 
  round((accuracy_naslund_training * 100), 2)
)
summary_table_training <- cbind(summary_table_training, "Accuracy(%)" = accuracy_training)

#----------------------------- Model Validation -----------------------------#
# Accuracy
# Testing
accuracy_power_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Power_Predictions)
accuracy_curtis_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Curtis_Predictions)
accuracy_logistics_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Logistics_Predictions)
accuracy_naslund_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Naslund_Predictions)
accuracy_testing <- c(
  round((accuracy_power_testing * 100), 2), 
  round((accuracy_curtis_testing * 100), 2), 
  round((accuracy_logistics_testing * 100), 2), 
  round((accuracy_naslund_testing * 100), 2)
)
summary_table_testing <- cbind(summary_table_testing, "Accuracy(%)" = accuracy_testing)

rownames(summary_table_training) <- c("Power_Values", "Curtis", "Logistics", "Naslund")
rownames(summary_table_testing) <- c("Power_Values", "Curtis", "Logistics", "Naslund")

# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_curtis_training, colour = "Curtis")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_logistics_training, colour = "Logistics")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_naslund_training, colour = "Naslund")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Power"="#0f0f10",
    "Curtis"="#eb7de3",
    "Logistics"="#eb1919",
    "Naslund"="#c9b424"
  )) 

# Model Residuals Table
residuals_table <- data.frame(
  "Power" = residuals(model_power),
  "Curtis" = residuals(model_curtis),
  "Logistics" = residuals(model_logistics),
  "Naslund" = residuals(model_naslund)
)

# Scattered Plot (Model Performance Line) -- Power
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Line (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c")) 


# Scattered Plot (Model Performance Curve) -- Curtis
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_curtis_training, colour = "Curtis")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Curtis)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Curtis"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Logistics
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_logistics_training, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Logistics"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Naslund
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_naslund_training, colour = "Naslund")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Naslund)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Naslund"="#120c4c")) 

# Scattered Plot (Fitted VS Residuals Values) -- Logistics
ggplot() +
  geom_point(aes(x = fitted(model_logistics), y = residuals(model_logistics), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Logistics")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Logistics)", color = "Legend") +
  theme(legend.position = c(0.75, 0.70), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Logistics"="#4954ed")) 

plot(model_logistics, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Logistics",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Fitted VS Residuals Values) -- Power
ggplot() +
  geom_point(aes(x = fitted(model_power), y = residuals(model_power), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Power")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Power)", color = "Legend") +
  theme(legend.position = c(0.75, 0.70), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#4954ed")) 

plot(model_power, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Power",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Power
ggplot(residuals_table, aes(sample = Power)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Power)")

qqnorm(residuals(model_power, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Power", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_logistics,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Logistics
ggplot(residuals_table, aes(sample = Logistics)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Logistics)")

qqnorm(residuals(model_logistics, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Logistics", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_logistics,type = "pearson"))


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L