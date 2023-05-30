#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, caTools, ggplot2, psych, tidyverse, Metrics, lmfor, dplyr, nlme)
	
# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//4 HD-D")

# Import data set
dataset <-  read.csv("full-data-3.csv", head=TRUE, sep=",")
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
training <- read.csv("training-data-4.csv", head=TRUE, sep=",")
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- read.csv("testing-data-4.csv", head=TRUE, sep=",")
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
start_values_gomperz <- startHDgomperz(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_logistics <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_naslund <- startHDnaslund(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh=1.3)

# Starting Values Table
starting_values_tables <- data.frame(
  "a" = c(
    round(start_values_power["a"], 2), 
    round(start_values_gomperz["a"], 2), 
    round(start_values_logistics["a"], 2), 
    round(start_values_naslund["a"], 2),
    round(start_values_chapman["a"], 2), 
    round(start_values_weibull["a"], 2)
  ),
  "b" = c(
    round(start_values_power["b"], 2), 
    round(start_values_gomperz["b"], 2), 
    round(start_values_logistics["b"], 2), 
    round(start_values_naslund["b"], 2),
    round(start_values_chapman["b"], 2), 
    round(start_values_weibull["b"], 2)
  ),
  "c" = c(
    "-",
    round(start_values_gomperz["c"], 2),
    round(start_values_logistics["c"], 2), 
    "-",
    round(start_values_chapman["c"], 2),
    round(start_values_weibull["c"], 2)
  )
)
rownames(starting_values_tables) <- c("Power", "Gomperz", "Logistics", "Naslund", "Chapman", "weibull")

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


# ----------------------------- Gomperz ----------------------------- #
# Gomperz Model Fitting
model_gomperz <- nls(formula = HEIGHT ~ (1.3 + (a * exp(-b * exp(-c * DIAMETER)))), 
                     data = training, start=list(
                       a=start_values_gomperz["a"], 
                       b=start_values_gomperz["b"],
                       c=start_values_gomperz["c"]))
summary_model_gomperz <- summary(model_gomperz)
length_model_gomperz <- length(summary(model_gomperz)$coef[,1])
model_gomperz

# Making Predictions
predictions_gomperz_training = predict(model_gomperz, training)
predictions_gomperz_testing = predict(model_gomperz, testing)


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
model_naslund <- nls(formula = HEIGHT ~ (1.3 + (a + b * log(DIAMETER))), 
                     data = training, start=list(a=start_values_naslund["a"], 
                                                 b=start_values_naslund["b"]))
summary_model_naslund <- summary(model_naslund)
length_model_naslund <- length(summary(model_naslund)$coef[,1])
model_naslund

# Making Predictions
predictions_naslund_training = predict(model_naslund, training)
predictions_naslund_testing = predict(model_naslund, testing)


# ----------------------------- Chapman-Richards ----------------------------- #
# Chapman-Richards Model Fitting
model_chapman <- nls(formula = HEIGHT ~ (1.3 + (a * (1 - exp(-b * DIAMETER))^c)), 
                     data = training, start=list(
                       a=start_values_chapman["a"], 
                       b=start_values_chapman["b"], 
                       c=start_values_chapman["c"]))
summary_model_chapman <- summary(model_chapman)
length_model_chapman <- length(summary(model_chapman)$coef[,1])
model_chapman

# Making Predictions
predictions_chapman_training = predict(model_chapman, training)
predictions_chapman_testing = predict(model_chapman, testing)


# ----------------------------- Weibull ----------------------------- #
# Weibull Model Fitting
model_weibull <- nls(formula = HEIGHT ~ (1.3 + (a * (1 - exp(-b * DIAMETER^c)))), 
                     data = training, start=list(a=start_values_weibull["a"], 
                                                 b=start_values_weibull["b"], 
                                                 c=start_values_weibull["c"]))
summary_model_weibull <- summary(model_weibull)
length_model_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull

# Making Predictions
predictions_weibull_training = predict(model_weibull, training)
predictions_weibull_testing = predict(model_weibull, testing)


# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$HEIGHT, 
  "Power_Predictions"=round(predictions_power_training, 1),
  "Gomperz_Predictions"=round(predictions_gomperz_training, 1),
  "Logistics_Predictions"=round(predictions_logistics_training, 1),
  "Naslund_Predictions"=round(predictions_naslund_training, 1),
  "Chapman_Predictions"=round(predictions_chapman_training, 1),
  "Weibull_Predictions"=round(predictions_weibull_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "Power_Predictions"=round(predictions_power_testing, 1),
  "Gomperz_Predictions"=round(predictions_gomperz_testing, 1),
  "Logistics_Predictions"=round(predictions_logistics_testing, 1),
  "Naslund_Predictions"=round(predictions_naslund_testing, 1),
  "Chapman_Predictions"=round(predictions_chapman_testing, 1),
  "Weibull_Predictions"=round(predictions_weibull_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()

# ---------------------- Manual Predictions -------------------------------#
predictor_variable <- data.frame(DIAMETER = 20.4)

# Power
predict_power <- predict(model_power, predictor_variable)

# Gomperz
predict_gomperz <- predict(model_gomperz, predictor_variable)

# Logistics
predict_logistics <- predict(model_logistics, predictor_variable)

# Naslund
predict_naslund <- predict(model_naslund, predictor_variable)

# Chapman-Richards
predict_chapman <- predict(model_chapman, predictor_variable)

# Weibull
predict_weibull <- predict(model_weibull, predictor_variable)

# Predict Table
predict_table <- data.frame(
  "DIAMETER" = predictor_variable$DIAMETER,
  "HEIGHT" = 7.5,
  "Power_Predict" = round(predict_power, 1),
  "Gomperz_Predict" = round(predict_gomperz, 1),
  "Logistics_Predict" = round(predict_logistics, 1),
  "Naslund_Predict" = round(predict_naslund, 1),
  "Chapman_Predict" = round(predict_chapman, 1),
  "Weibull_Predict" = round(predict_weibull, 1)
)


#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(round(summary_model_power[["parameters"]][1], 3), round(summary_model_gomperz[["parameters"]][1], 3), round(summary_model_logistics[["parameters"]][1], 3), round(summary_model_naslund[["parameters"]][1], 3), round(summary_model_chapman[["parameters"]][1], 3), round(summary_model_weibull[["parameters"]][1], 3)),
  "b" = c(round(summary_model_power[["parameters"]][2], 3), round(summary_model_gomperz[["parameters"]][2], 3), round(summary_model_logistics[["parameters"]][2], 3), round(summary_model_naslund[["parameters"]][2], 3), round(summary_model_chapman[["parameters"]][2], 3), round(summary_model_weibull[["parameters"]][2], 3)),
  "c" = c("-", "-", round(summary_model_logistics[["parameters"]][3], 3), "-", round(summary_model_chapman[["parameters"]][3], 3), round(summary_model_weibull[["parameters"]][3], 3))
)
rownames(parameters_nls) <- c("Power", "Gomperz", "Logistics", "Naslund", "Chapman", "Weibull")



#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_power_training <- rmse(training$HEIGHT, predictions_power_training)
RMSE_gomperz_training <- rmse(training$HEIGHT, predictions_gomperz_training)
RMSE_logistics_training <- rmse(training$HEIGHT, predictions_logistics_training)
RMSE_naslund_training <- rmse(training$HEIGHT, predictions_naslund_training)
RMSE_chapman_training <- rmse(training$HEIGHT, predictions_chapman_training)
RMSE_weibull_training <- rmse(training$HEIGHT, predictions_weibull_training)
RMSE_training <- c(
  round(RMSE_power_training, 3), 
  round(RMSE_gomperz_training, 3), 
  round(RMSE_logistics_training, 3), 
  round(RMSE_naslund_training, 3),
  round(RMSE_chapman_training, 3),
  round(RMSE_weibull_training, 3)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_power_testing <- rmse(testing$HEIGHT, predictions_power_testing)
RMSE_gomperz_testing <- rmse(testing$HEIGHT, predictions_gomperz_testing)
RMSE_logistics_testing <- rmse(testing$HEIGHT, predictions_logistics_testing)
RMSE_naslund_testing <- rmse(testing$HEIGHT, predictions_naslund_testing)
RMSE_chapman_testing <- rmse(testing$HEIGHT, predictions_chapman_testing)
RMSE_weibull_testing <- rmse(testing$HEIGHT, predictions_weibull_testing)
RMSE_testing <- c(
  round(RMSE_power_testing, 3), 
  round(RMSE_gomperz_testing, 3), 
  round(RMSE_logistics_testing, 3), 
  round(RMSE_naslund_testing, 3),
  round(RMSE_chapman_testing, 3),
  round(RMSE_weibull_testing, 3)
)
summary_table_testing <- data.frame("RMSE" = RMSE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_power_training <- cor(training$HEIGHT, predictions_power_training)^2 # Correlation Coefficient
RSQUARED_gomperz_training <- cor(training$HEIGHT, predictions_gomperz_training)^2 # Correlation Coefficient
RSQUARED_logistics_training <- cor(training$HEIGHT, predictions_logistics_training)^2 # Correlation Coefficient
RSQUARED_naslund_training <- cor(training$HEIGHT, predictions_naslund_training)^2 # Correlation Coefficient
RSQUARED_chapman_training <- cor(training$HEIGHT, predictions_chapman_training)^2 # Correlation Coefficient
RSQUARED_weibull_training <- cor(training$HEIGHT, predictions_weibull_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_power_training, 3), 
  round(RSQUARED_gomperz_training, 3), 
  round(RSQUARED_logistics_training, 3), 
  round(RSQUARED_naslund_training, 3),
  round(RSQUARED_chapman_training, 3),
  round(RSQUARED_weibull_training, 3)
)
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_power_testing <- cor(testing$HEIGHT, predictions_power_testing)^2 # Correlation Coefficient
RSQUARED_gomperz_testing <- cor(testing$HEIGHT, predictions_gomperz_testing)^2 # Correlation Coefficient
RSQUARED_logistics_testing <- cor(testing$HEIGHT, predictions_logistics_testing)^2 # Correlation Coefficient
RSQUARED_naslund_testing <- cor(testing$HEIGHT, predictions_naslund_testing)^2 # Correlation Coefficient
RSQUARED_chapman_testing <- cor(testing$HEIGHT, predictions_chapman_testing)^2 # Correlation Coefficient
RSQUARED_weibull_testing <- cor(testing$HEIGHT, predictions_weibull_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_power_testing, 3), 
  round(RSQUARED_gomperz_testing, 3), 
  round(RSQUARED_logistics_testing, 3), 
  round(RSQUARED_naslund_testing, 3),
  round(RSQUARED_chapman_testing, 3),
  round(RSQUARED_weibull_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Training
MAPE_power_training <- mape(training$HEIGHT, predictions_power_training)
MAPE_gomperz_training <- mape(training$HEIGHT, predictions_gomperz_training)
MAPE_logistics_training <- mape(training$HEIGHT, predictions_logistics_training)
MAPE_naslund_training <- mape(training$HEIGHT, predictions_naslund_training)
MAPE_chapman_training <- mape(training$HEIGHT, predictions_chapman_training)
MAPE_weibull_training <- mape(training$HEIGHT, predictions_weibull_training)
MAPE_training <- c(
  round(MAPE_power_training, 3),
  round(MAPE_gomperz_training, 3),
  round(MAPE_logistics_training, 3),
  round(MAPE_naslund_training, 3),
  round(MAPE_chapman_training, 3),
  round(MAPE_weibull_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAPE" = MAPE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Testing
MAPE_power_testing <- mape(testing$HEIGHT, predictions_power_testing)
MAPE_gomperz_testing <- mape(testing$HEIGHT, predictions_gomperz_testing)
MAPE_logistics_testing <- mape(testing$HEIGHT, predictions_logistics_testing)
MAPE_naslund_testing <- mape(testing$HEIGHT, predictions_naslund_testing)
MAPE_chapman_testing <- mape(testing$HEIGHT, predictions_chapman_testing)
MAPE_weibull_testing <- mape(testing$HEIGHT, predictions_weibull_testing)
MAPE_testing <- c(
  round(MAPE_power_testing, 3),
  round(MAPE_gomperz_testing, 3),
  round(MAPE_logistics_testing, 3),
  round(MAPE_naslund_testing, 3),
  round(MAPE_chapman_testing, 3),
  round(MAPE_weibull_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAPE" = MAPE_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_power_training <- bias(training$HEIGHT, predictions_power_training)
bias_gomperz_training <- bias(training$HEIGHT, predictions_gomperz_training)
bias_logistics_training <- bias(training$HEIGHT, predictions_logistics_training)
bias_naslund_training <- bias(training$HEIGHT, predictions_naslund_training)
bias_chapman_training <- bias(training$HEIGHT, predictions_chapman_training)
bias_weibull_training <- bias(training$HEIGHT, predictions_weibull_training)
BIAS_training <- c(
  round(bias_power_training, 3), 
  round(bias_gomperz_training, 3), 
  round(bias_logistics_training, 3), 
  round(bias_naslund_training, 3),
  round(bias_chapman_training, 3),
  round(bias_weibull_training, 3)
)
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_power_testing <- bias(testing$HEIGHT, predictions_power_testing)
bias_gomperz_testing <- bias(testing$HEIGHT, predictions_gomperz_testing)
bias_logistics_testing <- bias(testing$HEIGHT, predictions_logistics_testing)
bias_naslund_testing <- bias(testing$HEIGHT, predictions_naslund_testing)
bias_chapman_testing <- bias(testing$HEIGHT, predictions_chapman_testing)
bias_weibull_testing <- bias(testing$HEIGHT, predictions_weibull_testing)
BIAS_testing <- c(
  round(bias_power_testing, 3), 
  round(bias_gomperz_testing, 3), 
  round(bias_logistics_testing, 3), 
  round(bias_naslund_testing, 3),
  round(bias_chapman_testing, 3),
  round(bias_weibull_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Error (MAE)
# Training
MAE_power_training <- mae(training$HEIGHT, predictions_power_training)
MAE_gomperz_training <- mae(training$HEIGHT, predictions_gomperz_training)
MAE_logistics_training <- mae(training$HEIGHT, predictions_logistics_training)
MAE_naslund_training <- mae(training$HEIGHT, predictions_naslund_training)
MAE_chapman_training <- mae(training$HEIGHT, predictions_chapman_training)
MAE_weibull_training <- mae(training$HEIGHT, predictions_weibull_training)
MAE_training <- c(
  round(MAE_power_training, 3), 
  round(MAE_gomperz_training, 3), 
  round(MAE_logistics_training, 3), 
  round(MAE_naslund_training, 3),
  round(MAE_chapman_training, 3),
  round(MAE_weibull_training, 3)
)
summary_table_training <- cbind(summary_table_training, "MAE" = MAE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Error (MAE)
# Testing
MAE_power_testing <- mae(testing$HEIGHT, predictions_power_testing)
MAE_gomperz_testing <- mae(testing$HEIGHT, predictions_gomperz_testing)
MAE_logistics_testing <- mae(testing$HEIGHT, predictions_logistics_testing)
MAE_naslund_testing <- mae(testing$HEIGHT, predictions_naslund_testing)
MAE_chapman_testing <- mae(testing$HEIGHT, predictions_chapman_testing)
MAE_weibull_testing <- mae(testing$HEIGHT, predictions_weibull_testing)
MAE_testing <- c(
  round(MAE_power_testing, 3), 
  round(MAE_gomperz_testing, 3), 
  round(MAE_logistics_testing, 3), 
  round(MAE_naslund_testing, 3),
  round(MAE_chapman_testing, 3),
  round(MAE_weibull_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "MAE" = MAE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_power <- AIC(model_power)
AIC_gomperz <- AIC(model_gomperz)
AIC_logistics <- AIC(model_logistics)
AIC_naslund <- AIC(model_naslund)
AIC_chapman <- AIC(model_chapman)
AIC_weibull <- AIC(model_weibull)
AIC <- c(
  round(AIC_power, 3),
  round(AIC_gomperz, 3),
  round(AIC_logistics, 3),
  round(AIC_naslund, 3),
  round(AIC_chapman, 3),
  round(AIC_weibull, 3)
)
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)


# calculate BIC
BIC_power <- BIC(model_power)
BIC_gomperz <- BIC(model_gomperz)
BIC_logistics <- BIC(model_logistics)
BIC_naslund <- BIC(model_naslund)
BIC_chapman <- BIC(model_chapman)
BIC_weibull <- BIC(model_weibull)
BIC <- c(
  round(BIC_power, 2),
  round(BIC_gomperz, 2),
  round(BIC_logistics, 2),
  round(BIC_naslund, 2),
  round(BIC_chapman, 2),
  round(BIC_weibull, 2)
)
summary_table_training <- cbind(summary_table_training, BIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, BIC)

#----------------------------- Model Evaluation -----------------------------#
# Accuracy
# Training
accuracy_power_training <- accuracy(training$HEIGHT, prediction_frame_training$Power_Predictions)
accuracy_gomperz_training <- accuracy(training$HEIGHT, prediction_frame_training$Gomperz_Predictions)
accuracy_logistics_training <- accuracy(training$HEIGHT, prediction_frame_training$Logistics_Predictions)
accuracy_naslund_training <- accuracy(training$HEIGHT, prediction_frame_training$Naslund_Predictions)
accuracy_chapman_training <- accuracy(training$HEIGHT, prediction_frame_training$Chapman_Predictions)
accuracy_weibull_training <- accuracy(training$HEIGHT, prediction_frame_training$Weilbull_Predictions)
accuracy_training <- c(
  round((accuracy_power_training * 100), 2), 
  round((accuracy_gomperz_training * 100), 2), 
  round((accuracy_logistics_training * 100), 2), 
  round((accuracy_naslund_training * 100), 2),
  round((accuracy_chapman_training * 100), 2),
  round((accuracy_weibull_training * 100), 2)
)
summary_table_training <- cbind(summary_table_training, "Accuracy(%)" = accuracy_training)

#----------------------------- Model Validation -----------------------------#
# Accuracy
# Testing
accuracy_power_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Power_Predictions)
accuracy_gomperz_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Gomperz_Predictions)
accuracy_logistics_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Logistics_Predictions)
accuracy_naslund_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Naslund_Predictions)
accuracy_chapman_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Chapman_Predictions)
accuracy_weibull_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Weilbull_Predictions)
accuracy_testing <- c(
  round((accuracy_power_testing * 100), 2), 
  round((accuracy_gomperz_testing * 100), 2), 
  round((accuracy_logistics_testing * 100), 2), 
  round((accuracy_naslund_testing * 100), 2),
  round((accuracy_chapman_testing * 100), 2),
  round((accuracy_weibull_testing * 100), 2)
)
summary_table_testing <- cbind(summary_table_testing, "Accuracy(%)" = accuracy_testing)

rownames(summary_table_training) <- c("Power", "Gomperz", "Logistics", "Naslund", "Chapman", "Weibull")
rownames(summary_table_testing) <- c("Power", "Gomperz", "Logistics", "Naslund", "Chapman", "Weibull")

# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_gomperz_training, colour = "Gomperz")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_logistics_training, colour = "Logistics")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_naslund_training, colour = "Naslund")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_chapman_training, colour = "Chapman")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_weibull_training, colour = "Weibull")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Power"="#0f0f10",
    "Gomperz"="#eb7de3",
    "Logistics"="#eb1919",
    "Naslund"="#c9b424",
    "Chapman"="#2533d3", 
    "Weibull"="#69ef79"
  )) 

# Model Residuals Table
residuals_table <- data.frame(
  "Power" = residuals(model_power),
  "Gomperz" = residuals(model_gomperz),
  "Logistics" = residuals(model_logistics),
  "Naslund" = residuals(model_naslund),
  "Chapman" = residuals(model_chapman),
  "Weibull" = residuals(model_weibull)
)

# Scattered Plot (Model Performance Line) -- Power
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Line (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c")) 


# Scattered Plot (Model Performance Curve) -- Gomperz
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_gomperz_training, colour = "Gomperz")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Gomperz)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Gomperz"="#120c4c")) 

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

# Scattered Plot (Model Performance Curve) -- Chapman
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_chapman_training, colour = "Chapman")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Chapman)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Chapman"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Weibull
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_weibull_training, colour = "Weibull")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Weibull)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Weibull"="#120c4c"))


# Scattered Plot (Fitted VS Residuals Values) -- Chapman
ggplot() +
  geom_point(aes(x = fitted(model_chapman), y = residuals(model_chapman), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Chapman")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Chapman)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Chapman"="#4954ed")) 

plot(model_chapman, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Chapman",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Fitted VS Residuals Values) -- Weibull
ggplot() +
  geom_point(aes(x = fitted(model_weibull), y = residuals(model_weibull), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Weibull")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Weibull)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Weibull"="#4954ed")) 

plot(model_weibull, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Weibull",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Chapman
ggplot(residuals_table, aes(sample = Chapman)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Chapman)")

qqnorm(residuals(model_chapman, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Chapman", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_chapman,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Weibull
ggplot(residuals_table, aes(sample = Weibull)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Weibull)")

qqnorm(residuals(model_weibull, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Weibull", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_weibull,type = "pearson"))

# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
