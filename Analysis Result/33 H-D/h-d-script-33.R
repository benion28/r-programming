#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, caTools, ggplot2, psych, tidyverse, Metrics, lmfor, dplyr, nlme)


# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//33 H-D")


# Import data set
dataset <-  read.csv("full-data-13.csv", head=TRUE, sep=",")

# Filter Dataset (Khaya senegalensis)
dataset <- filter(dataset, dataset$SPECIES == "Azadirachta indica")
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
  "Training_Height" = c(round(mean(training$HEIGHT), 2), round(min(training$HEIGHT), 2), round(max(training$HEIGHT), 2), round(SD(training$HEIGHT), 2)),
  "Testing_Height" = c(round(mean(testing$HEIGHT), 2), round(min(testing$HEIGHT), 2), round(max(testing$HEIGHT), 2), round(SD(testing$HEIGHT), 2)),
  "All_Height" = c(round(mean(dataset$HEIGHT), 2), round(min(dataset$HEIGHT), 2), round(max(dataset$HEIGHT), 2), round(SD(dataset$DIAMETER), 2))
)
descriptive_statistcic_table <- rbind(descriptive_statistcic_table, c(
  length(training$DIAMETER), length(testing$DIAMETER), length(dataset$DIAMETER), length(training$HEIGHT), length(testing$HEIGHT), length(dataset$HEIGHT)))
rownames(descriptive_statistcic_table) <- c("Mean", "Minimum", "Maximum", "Standard_Deviation", "Obsersavations(n)")

# Basal Height
bh <- 1.3

# Starting Values
start_values_naslund <- startHDnaslund(training$DIAMETER, training$HEIGHT, bh)
start_values_curtis <- startHDcurtis(training$DIAMETER, training$HEIGHT, bh)
start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh)
start_values_wykoff <- startHDwykoff(training$DIAMETER, training$HEIGHT, bh)
start_values_prodan <- startHDprodan(training$DIAMETER, training$HEIGHT, bh)
start_values_logistic <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh)
start_values_sibbesen <- startHDsibbesen(training$DIAMETER, training$HEIGHT, bh)
start_values_ratkowsky <- startHDratkowsky(training$DIAMETER, training$HEIGHT, bh)

# ----------------------------- Model Naslund ----------------------------- #
# Model Naslund Fitting
model_naslund <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(a + b * DIAMETER)^2), 
                     data = training, start=list(
                       a=start_values_naslund["a"], 
                       b=start_values_naslund["b"]))
summary_model_naslund <- summary(model_naslund)
length_model_naslund <- length(summary(model_naslund)$coef[,1])
model_naslund


# Making Predictions
predictions_model_naslund_training = predict(model_naslund, training)
predictions_model_naslund_testing = predict(model_naslund, testing)


# ----------------------------- Model Curtis ----------------------------- #
# Model Curtis Fitting
model_curtis <- nls(formula = HEIGHT ~ (bh + a * (DIAMETER/(1 + DIAMETER))^b), 
                    data = training, start=list(
                      a=start_values_curtis["a"], 
                      b=start_values_curtis["b"]))
summary_model_curtis <- summary(model_curtis)
length_model_curtis <- length(summary(model_curtis)$coef[,1])
model_curtis


# Making Predictions
predictions_model_curtis_training = predict(model_curtis, training)
predictions_model_curtis_testing = predict(model_curtis, testing)


# ----------------------------- Model Power ----------------------------- #
# Model Power Fitting
model_power <- nls(formula = HEIGHT ~ (bh + a * DIAMETER^b), 
                   data = training, start=list(
                     a=start_values_power["a"], 
                     b=start_values_power["b"]))
summary_model_power <- summary(model_power)
length_model_power <- length(summary(model_power)$coef[,1])
model_power


# Making Predictions
predictions_model_power_training = predict(model_power, training)
predictions_model_power_testing = predict(model_power, testing)


# ----------------------------- Model Wykoff ----------------------------- #
# Model Wykoff Fitting
model_wykoff <- nls(formula = HEIGHT ~ (bh + exp(a + b/(DIAMETER + 1))), 
                    data = training, start=list(
                      a=start_values_wykoff["a"], 
                      b=start_values_wykoff["b"]))
summary_model_wykoff <- summary(model_wykoff)
length_model_wykoff <- length(summary(model_wykoff)$coef[,1])
model_wykoff


# Making Predictions
predictions_model_wykoff_training = predict(model_wykoff, training)
predictions_model_wykoff_testing = predict(model_wykoff, testing)


# ----------------------------- Model Prodan ----------------------------- #
# Model Prodan Fitting
model_prodan <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(a + b * DIAMETER + c * DIAMETER^2)), 
                    data = training, start=list(
                      a=start_values_prodan["a"], 
                      b=start_values_prodan["b"],
                      c=start_values_prodan["c"]))
summary_model_prodan <- summary(model_prodan)
length_model_prodan <- length(summary(model_prodan)$coef[,1])
model_prodan


# Making Predictions
predictions_model_prodan_training = predict(model_prodan, training)
predictions_model_prodan_testing = predict(model_prodan, testing)


# Models Names
models_names <- c(
  "Model Naslund", "Model Curtis", "Model Power", "Model Wykoff", "Model Prodan")

# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$HEIGHT, 
  "Model Naslund_Predictions"=round(predictions_model_naslund_training, 1),
  "Model Curtis_Predictions"=round(predictions_model_curtis_training, 1),
  "Model Power_Predictions"=round(predictions_model_power_training, 1),
  "Model Wykoff_Predictions"=round(predictions_model_wykoff_training, 1),
  "Model Prodan_Predictions"=round(predictions_model_prodan_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "Model Naslund_Predictions"=round(predictions_model_naslund_testing, 1),
  "Model Curtis_Predictions"=round(predictions_model_curtis_testing, 1),
  "Model Power_Predictions"=round(predictions_model_power_testing, 1),
  "Model Wykoff_Predictions"=round(predictions_model_wykoff_testing, 1),
  "Model Prodan_Predictions"=round(predictions_model_prodan_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()

#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(
    round(summary_model_naslund$parameters[1], 2), round(summary_model_curtis$parameters[1], 2), 
    round(summary_model_power$parameters[1], 2), round(summary_model_wykoff$parameters[1], 2),
    round(summary_model_prodan$parameters[1], 2)
  ),
  "b" = c(
    round(summary_model_naslund$parameters[2], 2), round(summary_model_curtis$parameters[2], 2), 
    round(summary_model_power$parameters[2], 2), round(summary_model_wykoff$parameters[2], 2),
    round(summary_model_prodan$parameters[2], 2)
  ),
  "c" = c(
    "-", "-", "-", "-", round(summary_model_prodan$parameters[3], 2)
  )
)
rownames(parameters_nls) <- models_names


#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_model_naslund_training <- rmse(training$HEIGHT, predictions_model_naslund_training)
RMSE_model_curtis_training <- rmse(training$HEIGHT, predictions_model_curtis_training)
RMSE_model_power_training <- rmse(training$HEIGHT, predictions_model_power_training)
RMSE_model_wykoff_training <- rmse(training$HEIGHT, predictions_model_wykoff_training)
RMSE_model_prodan_training <- rmse(training$HEIGHT, predictions_model_prodan_training)
RMSE_training <- c(
  round(RMSE_model_naslund_training, 4), 
  round(RMSE_model_curtis_training, 4), 
  round(RMSE_model_power_training, 4), 
  round(RMSE_model_wykoff_training, 4),
  round(RMSE_model_prodan_training, 4)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_model_naslund_testing <- rmse(testing$HEIGHT, predictions_model_naslund_testing)
RMSE_model_curtis_testing <- rmse(testing$HEIGHT, predictions_model_curtis_testing)
RMSE_model_power_testing <- rmse(testing$HEIGHT, predictions_model_power_testing)
RMSE_model_wykoff_testing <- rmse(testing$HEIGHT, predictions_model_wykoff_testing)
RMSE_model_prodan_testing <- rmse(testing$HEIGHT, predictions_model_prodan_testing)
RMSE_testing <- c(
  round(RMSE_model_naslund_testing, 4), 
  round(RMSE_model_curtis_testing, 4), 
  round(RMSE_model_power_testing, 4), 
  round(RMSE_model_wykoff_testing, 4),
  round(RMSE_model_prodan_testing, 4)
)
summary_table_testing <- data.frame("RMSE" = RMSE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_model_naslund_training <- cor(training$HEIGHT, predictions_model_naslund_training)^2 # Correlation Coefficient
RSQUARED_model_curtis_training <- cor(training$HEIGHT, predictions_model_curtis_training)^2 # Correlation Coefficient
RSQUARED_model_power_training <- cor(training$HEIGHT, predictions_model_power_training)^2 # Correlation Coefficient
RSQUARED_model_wykoff_training <- cor(training$HEIGHT, predictions_model_wykoff_training)^2 # Correlation Coefficient
RSQUARED_model_prodan_training <- cor(training$HEIGHT, predictions_model_prodan_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_model_naslund_training, 4), 
  round(RSQUARED_model_curtis_training, 4), 
  round(RSQUARED_model_power_training, 4), 
  round(RSQUARED_model_wykoff_training, 4),
  round(RSQUARED_model_prodan_training, 4)
)
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_model_naslund_testing <- cor(testing$HEIGHT, predictions_model_naslund_testing)^2 # Correlation Coefficient
RSQUARED_model_curtis_testing <- cor(testing$HEIGHT, predictions_model_curtis_testing)^2 # Correlation Coefficient
RSQUARED_model_power_testing <- cor(testing$HEIGHT, predictions_model_power_testing)^2 # Correlation Coefficient
RSQUARED_model_wykoff_testing <- cor(testing$HEIGHT, predictions_model_wykoff_testing)^2 # Correlation Coefficient
RSQUARED_model_prodan_testing <- cor(testing$HEIGHT, predictions_model_prodan_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_model_naslund_testing, 4), 
  round(RSQUARED_model_curtis_testing, 4), 
  round(RSQUARED_model_power_testing, 4), 
  round(RSQUARED_model_wykoff_testing, 4),
  round(RSQUARED_model_prodan_testing, 4)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_model_naslund_training <- bias(training$HEIGHT, predictions_model_naslund_training)
bias_model_curtis_training <- bias(training$HEIGHT, predictions_model_curtis_training)
bias_model_power_training <- bias(training$HEIGHT, predictions_model_power_training)
bias_model_wykoff_training <- bias(training$HEIGHT, predictions_model_wykoff_training)
bias_model_prodan_training <- bias(training$HEIGHT, predictions_model_prodan_training)
BIAS_training <- c(
  round(bias_model_naslund_training, 3), 
  round(bias_model_curtis_training, 3), 
  round(bias_model_power_training, 3), 
  round(bias_model_wykoff_training, 3),
  round(bias_model_prodan_training, 3)
)
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_model_naslund_testing <- bias(testing$HEIGHT, predictions_model_naslund_testing)
bias_model_curtis_testing <- bias(testing$HEIGHT, predictions_model_curtis_testing)
bias_model_power_testing <- bias(testing$HEIGHT, predictions_model_power_testing)
bias_model_wykoff_testing <- bias(testing$HEIGHT, predictions_model_wykoff_testing)
bias_model_prodan_testing <- bias(testing$HEIGHT, predictions_model_prodan_testing)
BIAS_testing <- c(
  round(bias_model_naslund_testing, 3), 
  round(bias_model_curtis_testing, 3), 
  round(bias_model_power_testing, 3), 
  round(bias_model_wykoff_testing, 3),
  round(bias_model_prodan_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)


#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_model_naslund <- AIC(model_naslund)
AIC_model_curtis <- AIC(model_curtis)
AIC_model_power <- AIC(model_power)
AIC_model_wykoff <- AIC(model_wykoff)
AIC_model_prodan <- AIC(model_prodan)
AIC <- c(
  round(AIC_model_naslund, 3),
  round(AIC_model_curtis, 3),
  round(AIC_model_power, 3),
  round(AIC_model_wykoff, 3),
  round(AIC_model_prodan, 3)
)
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)

rownames(summary_table_training) <- models_names
rownames(summary_table_testing) <- models_names

# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_naslund_training, colour = "Naslund")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_curtis_training, colour = "Curtis")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_power_training, colour = "Power")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_wykoff_training, colour = "Wykoff")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_prodan_training, colour = "Prodan")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.98, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Naslund"="#0f0f10",
    "Curtis"="#eb7de3",
    "Power"="#eb1919",
    "Wykoff"="#c9b424",
    "Prodan"="#2533d3"
  )) 

# Model Residuals Table
residuals_table <- data.frame(
  "Naslund" = residuals(model_naslund),
  "Curtis" = residuals(model_curtis),
  "Power" = residuals(model_power),
  "Wykoff" = residuals(model_wykoff),
  "Prodan" = residuals(model_prodan)
)

# Scattered Plot (Model Performance Line) -- Naslund
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_naslund_training, colour = "Naslund")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Line (Naslund)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Naslund"="#120c4c")) 


# Scattered Plot (Model Performance Curve) -- Curtis
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_curtis_training, colour = "Curtis")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Curtis)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Curtis"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Power
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_power_training, colour = "Power")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Wykoff
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_wykoff_training, colour = "Wykoff")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Wykoff)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Wykoff"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Prodan
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_prodan_training, colour = "Prodan")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Prodan)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Prodan"="#120c4c"))


# Scattered Plot (Fitted VS Residuals Values) -- Prodan
ggplot() +
  geom_point(aes(x = fitted(model_prodan), y = residuals(model_prodan), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Prodan")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Prodan)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Prodan"="#4954ed")) 

plot(model_prodan, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Prodan",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Fitted VS Residuals Values) -- Power
ggplot() +
  geom_point(aes(x = fitted(model_power), y = residuals(model_power), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Power")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Power)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#4954ed")) 

plot(model_power, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Power",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Prodan
ggplot(residuals_table, aes(sample = Prodan)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Prodan)")

qqnorm(residuals(model_prodan, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Prodan", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_prodan,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Power
ggplot(residuals_table, aes(sample = Power)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Power)")

qqnorm(residuals(model_power, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Power", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_power,type = "pearson"))


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

# Height Classes
class_1_5_tht <- dataset %>% filter(HEIGHT > 1 & HEIGHT <= 5)
class_5_10_tht <- dataset %>% filter(HEIGHT > 5 & HEIGHT <= 10)
class_10_15_tht <- dataset %>% filter(HEIGHT > 10 & HEIGHT <= 15)
class_15_20_tht <- dataset %>% filter(HEIGHT > 15 & HEIGHT <= 20)
class_20_25_tht <- dataset %>% filter(HEIGHT > 20 & HEIGHT <= 25)
class_25_tht <- dataset %>% filter(HEIGHT > 25)

# Summary Variables Class Table
density <- area_of_plot_ha * number_of_plots
# DBH
class_1_5_dbh_density <- length(class_1_5_dbh$DIAMETER)
class_5_10_dbh_density <- length(class_5_10_dbh$DIAMETER)
class_10_15_dbh_density <- length(class_10_15_dbh$DIAMETER)
class_15_20_dbh_density <- length(class_15_20_dbh$DIAMETER)
class_20_25_dbh_density <- length(class_20_25_dbh$DIAMETER)
class_25_dbh_density <- length(class_25_dbh$DIAMETER)
# THT
class_1_5_tht_density <- length(class_1_5_tht$HEIGHT)
class_5_10_tht_density <- length(class_5_10_tht$HEIGHT)
class_10_15_tht_density <- length(class_10_15_tht$HEIGHT)
class_15_20_tht_density <- length(class_15_20_tht$HEIGHT)
class_20_25_tht_density <- length(class_20_25_tht$HEIGHT)
class_25_tht_density <- length(class_25_tht$HEIGHT)

summary_variable_classes_table <- data.frame(
  "DBH.density" = c(class_1_5_dbh_density, class_5_10_dbh_density, class_10_15_dbh_density, class_15_20_dbh_density, class_20_25_dbh_density, class_25_dbh_density),
  "DBH.trees.ha" = c((class_1_5_dbh_density/density), (class_5_10_dbh_density/density), (class_10_15_dbh_density/density), (class_15_20_dbh_density/density), (class_20_25_dbh_density/density), (class_25_dbh_density/density)),
  "THT.density" = c(class_1_5_tht_density, class_5_10_tht_density, class_10_15_tht_density, class_15_20_tht_density, class_20_25_tht_density, class_25_tht_density),
  "THT.trees.ha" = c((class_1_5_tht_density/density), (class_5_10_tht_density/density), (class_10_15_tht_density/density), (class_15_20_tht_density/density), (class_20_25_tht_density/density), (class_25_tht_density/density))
)
summary_variable_classes_table <- rbind(summary_variable_classes_table, 
                                        c(sum(summary_variable_classes_table$DBH.density), sum(summary_variable_classes_table$DBH.trees.ha), 
                                          sum(summary_variable_classes_table$THT.density), sum(summary_variable_classes_table$THT.trees.ha)
                                        )
)
rownames(summary_variable_classes_table) <- c("1 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25", "25>", "Total")


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
