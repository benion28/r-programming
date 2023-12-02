#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, caTools, ggplot2, psych, tidyverse, Metrics, lmfor, dplyr, nlme)


# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//35 H-D")


# Import data set
dataset <-  read.csv("full-data-14.csv", head=TRUE, sep=",")
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
training <- read.csv("training-data-14.csv", head=TRUE, sep=",")
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- read.csv("testing-data-14.csv", head=TRUE, sep=",")
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
start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh)
start_values_prodan <- startHDprodan(training$DIAMETER, training$HEIGHT, bh)
start_values_logistic <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh)
start_values_ratkowsky <- startHDratkowsky(training$DIAMETER, training$HEIGHT, bh)
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh)
start_values_korf <- startHDkorf(training$DIAMETER, training$HEIGHT, bh)
start_values_hossfeldIV <- startHDhossfeldIV(training$DIAMETER, training$HEIGHT, bh)


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


# ----------------------------- Model Logistic ----------------------------- #4
# Model Logistic Fitting
model_logistic <- nls(formula = HEIGHT ~ (bh + a/(1 + b * exp(-c * DIAMETER))), 
                      data = training, start=list(
                        a=start_values_logistic["a"], 
                        b=start_values_logistic["b"],
                        c=start_values_logistic["c"]))
summary_model_logistic <- summary(model_logistic)
length_model_logistic <- length(summary(model_logistic)$coef[,1])
model_logistic


# Making Predictions
predictions_model_logistic_training = predict(model_logistic, training)
predictions_model_logistic_testing = predict(model_logistic, testing)


# ----------------------------- Model Ratkowsky ----------------------------- #5
# Model Ratkowsky Fitting
model_ratkowsky <- nls(formula = HEIGHT ~ (bh + a * exp(-b/(DIAMETER + c))), 
                       data = training, start=list(
                         a=start_values_ratkowsky["a"], 
                         b=start_values_ratkowsky["b"],
                         c=start_values_ratkowsky["c"]))
summary_model_ratkowsky <- summary(model_ratkowsky)
length_model_ratkowsky <- length(summary(model_ratkowsky)$coef[,1])
model_ratkowsky


# Making Predictions
predictions_model_ratkowsky_training = predict(model_ratkowsky, training)
predictions_model_ratkowsky_testing = predict(model_ratkowsky, testing)


# ----------------------------- Model Chapman ----------------------------- #
# Model Chapman Fitting
model_chapman <- nls(formula = HEIGHT ~ (bh + a *(1 - exp(-b*DIAMETER))^c), 
                     data = training, start=list(
                       a=start_values_chapman["a"], 
                       b=start_values_chapman["b"],
                       c=start_values_chapman["c"]))
summary_model_chapman <- summary(model_chapman)
length_model_chapman <- length(summary(model_chapman)$coef[,1])
model_chapman


# Making Predictions
predictions_model_chapman_training = predict(model_chapman, training)
predictions_model_chapman_testing = predict(model_chapman, testing)


# ----------------------------- Model Weibull ----------------------------- #
# Model Weibull Fitting
model_weibull <- nls(formula = HEIGHT ~ (bh + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(
                       a=start_values_weibull["a"], 
                       b=start_values_weibull["b"],
                       c=start_values_weibull["c"]))
summary_model_weibull <- summary(model_weibull)
length_model_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull


# Making Predictions
predictions_model_weibull_training = predict(model_weibull, training)
predictions_model_weibull_testing = predict(model_weibull, testing)


# ----------------------------- Model Korf ----------------------------- #
# Model Korf Fitting
model_korf <- nls(formula = HEIGHT ~ (bh + a * exp(-b * DIAMETER^(-c))), 
                  data = training, start=list(
                    a=start_values_korf["a"], 
                    b=start_values_korf["b"],
                    c=start_values_korf["c"]))
summary_model_korf <- summary(model_korf)
length_model_korf <- length(summary(model_korf)$coef[,1])
model_korf


# Making Predictions
predictions_model_korf_training = predict(model_korf, training)
predictions_model_korf_testing = predict(model_korf, testing)


# ----------------------------- Model Hossfeld IV ----------------------------- #
# Model Hossfeld IV Fitting
model_hossfeldIV <- nls(formula = HEIGHT ~ (bh + a/(1 + 1/(b*DIAMETER^c))),
                        data = training, start=list(
                          a=start_values_hossfeldIV["a"], 
                          b=start_values_hossfeldIV["b"],
                          c=start_values_hossfeldIV["c"]))
summary_model_hossfeldIV <- summary(model_hossfeldIV)
length_model_hossfeldIV <- length(summary(model_hossfeldIV)$coef[,1])
model_hossfeldIV


# Making Predictions
predictions_model_hossfeldIV_training = predict(model_hossfeldIV, training)
predictions_model_hossfeldIV_testing = predict(model_hossfeldIV, testing)


# Models Names
models_names <- c(
  "Model Chapman", "Model Weibull", "Model Power", "Model Korf",
  "Model Prodan", "Model Logistic", "Model Hossfeld IV", "Model Ratkowsky")

# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$HEIGHT, 
  "Model Chapman_Predictions"=round(predictions_model_chapman_training, 1),
  "Model Weibull_Predictions"=round(predictions_model_weibull_training, 1),
  "Model Power_Predictions"=round(predictions_model_power_training, 1),
  "Model Korf_Predictions"=round(predictions_model_korf_training, 1),
  "Model Prodan_Predictions"=round(predictions_model_prodan_training, 1),
  "Model Logistic_Predictions"=round(predictions_model_logistic_training, 1),
  "Model Hossfeld_IV_Predictions"=round(predictions_model_hossfeldIV_training, 1),
  "Model Ratkowsky_Predictions"=round(predictions_model_ratkowsky_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "Model Chapman_Predictions"=round(predictions_model_chapman_testing, 1),
  "Model Weibull_Predictions"=round(predictions_model_weibull_testing, 1),
  "Model Power_Predictions"=round(predictions_model_power_testing, 1),
  "Model Korf_Predictions"=round(predictions_model_korf_testing, 1),
  "Model Prodan_Predictions"=round(predictions_model_prodan_testing, 1),
  "Model Logistic_Predictions"=round(predictions_model_logistic_testing, 1),
  "Model Hossfeld_IV_Predictions"=round(predictions_model_hossfeldIV_testing, 1),
  "Model Ratkowsky_Predictions"=round(predictions_model_ratkowsky_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()

#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(
    round(summary_model_chapman$parameters[1], 2), round(summary_model_weibull$parameters[1], 2), 
    round(summary_model_power$parameters[1], 2), round(summary_model_korf$parameters[1], 2),
    round(summary_model_prodan$parameters[1], 2), round(summary_model_logistic$parameters[1], 2), 
    round(summary_model_hossfeldIV$parameters[1], 2), round(summary_model_ratkowsky$parameters[1], 2)
  ),
  "b" = c(
    round(summary_model_chapman$parameters[2], 2), round(summary_model_weibull$parameters[2], 2), 
    round(summary_model_power$parameters[2], 2), round(summary_model_korf$parameters[2], 2),
    round(summary_model_prodan$parameters[2], 2), round(summary_model_logistic$parameters[2], 2), 
    round(summary_model_hossfeldIV$parameters[2], 2), round(summary_model_ratkowsky$parameters[2], 2)
  ),
  "c" = c(
    round(summary_model_chapman$parameters[3], 2), round(summary_model_weibull$parameters[3], 2),
    "-", "-",
    round(summary_model_prodan$parameters[3], 2), round(summary_model_logistic$parameters[3], 2), 
    round(summary_model_hossfeldIV$parameters[3], 2), round(summary_model_ratkowsky$parameters[3], 2)
  )
)
rownames(parameters_nls) <- models_names


#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_model_chapman_training <- rmse(training$HEIGHT, predictions_model_chapman_training)
RMSE_model_weibull_training <- rmse(training$HEIGHT, predictions_model_weibull_training)
RMSE_model_power_training <- rmse(training$HEIGHT, predictions_model_power_training)
RMSE_model_korf_training <- rmse(training$HEIGHT, predictions_model_korf_training)
RMSE_model_prodan_training <- rmse(training$HEIGHT, predictions_model_prodan_training)
RMSE_model_logistic_training <- rmse(training$HEIGHT, predictions_model_logistic_training)
RMSE_model_hossfeldIV_training <- rmse(training$HEIGHT, predictions_model_hossfeldIV_training)
RMSE_model_ratkowsky_training <- rmse(training$HEIGHT, predictions_model_ratkowsky_training)
RMSE_training <- c(
  round(RMSE_model_chapman_training, 4), 
  round(RMSE_model_weibull_training, 4), 
  round(RMSE_model_power_training, 4), 
  round(RMSE_model_korf_training, 4),
  round(RMSE_model_prodan_training, 4), 
  round(RMSE_model_logistic_training, 4), 
  round(RMSE_model_hossfeldIV_training, 4), 
  round(RMSE_model_ratkowsky_training, 4)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_model_chapman_testing <- rmse(testing$HEIGHT, predictions_model_chapman_testing)
RMSE_model_weibull_testing <- rmse(testing$HEIGHT, predictions_model_weibull_testing)
RMSE_model_power_testing <- rmse(testing$HEIGHT, predictions_model_power_testing)
RMSE_model_korf_testing <- rmse(testing$HEIGHT, predictions_model_korf_testing)
RMSE_model_prodan_testing <- rmse(testing$HEIGHT, predictions_model_prodan_testing)
RMSE_model_logistic_testing <- rmse(testing$HEIGHT, predictions_model_logistic_testing)
RMSE_model_hossfeldIV_testing <- rmse(testing$HEIGHT, predictions_model_hossfeldIV_testing)
RMSE_model_ratkowsky_testing <- rmse(testing$HEIGHT, predictions_model_ratkowsky_testing)
RMSE_testing <- c(
  round(RMSE_model_chapman_testing, 4), 
  round(RMSE_model_weibull_testing, 4), 
  round(RMSE_model_power_testing, 4), 
  round(RMSE_model_korf_testing, 4),
  round(RMSE_model_prodan_testing, 4), 
  round(RMSE_model_logistic_testing, 4), 
  round(RMSE_model_hossfeldIV_testing, 4), 
  round(RMSE_model_ratkowsky_testing, 4)
)
summary_table_testing <- data.frame("RMSE" = RMSE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_model_chapman_training <- cor(training$HEIGHT, predictions_model_chapman_training)^2 # Correlation Coefficient
RSQUARED_model_weibull_training <- cor(training$HEIGHT, predictions_model_weibull_training)^2 # Correlation Coefficient
RSQUARED_model_power_training <- cor(training$HEIGHT, predictions_model_power_training)^2 # Correlation Coefficient
RSQUARED_model_korf_training <- cor(training$HEIGHT, predictions_model_korf_training)^2 # Correlation Coefficient
RSQUARED_model_prodan_training <- cor(training$HEIGHT, predictions_model_prodan_training)^2 # Correlation Coefficient
RSQUARED_model_logistic_training <- cor(training$HEIGHT, predictions_model_logistic_training)^2 # Correlation Coefficient
RSQUARED_model_hossfeldIV_training <- cor(training$HEIGHT, predictions_model_hossfeldIV_training)^2 # Correlation Coefficient
RSQUARED_model_ratkowsky_training <- cor(training$HEIGHT, predictions_model_ratkowsky_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_model_chapman_training, 4), 
  round(RSQUARED_model_weibull_training, 4), 
  round(RSQUARED_model_power_training, 4), 
  round(RSQUARED_model_korf_training, 4),
  round(RSQUARED_model_prodan_training, 4), 
  round(RSQUARED_model_logistic_training, 4), 
  round(RSQUARED_model_hossfeldIV_training, 4), 
  round(RSQUARED_model_ratkowsky_training, 4)
)
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_model_chapman_testing <- cor(testing$HEIGHT, predictions_model_chapman_testing)^2 # Correlation Coefficient
RSQUARED_model_weibull_testing <- cor(testing$HEIGHT, predictions_model_weibull_testing)^2 # Correlation Coefficient
RSQUARED_model_power_testing <- cor(testing$HEIGHT, predictions_model_power_testing)^2 # Correlation Coefficient
RSQUARED_model_korf_testing <- cor(testing$HEIGHT, predictions_model_korf_testing)^2 # Correlation Coefficient
RSQUARED_model_prodan_testing <- cor(testing$HEIGHT, predictions_model_prodan_testing)^2 # Correlation Coefficient
RSQUARED_model_logistic_testing <- cor(testing$HEIGHT, predictions_model_logistic_testing)^2 # Correlation Coefficient
RSQUARED_model_hossfeldIV_testing <- cor(testing$HEIGHT, predictions_model_hossfeldIV_testing)^2 # Correlation Coefficient
RSQUARED_model_ratkowsky_testing <- cor(testing$HEIGHT, predictions_model_ratkowsky_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_model_chapman_testing, 4), 
  round(RSQUARED_model_weibull_testing, 4), 
  round(RSQUARED_model_power_testing, 4), 
  round(RSQUARED_model_korf_testing, 4),
  round(RSQUARED_model_prodan_testing, 4), 
  round(RSQUARED_model_logistic_testing, 4), 
  round(RSQUARED_model_hossfeldIV_testing, 4), 
  round(RSQUARED_model_ratkowsky_testing, 4)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_model_chapman_training <- bias(training$HEIGHT, predictions_model_chapman_training)
bias_model_weibull_training <- bias(training$HEIGHT, predictions_model_weibull_training)
bias_model_power_training <- bias(training$HEIGHT, predictions_model_power_training)
bias_model_korf_training <- bias(training$HEIGHT, predictions_model_korf_training)
bias_model_prodan_training <- bias(training$HEIGHT, predictions_model_prodan_training)
bias_model_logistic_training <- bias(training$HEIGHT, predictions_model_logistic_training)
bias_model_hossfeldIV_training <- bias(training$HEIGHT, predictions_model_hossfeldIV_training)
bias_model_ratkowsky_training <- bias(training$HEIGHT, predictions_model_ratkowsky_training)
BIAS_training <- c(
  round(bias_model_chapman_training, 3), 
  round(bias_model_weibull_training, 3), 
  round(bias_model_power_training, 3), 
  round(bias_model_korf_training, 3),
  round(bias_model_prodan_training, 3), 
  round(bias_model_logistic_training, 3), 
  round(bias_model_hossfeldIV_training, 3), 
  round(bias_model_ratkowsky_training, 3)
)
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_model_chapman_testing <- bias(testing$HEIGHT, predictions_model_chapman_testing)
bias_model_weibull_testing <- bias(testing$HEIGHT, predictions_model_weibull_testing)
bias_model_power_testing <- bias(testing$HEIGHT, predictions_model_power_testing)
bias_model_korf_testing <- bias(testing$HEIGHT, predictions_model_korf_testing)
bias_model_prodan_testing <- bias(testing$HEIGHT, predictions_model_prodan_testing)
bias_model_logistic_testing <- bias(testing$HEIGHT, predictions_model_logistic_testing)
bias_model_hossfeldIV_testing <- bias(testing$HEIGHT, predictions_model_hossfeldIV_testing)
bias_model_ratkowsky_testing <- bias(testing$HEIGHT, predictions_model_ratkowsky_testing)
BIAS_testing <- c(
  round(bias_model_chapman_testing, 3), 
  round(bias_model_weibull_testing, 3), 
  round(bias_model_power_testing, 3), 
  round(bias_model_korf_testing, 3),
  round(bias_model_prodan_testing, 3), 
  round(bias_model_logistic_testing, 3), 
  round(bias_model_hossfeldIV_testing, 3), 
  round(bias_model_ratkowsky_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)


#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_model_chapman <- AIC(model_chapman)
AIC_model_weibull <- AIC(model_weibull)
AIC_model_power <- AIC(model_power)
AIC_model_korf <- AIC(model_korf)
AIC_model_prodan <- AIC(model_prodan)
AIC_model_logistic <- AIC(model_logistic)
AIC_model_hossfeldIV <- AIC(model_hossfeldIV)
AIC_model_ratkowsky <- AIC(model_ratkowsky)
AIC <- c(
  round(AIC_model_chapman, 3),
  round(AIC_model_weibull, 3),
  round(AIC_model_power, 3),
  round(AIC_model_korf, 3),
  round(AIC_model_prodan, 3),
  round(AIC_model_logistic, 3),
  round(AIC_model_hossfeldIV, 3),
  round(AIC_model_ratkowsky, 3)
)
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)

rownames(summary_table_training) <- models_names
rownames(summary_table_testing) <- models_names

# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_chapman_training, colour = "Chapman")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_weibull_training, colour = "Weibull")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_power_training, colour = "Power")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_korf_training, colour = "Korf")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_prodan_training, colour = "Prodan")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_logistic_training, colour = "Logistic")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_hossfeldIV_training, colour = "Hossfeld IV")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_ratkowsky_training, colour = "Ratkowsky")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.98, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Chapman"="#0f0f10",
    "Weibull"="#eb7de3",
    "Power"="#eb1919",
    "Korf"="#c9b424",
    "Prodan"="#2533d3", 
    "Logistic"="#69ef79",
    "Hossfeld IV"="#880373",
    "Ratkowsky"="#085b5b"
  )) 

# Model Residuals Table
residuals_table <- data.frame(
  "Chapman" = residuals(model_chapman),
  "Weibull" = residuals(model_weibull),
  "Power" = residuals(model_power),
  "Korf" = residuals(model_korf),
  "Prodan" = residuals(model_prodan),
  "Logistics" = residuals(model_logistic),
  "Hossfeld_IV" = residuals(model_hossfeldIV),
  "Ratkowsky" = residuals(model_ratkowsky)
)

# Scattered Plot (Model Performance Line) -- Chapman
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_chapman_training, colour = "Chapman")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Line (Chapman)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Chapman"="#120c4c")) 


# Scattered Plot (Model Performance Curve) -- Weibull
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_weibull_training, colour = "Weibull")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Weibull)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Weibull"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Power
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_power_training, colour = "Power")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Korf
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_korf_training, colour = "Korf")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Korf)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Korf"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Prodan
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_prodan_training, colour = "Prodan")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Prodan)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Prodan"="#120c4c"))

# Scattered Plot (Model Performance Curve) -- Logistics
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_logistic_training, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Logistics"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Hossfeld IV
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_hossfeldIV_training, colour = "Hossfeld IV")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Hossfeld IV)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Hossfeld IV"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Ratkowsky
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_model_ratkowsky_training, colour = "Ratkowsky")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Ratkowsky)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Ratkowsky"="#120c4c")) 

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


# Scattered Plot (Fitted VS Residuals Values) -- Logistic
ggplot() +
  geom_point(aes(x = fitted(model_logistic), y = residuals(model_logistic), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Logistic")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Logistic)", color = "Legend") +
  theme(legend.position = c(0.99, 0.05), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Logistic"="#4954ed")) 

plot(model_logistic, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Logistic",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Weibull
ggplot(residuals_table, aes(sample = Weibull)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Weibull)")

qqnorm(residuals(model_weibull, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Weibull", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_weibull,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Logistic
ggplot(residuals_table, aes(sample = Logistics)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Logistic)")

qqnorm(residuals(model_logistic, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Logistic", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_logistic,type = "pearson"))


#------------------------------ Number of Trees per Hectar --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 20 * 20
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 15
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
