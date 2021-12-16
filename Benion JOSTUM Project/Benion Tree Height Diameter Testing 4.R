#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics, neuralnet, lmfor, dplyr, nlme)

# Set Working Directory
setwd("C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project")


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\benion-tree-height-diameter-dataset-new.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset


#-------------------- EXploratory Data Analysis ----------------------------#

# All Species Distribution Plot
ggplot() +
  geom_point(aes(x = dataset$DIAMETER, y = dataset$HEIGHT), colour = "#a32e2e") +
  ggtitle("Tree Height vs Diameter (Data Distribution)") +
  xlab("Diameter") +
  ylab("Height")

#---------- Unique Species Distribution Table ---------------------#

# Tectona grandis Species Table
tg_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Tectona grandis"]
tg_species <- dataset$SPECIES[dataset$SPECIES == "Tectona grandis"]
tg_diameter <- dataset$DIAMETER[dataset$SPECIES == "Tectona grandis"]
tg_height <- dataset$HEIGHT[dataset$SPECIES == "Tectona grandis"]
t_grandis_table <- data.frame("Plot No"=tg_plot_no, "Species"=tg_species, "Diameter"=tg_diameter, "Height"=tg_height)

# Kashier(Tiv) Species Table
k_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Kashier(Tiv)"]
k_species <- dataset$SPECIES[dataset$SPECIES == "Kashier(Tiv)"]
k_diameter <- dataset$DIAMETER[dataset$SPECIES == "Kashier(Tiv)"]
k_height <- dataset$HEIGHT[dataset$SPECIES == "Kashier(Tiv)"]
kashier_table <- data.frame("Plot No"=k_plot_no, "Species"=k_species, "Diameter"=k_diameter, "Height"=k_height)

# Daniella olivera Species Table
do_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Daniella olivera"]
do_species <- dataset$SPECIES[dataset$SPECIES == "Daniella olivera"]
do_diameter <- dataset$DIAMETER[dataset$SPECIES == "Daniella olivera"]
do_height <- dataset$HEIGHT[dataset$SPECIES == "Daniella olivera"]
d_olivera_table <- data.frame("Plot No"=do_plot_no, "Species"=do_species, "Diameter"=do_diameter, "Height"=do_height)

# Gmelina arborea Species Table
ga_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Gmelina arborea"]
ga_species <- dataset$SPECIES[dataset$SPECIES == "Gmelina arborea"]
ga_diameter <- dataset$DIAMETER[dataset$SPECIES == "Gmelina arborea"]
ga_height <- dataset$HEIGHT[dataset$SPECIES == "Gmelina arborea"]
g_arborea_table <- data.frame("Plot No"=ga_plot_no, "Species"=ga_species, "Diameter"=ga_diameter, "Height"=ga_height)

# Khaya senegalensis Species Table
ks_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Khaya senegalensis"]
ks_species <- dataset$SPECIES[dataset$SPECIES == "Khaya senegalensis"]
ks_diameter <- dataset$DIAMETER[dataset$SPECIES == "Khaya senegalensis"]
ks_height <- dataset$HEIGHT[dataset$SPECIES == "Khaya senegalensis"]
k_senegalensis_table <- data.frame("Plot No"=ks_plot_no, "Species"=ks_species, "Diameter"=ks_diameter, "Height"=ks_height)

# Vitex doniana Species Table
vd_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Vitex doniana"]
vd_species <- dataset$SPECIES[dataset$SPECIES == "Vitex doniana"]
vd_diameter <- dataset$DIAMETER[dataset$SPECIES == "Vitex doniana"]
vd_height <- dataset$HEIGHT[dataset$SPECIES == "Vitex doniana"]
v_doniana_table <- data.frame("Plot No"=vd_plot_no, "Species"=vd_species, "Diameter"=vd_diameter, "Height"=vd_height)

# Anthocleista djalonensis Species Table
ad_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Anthocleista djalonensis"]
ad_species <- dataset$SPECIES[dataset$SPECIES == "Anthocleista djalonensis"]
ad_diameter <- dataset$DIAMETER[dataset$SPECIES == "Anthocleista djalonensis"]
ad_height <- dataset$HEIGHT[dataset$SPECIES == "Anthocleista djalonensis"]
a_djalonensis_table <- data.frame("Plot No"=ad_plot_no, "Species"=ad_species, "Diameter"=ad_diameter, "Height"=ad_height)

# Mangifera indica Species Table
mi_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Mangifera indica"]
mi_species <- dataset$SPECIES[dataset$SPECIES == "Mangifera indica"]
mi_diameter <- dataset$DIAMETER[dataset$SPECIES == "Mangifera indica"]
mi_height <- dataset$HEIGHT[dataset$SPECIES == "Mangifera indica"]
m_indica_table <- data.frame("Plot No"=mi_plot_no, "Species"=mi_species, "Diameter"=mi_diameter, "Height"=mi_height)

# Afzelia africana Species Table
aa_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Afzelia africana"]
aa_species <- dataset$SPECIES[dataset$SPECIES == "Afzelia africana"]
aa_diameter <- dataset$DIAMETER[dataset$SPECIES == "Afzelia africana"]
aa_height <- dataset$HEIGHT[dataset$SPECIES == "Afzelia africana"]
a_africana_table <- data.frame("Plot No"=aa_plot_no, "Species"=aa_species, "Diameter"=aa_diameter, "Height"=aa_height)

# Pterocarpus erinaceus Species Table
pe_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Pterocarpus erinaceus"]
pe_species <- dataset$SPECIES[dataset$SPECIES == "Pterocarpus erinaceus"]
pe_diameter <- dataset$DIAMETER[dataset$SPECIES == "Pterocarpus erinaceus"]
pe_height <- dataset$HEIGHT[dataset$SPECIES == "Pterocarpus erinaceus"]
p_erinaceus_table <- data.frame("Plot No"=pe_plot_no, "Species"=pe_species, "Diameter"=pe_diameter, "Height"=pe_height)

# Parkia biglobosa Species Table
pb2_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Parkia biglobosa"]
pb2_species <- dataset$SPECIES[dataset$SPECIES == "Parkia biglobosa"]
pb2_diameter <- dataset$DIAMETER[dataset$SPECIES == "Parkia biglobosa"]
pb2_height <- dataset$HEIGHT[dataset$SPECIES == "Parkia biglobosa"]
p_biglobosa_table <- data.frame("Plot No"=pb2_plot_no, "Species"=pb2_species, "Diameter"=pb2_diameter, "Height"=pb2_height)

# Lannea schimperi Species Table
ls_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Lannea schimperi"]
ls_species <- dataset$SPECIES[dataset$SPECIES == "Lannea schimperi"]
ls_diameter <- dataset$DIAMETER[dataset$SPECIES == "Lannea schimperi"]
ls_height <- dataset$HEIGHT[dataset$SPECIES == "Lannea schimperi"]
l_schimperi_table <- data.frame("Plot No"=ls_plot_no, "Species"=ls_species, "Diameter"=ls_diameter, "Height"=ls_height)

# Sarcocephalus latifolius Species Table
sl_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Sarcocephalus latifolius"]
sl_species <- dataset$SPECIES[dataset$SPECIES == "Sarcocephalus latifolius"]
sl_diameter <- dataset$DIAMETER[dataset$SPECIES == "Sarcocephalus latifolius"]
sl_height <- dataset$HEIGHT[dataset$SPECIES == "Sarcocephalus latifolius"]
s_latifolius_table <- data.frame("Plot No"=sl_plot_no, "Species"=sl_species, "Diameter"=sl_diameter, "Height"=sl_height)

# Ficus sur Species Table
fs_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Ficus sur"]
fs_species <- dataset$SPECIES[dataset$SPECIES == "Ficus sur"]
fs_diameter <- dataset$DIAMETER[dataset$SPECIES == "Ficus sur"]
fs_height <- dataset$HEIGHT[dataset$SPECIES == "Ficus sur"]
f_sur_table <- data.frame("Plot No"=fs_plot_no, "Species"=fs_species, "Diameter"=fs_diameter, "Height"=fs_height)


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


# Frequency Distribution Bar
frequency_dist_bar <- data.frame(
  "Species" = frequency_table$Species[frequency_table$Species != "Total"],
  "Initials" = c("T.a", "G.a", "K", "K.s", "D.o", "V.d", "A.d", "M.i", "A.a", "P.e", "P.b", "L.s", "S.l", "F.s"),
  "Frequency" = frequency_table$Frequency[frequency_table$Species != "Total"]
  )

ggplot(frequency_dist_bar, aes(x = Initials, y = Frequency)) +
  geom_col(fill = "#4038bb", color = "#e92929") +
  labs(title = "Species Frequency Distribution Bar", x = "Species", y = "Frequency")


ggplot(frequency_dist_bar, aes(x = Initials, y = as.integer(Frequency))) +
  geom_col(fill = "#161516", color = "#5d199b") +
  labs(title = "Species Frequency Distribution Bar", x = "Species", y = "Frequency")


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


#-------------------------------- SVR Model ------------------------------#
# SVR Model Fitting
model_svr <-  svm(formula = HEIGHT ~ DIAMETER, data = training, type = "eps-regression")
summary(model_svr)
summary_model_svr <- summary(model_svr)
model_svr

# Making Predictions
predictions_svr = predict(model_svr, testing)


#-------------------------------- ANN Model ------------------------------#
# ANN Model Fitting
model_ann <-  neuralnet(formula = HEIGHT ~ DIAMETER, data = training, hidden = 2, linear.output = TRUE)
summary(model_ann)
summary_model_ann <- summary(model_ann)
model_ann

# Making Predictions
predictions_ann = predict(model_ann, testing)


# Starting Values 
start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_logistics <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh=1.3)

# Starting Values Table
starting_values_tables <- data.frame(
  "a" = c(
    round(start_values_power["a"], 2), 
    round(start_values_chapman["a"], 2), 
    round(start_values_logistics["a"], 2), 
    round(start_values_weibull["a"], 2)
    ),
  "b" = c(
    round(start_values_power["b"], 2), 
    round(start_values_chapman["b"], 2), 
    round(start_values_logistics["b"], 2), 
    round(start_values_weibull["b"], 2)
  ),
  "c" = c(
    "-",
    round(start_values_logistics["c"], 2),
    round(start_values_logistics["c"], 2), 
    round(start_values_weibull["c"], 2)
  )
)
rownames(starting_values_tables) <- c("Power", "Chapman", "Logistics", "Weibull")

# ----------------------------- Power Model----------------------------- #
# Power Model Fitting
model_power <- nls(formula = HEIGHT ~ (1.3 + a * DIAMETER^b), 
                   data = training, start=list(
                     a=start_values_power["a"], 
                     b=start_values_power["b"]))
summary_power <- summary(model_power)
length_power <- length(summary(model_power)$coef[,1])
model_power

# Making Predictions
predictions_power = predict(model_power, testing)


# ----------------------------- Chapman-Richards ----------------------------- #
# Chapman-Richards Model Fitting
model_chapman <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER))^c), 
                     data = training, start=list(
                       a=start_values_chapman["a"], 
                       b=start_values_chapman["b"], 
                       c=start_values_chapman["c"]))
summary_chapman <- summary(model_chapman)
length_chapman <- length(summary(model_chapman)$coef[,1])
model_chapman

# Making Predictions
predictions_chapman = predict(model_chapman, testing)


# ----------------------------- Logistics ----------------------------- #
# Logistics Model Fitting
model_logistics <- nls(formula = HEIGHT ~ (1.3 + a / (1 + b * exp(-c * DIAMETER))),
                       data = training, start=list(a=start_values_logistics["a"],
                                                   b=start_values_logistics["b"],
                                                   c=start_values_logistics["c"]))
summary_logistics <- summary(model_logistics)
length_logistics <- length(summary(model_logistics)$coef[,1])
model_logistics

# Making Predictions
predictions_logistics = predict(model_logistics, testing)


# ----------------------------- Weibull ----------------------------- #
# Weibull Model Fitting
model_weibull <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(a=start_values_weibull["a"], 
                                                 b=start_values_weibull["b"], 
                                                 c=start_values_weibull["c"]))
summary_weibull <- summary(model_weibull)
length_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull

# Making Predictions
predictions_weibull = predict(model_weibull, testing)

# Model Comparison Frame
prediction_frame <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "SVR_Predictions"=round(predictions_svr, 1),
  "ANN_Predictions"=round(predictions_ann, 1),
  "Power_Predictions"=round(predictions_power, 1),
  "Chapman_Predictions"=round(predictions_chapman, 1),
  "Logistics_Predictions"=round(predictions_logistics, 1),
  "Weibull_Predictions"=round(predictions_weibull, 1)
)

# Summary Table
summary_table <- data.frame()


# ---------------------- Manual Predictions -------------------------------#
predictor_variable <- data.frame(DIAMETER = 20.4)

# SVR
predict_svr <- predict(model_svr, predictor_variable)

# ANN
predict_ann <- predict(model_ann, predictor_variable)

# Power
predict_power <- predict(model_power, predictor_variable)

# Chapman-Richards
predict_chapman <- predict(model_chapman, predictor_variable)

# Logistics
predict_logistics <- predict(model_logistics, predictor_variable)

# Weibull
predict_weibull <- predict(model_weibull, predictor_variable)

# Predict Table
predict_table <- data.frame(
  "DIAMETER" = predictor_variable$DIAMETER,
  "HEIGHT" = 7.5,
  "SVR_Predict" = round(predict_svr, 1), 
  "ANN_Predict" = round(predict_ann, 1), 
  "Power_Predict" = round(predict_power, 1),
  "Chapman_Predict" = round(predict_chapman, 1),
  "Logistics_Predict" = round(predict_logistics, 1),
  "Weibull_Predict" = round(predict_weibull, 1)
)

#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
RMSE_svr <- rmse(testing$HEIGHT, predictions_svr)
RMSE_ann <- rmse(testing$HEIGHT, predictions_ann)
RMSE_power <- rmse(testing$HEIGHT, predictions_power)
RMSE_chapman <- rmse(testing$HEIGHT, predictions_chapman)
RMSE_logistics <- rmse(testing$HEIGHT, predictions_logistics)
RMSE_weibull <- rmse(testing$HEIGHT, predictions_weibull)
RMSE <- c(
  round(RMSE_svr, 2), 
  round(RMSE_ann, 2), 
  round(RMSE_power, 2), 
  round(RMSE_chapman, 2), 
  round(RMSE_logistics, 2), 
  round(RMSE_weibull, 2)
          )
summary_table <- data.frame(RMSE)

# Calculate RSCORE
RSQUARED_svr <- cor(testing$HEIGHT, predictions_svr)^2 # Correlation Coefficient
RSQUARED_ann <- cor(testing$HEIGHT, predictions_ann)^2 # Correlation Coefficient
RSQUARED_power <- cor(testing$HEIGHT, predictions_power)^2 # Correlation Coefficient
RSQUARED_chapman <- cor(testing$HEIGHT, predictions_chapman)^2 # Correlation Coefficient
RSQUARED_logistics <- cor(testing$HEIGHT, predictions_logistics)^2 # Correlation Coefficient
RSQUARED_weibull <- cor(testing$HEIGHT, predictions_power)^2 # Correlation Coefficient
RSQUARED <- c(
  round(RSQUARED_svr, 2), 
  round(RSQUARED_ann, 2), 
  round(RSQUARED_power, 2), 
  round(RSQUARED_chapman, 2), 
  round(RSQUARED_logistics, 2), 
  round(RSQUARED_weibull, 2)
  )
summary_table <- cbind(summary_table, RSQUARED)

# Mean Absolute Percent Error (MAPE)
MAPE_svr <- mape(testing$HEIGHT, predictions_svr)
MAPE_ann <- mape(testing$HEIGHT, predictions_ann)
MAPE_power <- mape(testing$HEIGHT, predictions_power)
MAPE_chapman <- mape(testing$HEIGHT, predictions_chapman)
MAPE_logistics <- mape(testing$HEIGHT, predictions_logistics)
MAPE_weibull <- mape(testing$HEIGHT, predictions_weibull)
MAPE <- c(
  round(MAPE_svr, 3), 
  round(MAPE_ann, 3),
  round(MAPE_power, 3),
  round(MAPE_chapman, 3),
  round(MAPE_logistics, 3),
  round(MAPE_weibull, 3)
  )
summary_table <- cbind(summary_table, MAPE)

# BIAS
bias_svr <- bias(testing$HEIGHT, predictions_svr)
bias_ann <- bias(testing$HEIGHT, predictions_ann)
bias_power <- bias(testing$HEIGHT, predictions_power)
bias_chapman <- bias(testing$HEIGHT, predictions_chapman)
bias_logistics <- bias(testing$HEIGHT, predictions_logistics)
bias_weibull <- bias(testing$HEIGHT, predictions_weibull)
BIAS <- c(
  round(bias_svr, 2), 
  round(bias_ann, 2), 
  round(bias_power, 2), 
  round(bias_chapman, 2), 
  round(bias_logistics, 2), 
  round(bias_weibull, 2)
  )
summary_table <- cbind(summary_table, BIAS)

# Mean Absolute Error (MAE)
MAE_svr <- mae(testing$HEIGHT, predictions_svr)
MAE_ann <- mae(testing$HEIGHT, predictions_ann)
MAE_power <- mae(testing$HEIGHT, predictions_power)
MAE_chapman <- mae(testing$HEIGHT, predictions_chapman)
MAE_logistics <- mae(testing$HEIGHT, predictions_logistics)
MAE_weibull <- mae(testing$HEIGHT, predictions_weibull)
MAE <- c(
  round(MAE_svr, 2), 
  round(MAE_ann, 2), 
  round(MAE_power, 2), 
  round(MAE_chapman, 2), 
  round(MAE_logistics, 2), 
  round(MAE_weibull, 2)
  )
summary_table <- cbind(summary_table, MAE)

# Calculate AIC
AIC_power <- AIC(model_power)
AIC_chapman <- AIC(model_chapman)
AIC_logistics <- AIC(model_logistics)
AIC_weibull <- AIC(model_weibull)
AIC <- c(
    "-", 
    "-", 
    round(AIC_power, 2),
    round(AIC_chapman, 2),
    round(AIC_logistics, 2),
    round(AIC_weibull, 2)
  )
summary_table <- cbind(summary_table, AIC)


# calculate BIC
BIC_power <- BIC(model_power)
BIC_chapman <- BIC(model_chapman)
BIC_logistics <- BIC(model_logistics)
BIC_weibull <- BIC(model_weibull)
BIC <- c(
  "-", 
  "-", 
  round(BIC_power, 2),
  round(BIC_chapman, 2),
  round(BIC_logistics, 2),
  round(BIC_weibull, 2)
  )
summary_table <- cbind(summary_table, BIC)

# Accuracy
accuracy_svr <- accuracy(testing$HEIGHT, prediction_frame$SVR_Predictions)
accuracy_ann <- accuracy(testing$HEIGHT, prediction_frame$ANN_Predictions)
accuracy_power <- accuracy(testing$HEIGHT, prediction_frame$Power_Predictions)
accuracy_chapman <- accuracy(testing$HEIGHT, prediction_frame$Chapman_Predictions)
accuracy_logistics <- accuracy(testing$HEIGHT, prediction_frame$Logistics_Predictions)
accuracy_weibull <- accuracy(testing$HEIGHT, prediction_frame$Weibull_Predictions)
Accuracy <- c(
  round((accuracy_svr * 100), 1), 
  round((accuracy_ann * 100), 1), 
  round((accuracy_power * 100), 1), 
  round((accuracy_chapman * 100), 1), 
  round((accuracy_logistics * 100), 1), 
  round((accuracy_weibull * 100), 1)
  )
summary_table <- cbind(summary_table, "Accuracy(%)" = Accuracy)
rownames(summary_table) <- c("SVR_Values", "ANN_Values", "Power_Values", "Chapman", "Logistics", "Weibull")


# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_svr, colour = "SVR")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_ann, colour = "ANN")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_power, colour = "Power")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_chapman, colour = "Chapman")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_logistics, colour = "Logistics")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_weibull, colour = "Weibull")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Line", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "SVR"="#2533d3", 
    "ANN"="#69ef79", 
    "Power"="#0f0f10",
    "Chapman"="#eb7de3",
    "Logistics"="#eb1919",
    "Weibull"="#c9b424"
    )) 

# Scattered Plot (Model Performance Curve) -- SVR
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_svr, colour = "SVR")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Curve (SVR)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "SVR"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- ANN
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_ann, colour = "ANN")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Curve (ANN)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "ANN"="#120c4c")) 

# Scattered Plot (Model Performance Line) -- Power
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_power, colour = "Power")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Line (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Chapman
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_chapman, colour = "Chapman")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Curve (Chapman)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "Chapman"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Logistics
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_logistics, colour = "Logistics")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Curve (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "Logistics"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Weibull
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT, colour = "Points")) +
  geom_line(aes(x = testing$DIAMETER, y = predictions_weibull, colour = "Weibull")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Curve (Weibull)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) 
  scale_color_manual(values = c("Points"="#a32e2e", "Weibull"="#120c4c")) 


# -------------------------------Export Tables---------------------------- #

# Predict Table
write.table(predict_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\predict_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(predict_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\predict_table.csv", quote = FALSE, row.names = TRUE)

# Training Table
write.table(training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\training_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\training_table.csv", quote = FALSE, row.names = TRUE)

# Testing Table
write.table(testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\testing_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\testing_table.csv", quote = FALSE, row.names = TRUE)

# Summary Table
write.table(summary_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\summary_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(summary_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\summary_table.csv", quote = FALSE, row.names = TRUE)

# Prediction Table
write.table(prediction_frame, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\prediction_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(prediction_frame, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\prediction_table.csv", quote = FALSE, row.names = TRUE)

# Frequency Table
write.table(frequency_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\frequency_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(frequency_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\frequency_table.csv", quote = FALSE, row.names = TRUE)


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
