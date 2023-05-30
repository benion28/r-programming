#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics, neuralnet, lmfor, dplyr, nlme)

# Set Working Directory
setwd("C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project")



# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\benion-tree-hd-dataset-new.csv", head=TRUE, sep=",")
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
  ggtitle("Tree H-D Height vs Diameter (Data Distribution)") +
  xlab("Diameter (cm)") +
  ylab("Height (m)")

plot(
  x = dataset$DIAMETER, 
  y = dataset$HEIGHT, 
  main = "Tree H-D Height vs Diameter (Data Distribution)", 
  xlab = "Diameter (cm)", ylab = "Height (m)")

# Correlation Graph
r_dataset <- cor(dataset$DIAMETER, dataset$HEIGHT)
r_label <- interaction("R =", round(r_dataset, 3), sep=" ")

ggplot(dataset, aes(DIAMETER, HEIGHT)) + 
  geom_point(aes(DIAMETER, HEIGHT, colour = r_label)) + 
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Correlation", color = "Legend") +
  theme(legend.position = c(0.65, 0.75), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    r_label
  )) 


#---------- Unique Species Distribution Table ---------------------#

# Tectona grandis Species Table
tg_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Tectona grandis"]
tg_species <- dataset$SPECIES[dataset$SPECIES == "Tectona grandis"]
tg_diameter <- dataset$DIAMETER[dataset$SPECIES == "Tectona grandis"]
tg_height <- dataset$HEIGHT[dataset$SPECIES == "Tectona grandis"]
t_grandis_table <- data.frame("Plot No"=tg_plot_no, "Species"=tg_species, "Diameter"=tg_diameter, "Height"=tg_height)

# Senna siamea Species Table
ss_plot_no <- dataset$PLOT.NO[dataset$SPECIES == "Senna siamea"]
ss_species <- dataset$SPECIES[dataset$SPECIES == "Senna siamea"]
ss_diameter <- dataset$DIAMETER[dataset$SPECIES == "Senna siamea"]
ss_height <- dataset$HEIGHT[dataset$SPECIES == "Senna siamea"]
s_siamea_table <- data.frame("Plot No"=ss_plot_no, "Species"=ss_species, "Diameter"=ss_diameter, "Height"=ss_height)

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

# Initials Table
initials_table <- data.frame(
  "Species" = frequency_table$Species[frequency_table$Species != "Total"],
  "Initials" = c("T.g", "G.a", "S.s", "K.s", "D.o", "V.d", "A.d", "M.i", "A.a", "P.e", "P.b", "L.s", "S.l", "F.s"),
  "Frequency" = frequency_table$Frequency[frequency_table$Species != "Total"]
)

# Frequency Distribution Bar
frequency_dist_bar <- data.frame(
  "Species" = initials_table$Species,
  "Initials" = initials_table$Initials,
  "Frequency" = initials_table$Frequency
  )

#------------------------------ Number of Trees per Hectar --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 21
expansion_factor <- 10000 / (area_of_plot_m2 * number_of_plots)


#------------------------------ Species Distribution --------------------------------#
percentage <- 100
species_names <- unique_species_dataset$Unique.Species
species_length <- length(frequency_table$Frequency)
tree_per_ha <- expansion_factor * species_frequency
total_species <- sum(species_frequency)
total_trees_ha <- expansion_factor * total_species
species_proportion <- ((tree_per_ha/total_trees_ha) * percentage)
species_dist_table <- data.frame(
  "Species" = species_names,
  "Initials" = initials_table$Initials,
  "Frequency" = species_frequency,
  "Trees.HA" = round(tree_per_ha, 1),
  "Proportion(%)" = round(species_proportion, 1)
)
species_dist_table <- rbind(species_dist_table, c("Total", " ", sum(species_frequency), sum(tree_per_ha), sum(species_proportion)))


ggplot(frequency_dist_bar, aes(x = Initials, y = Frequency)) +
  geom_col(fill = "#4038bb", color = "#e92929") +
  labs(title = "Species Frequency Distribution Bar", x = "Species", y = "Frequency")


ggplot(frequency_dist_bar, aes(x = Initials, y = as.integer(Frequency))) +
  geom_col(fill = "#161516", color = "#5d199b") +
  labs(title = "Species Frequency Distribution Bar", x = "Species", y = "Frequency")


#---------------------- Loading Training and Testing Data -------------------------#
# Training or Fitting Data
training <- read.csv("C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\training-hd-dataset.csv", head=TRUE, sep=",")
summary(training)
training_description <- describe(training)

# Testing or Validation Data
testing <- read.csv("C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\testing-hd-dataset.csv", head=TRUE, sep=",")
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
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_logistics <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_gomperz <- startHDgomperz(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_naslund <- startHDnaslund(training$DIAMETER, training$HEIGHT, bh=1.3)

# Starting Values Table
starting_values_tables <- data.frame(
  "a" = c(
    round(start_values_power["a"], 2), 
    round(start_values_chapman["a"], 2), 
    round(start_values_logistics["a"], 2), 
    round(start_values_weibull["a"], 2),
    round(start_values_gomperz["a"], 2), 
    round(start_values_naslund["a"], 2)
    ),
  "b" = c(
    round(start_values_power["b"], 2), 
    round(start_values_chapman["b"], 2), 
    round(start_values_logistics["b"], 2), 
    round(start_values_weibull["b"], 2),
    round(start_values_gomperz["b"], 2),
    round(start_values_naslund["b"], 2)
  ),
  "c" = c(
    "-",
    round(start_values_chapman["c"], 2),
    round(start_values_logistics["c"], 2), 
    round(start_values_weibull["c"], 2),
    round(start_values_gomperz["c"], 2), 
    "-"
  )
)
rownames(starting_values_tables) <- c("Power", "Chapman", "Logistics", "Weibull", "Gomperz", "Naslund")

# ----------------------------- Power Model----------------------------- #
# Power Model Fitting
model_power <- nls(formula = HEIGHT ~ (1.3 + a * DIAMETER^b), 
                   data = training, start=list(
                     a=start_values_power["a"], 
                     b=start_values_power["b"]))
summary_model_power <- summary(model_power)
length_model_power <- length(summary(model_power)$coef[,1])
model_power


# Making Predictions
predictions_power_training = predict(model_power, training)
predictions_power_testing = predict(model_power, testing)


# ----------------------------- Chapman-Richards ----------------------------- #
# Chapman-Richards Model Fitting
model_chapman <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER))^c), 
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


# ----------------------------- Logistics ----------------------------- #
# Logistics Model Fitting
model_logistics <- nls(formula = HEIGHT ~ (1.3 + a / (1 + b * exp(-c * DIAMETER))),
                       data = training, start=list(a=start_values_logistics["a"],
                                                   b=start_values_logistics["b"],
                                                   c=start_values_logistics["c"]))
summary_model_logistics <- summary(model_logistics)
length_model_logistics <- length(summary(model_logistics)$coef[,1])
model_logistics

# Making Predictions
predictions_logistics_training = predict(model_logistics, training)
predictions_logistics_testing = predict(model_logistics, testing)


# ----------------------------- Weibull ----------------------------- #
# Weibull Model Fitting
model_weibull <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(a=start_values_weibull["a"], 
                                                 b=start_values_weibull["b"], 
                                                 c=start_values_weibull["c"]))
summary_model_weibull <- summary(model_weibull)
length_model_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull

# Making Predictions
predictions_weibull_training = predict(model_weibull, training)
predictions_weibull_testing = predict(model_weibull, testing)

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


# Model Comparison Frame
prediction_frame_training <- data.frame(
  "Real Values"=training$HEIGHT, 
  "Power_Predictions"=round(predictions_power_training, 1),
  "Chapman_Predictions"=round(predictions_chapman_training, 1),
  "Logistics_Predictions"=round(predictions_logistics_training, 1),
  "Weibull_Predictions"=round(predictions_weibull_training, 1),
  "Gomperz_Predictions"=round(predictions_gomperz_training, 1),
  "Naslund_Predictions"=round(predictions_naslund_training, 1)
)

prediction_frame_testing <- data.frame(
  "Real Values"=testing$HEIGHT, 
  "Power_Predictions"=round(predictions_power_testing, 1),
  "Chapman_Predictions"=round(predictions_chapman_testing, 1),
  "Logistics_Predictions"=round(predictions_logistics_testing, 1),
  "Weibull_Predictions"=round(predictions_weibull_testing, 1),
  "Gomperz_Predictions"=round(predictions_gomperz_testing, 1),
  "Naslund_Predictions"=round(predictions_naslund_testing, 1)
)

# Summary Table
summary_table_training <- data.frame()
summary_table_testing <- data.frame()


# ---------------------- Manual Predictions -------------------------------#
predictor_variable <- data.frame(DIAMETER = 20.4)

# Power
predict_power <- predict(model_power, predictor_variable)

# Chapman-Richards
predict_chapman <- predict(model_chapman, predictor_variable)

# Logistics
predict_logistics <- predict(model_logistics, predictor_variable)

# Weibull
predict_weibull <- predict(model_weibull, predictor_variable)

# Gomperz
predict_gomperz <- predict(model_gomperz, predictor_variable)

# Naslund
predict_naslund <- predict(model_naslund, predictor_variable)

# Predict Table
predict_table <- data.frame(
  "DIAMETER" = predictor_variable$DIAMETER,
  "HEIGHT" = 7.5,
  "Power_Predict" = round(predict_power, 1),
  "Chapman_Predict" = round(predict_chapman, 1),
  "Logistics_Predict" = round(predict_logistics, 1),
  "Weibull_Predict" = round(predict_weibull, 1),
  "Gomperz_Predict" = round(predict_gomperz, 1),
  "Naslund_Predict" = round(predict_naslund, 1)
)


#-------------------------------- Parameters Table -----------------------------#
# NLS Parameters
parameters_nls <- data.frame(
  "a" = c(round(summary_model_power[["parameters"]][1], 5), round(summary_model_chapman[["parameters"]][1], 5), round(summary_model_logistics[["parameters"]][1], 5), round(summary_model_weibull[["parameters"]][1], 5), round(summary_model_gomperz[["parameters"]][1], 5), round(summary_model_naslund[["parameters"]][1], 5)),
  "b" = c(round(summary_model_power[["parameters"]][2], 5), round(summary_model_chapman[["parameters"]][2], 5), round(summary_model_logistics[["parameters"]][2], 5), round(summary_model_weibull[["parameters"]][2], 5), round(summary_model_gomperz[["parameters"]][2], 5), round(summary_model_naslund[["parameters"]][2], 5)),
  "c" = c("-", round(summary_model_chapman[["parameters"]][3], 5), round(summary_model_logistics[["parameters"]][3], 5), round(summary_model_weibull[["parameters"]][3], 5), round(summary_model_gomperz[["parameters"]][3], 5), "-")
)
rownames(parameters_nls) <- c("Power", "Chapman", "Logistics", "Weibull", "Gomperz", "Naslund")



#----------------------------- Model Evaluation -----------------------------#
# Root Mean Squared Errors
# Training
RMSE_power_training <- rmse(training$HEIGHT, predictions_power_training)
RMSE_chapman_training <- rmse(training$HEIGHT, predictions_chapman_training)
RMSE_logistics_training <- rmse(training$HEIGHT, predictions_logistics_training)
RMSE_weibull_training <- rmse(training$HEIGHT, predictions_weibull_training)
RMSE_gomperz_training <- rmse(training$HEIGHT, predictions_gomperz_training)
RMSE_naslund_training <- rmse(training$HEIGHT, predictions_naslund_training)
RMSE_training <- c( 
  round(RMSE_power_training, 3), 
  round(RMSE_chapman_training, 3), 
  round(RMSE_logistics_training, 3), 
  round(RMSE_weibull_training, 3),
  round(RMSE_gomperz_training, 3),
  round(RMSE_naslund_training, 3)
)
summary_table_training <- data.frame("RMSE" = RMSE_training)

#----------------------------- Model Validation -----------------------------#
# Root Mean Squared Errors
# Testing
RMSE_power_testing <- rmse(testing$HEIGHT, predictions_power_testing)
RMSE_chapman_testing <- rmse(testing$HEIGHT, predictions_chapman_testing)
RMSE_logistics_testing <- rmse(testing$HEIGHT, predictions_logistics_testing)
RMSE_weibull_testing <- rmse(testing$HEIGHT, predictions_weibull_testing)
RMSE_gomperz_testing <- rmse(testing$HEIGHT, predictions_gomperz_testing)
RMSE_naslund_testing <- rmse(testing$HEIGHT, predictions_naslund_testing)
RMSE_testing <- c( 
  round(RMSE_power_testing, 3), 
  round(RMSE_chapman_testing, 3), 
  round(RMSE_logistics_testing, 3), 
  round(RMSE_weibull_testing, 3),
  round(RMSE_gomperz_testing, 3),
  round(RMSE_naslund_testing, 3)
  )
summary_table_testing <- data.frame("RMSE" = RMSE_testing)


#----------------------------- Model Evaluation -----------------------------#
# Calculate RSCORE
#Training
RSQUARED_power_training <- cor(training$HEIGHT, predictions_power_training)^2 # Correlation Coefficient
RSQUARED_chapman_training <- cor(training$HEIGHT, predictions_chapman_training)^2 # Correlation Coefficient
RSQUARED_logistics_training <- cor(training$HEIGHT, predictions_logistics_training)^2 # Correlation Coefficient
RSQUARED_weibull_training <- cor(training$HEIGHT, predictions_power_training)^2 # Correlation Coefficient
RSQUARED_gomperz_training <- cor(training$HEIGHT, predictions_gomperz_training)^2 # Correlation Coefficient
RSQUARED_naslund_training <- cor(training$HEIGHT, predictions_naslund_training)^2 # Correlation Coefficient
RSQUARED_training <- c(
  round(RSQUARED_power_training, 3), 
  round(RSQUARED_chapman_training, 3), 
  round(RSQUARED_logistics_training, 3), 
  round(RSQUARED_weibull_training, 3),
  round(RSQUARED_gomperz_training, 3),
  round(RSQUARED_naslund_training, 3)
  )
summary_table_training <- cbind(summary_table_training, "RSQUARED" = RSQUARED_training)

#----------------------------- Model Validation -----------------------------#
# Calculate RSCORE
# Testing
RSQUARED_power_testing <- cor(testing$HEIGHT, predictions_power_testing)^2 # Correlation Coefficient
RSQUARED_chapman_testing <- cor(testing$HEIGHT, predictions_chapman_testing)^2 # Correlation Coefficient
RSQUARED_logistics_testing <- cor(testing$HEIGHT, predictions_logistics_testing)^2 # Correlation Coefficient
RSQUARED_weibull_testing <- cor(testing$HEIGHT, predictions_power_testing)^2 # Correlation Coefficient
RSQUARED_gomperz_testing <- cor(testing$HEIGHT, predictions_gomperz_testing)^2 # Correlation Coefficient
RSQUARED_naslund_testing <- cor(testing$HEIGHT, predictions_naslund_testing)^2 # Correlation Coefficient
RSQUARED_testing <- c(
  round(RSQUARED_power_testing, 3), 
  round(RSQUARED_chapman_testing, 3), 
  round(RSQUARED_logistics_testing, 3), 
  round(RSQUARED_weibull_testing, 3),
  round(RSQUARED_gomperz_testing, 3),
  round(RSQUARED_naslund_testing, 3)
)
summary_table_testing <- cbind(summary_table_testing, "RSQUARED" = RSQUARED_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Training
MAPE_svr_training <- mape(training$HEIGHT, predictions_svr_training)
MAPE_ann_training <- mape(training$HEIGHT, predictions_ann_training)
MAPE_power_training <- mape(training$HEIGHT, predictions_power_training)
MAPE_chapman_training <- mape(training$HEIGHT, predictions_chapman_training)
MAPE_logistics_training <- mape(training$HEIGHT, predictions_logistics_training)
MAPE_weibull_training <- mape(training$HEIGHT, predictions_weibull_training)
MAPE_gomperz_training <- mape(training$HEIGHT, predictions_gomperz_training)
MAPE_naslund_training <- mape(training$HEIGHT, predictions_naslund_training)
MAPE_training <- c(
  round(MAPE_power_training, 3),
  round(MAPE_chapman_training, 3),
  round(MAPE_logistics_training, 3),
  round(MAPE_weibull_training, 3),
  round(MAPE_gomperz_training, 3),
  round(MAPE_naslund_training, 3)
  )
summary_table_training <- cbind(summary_table_training, "MAPE" = MAPE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Percent Error (MAPE)
# Testing
MAPE_power_testing <- mape(testing$HEIGHT, predictions_power_testing)
MAPE_chapman_testing <- mape(testing$HEIGHT, predictions_chapman_testing)
MAPE_logistics_testing <- mape(testing$HEIGHT, predictions_logistics_testing)
MAPE_weibull_testing <- mape(testing$HEIGHT, predictions_weibull_testing)
MAPE_gomperz_testing <- mape(testing$HEIGHT, predictions_gomperz_testing)
MAPE_naslund_testing <- mape(testing$HEIGHT, predictions_naslund_testing)
MAPE_testing <- c(
  round(MAPE_power_testing, 3),
  round(MAPE_chapman_testing, 3),
  round(MAPE_logistics_testing, 3),
  round(MAPE_weibull_testing, 3),
  round(MAPE_gomperz_testing, 3),
  round(MAPE_naslund_testing, 3)
  )
summary_table_testing <- cbind(summary_table_testing, "MAPE" = MAPE_testing)

#----------------------------- Model Evaluation -----------------------------#
# BIAS
# Training
bias_power_training <- bias(training$HEIGHT, predictions_power_training)
bias_chapman_training <- bias(training$HEIGHT, predictions_chapman_training)
bias_logistics_training <- bias(training$HEIGHT, predictions_logistics_training)
bias_weibull_training <- bias(training$HEIGHT, predictions_weibull_training)
bias_gomperz_training <- bias(training$HEIGHT, predictions_gomperz_training)
bias_naslund_training <- bias(training$HEIGHT, predictions_naslund_training)
BIAS_training <- c(
  round(bias_power_training, 3), 
  round(bias_chapman_training, 3), 
  round(bias_logistics_training, 3), 
  round(bias_weibull_training, 3),
  round(bias_gomperz_training, 3),
  round(bias_naslund_training, 3)
  )
summary_table_training <- cbind(summary_table_training, "BIAS" = BIAS_training)

#----------------------------- Model Validation -----------------------------#
# BIAS
# Testing
bias_power_testing <- bias(testing$HEIGHT, predictions_power_testing)
bias_chapman_testing <- bias(testing$HEIGHT, predictions_chapman_testing)
bias_logistics_testing <- bias(testing$HEIGHT, predictions_logistics_testing)
bias_weibull_testing <- bias(testing$HEIGHT, predictions_weibull_testing)
bias_gomperz_testing <- bias(testing$HEIGHT, predictions_gomperz_testing)
bias_naslund_testing <- bias(testing$HEIGHT, predictions_naslund_testing)
BIAS_testing <- c( 
  round(bias_power_testing, 3), 
  round(bias_chapman_testing, 3), 
  round(bias_logistics_testing, 3), 
  round(bias_weibull_testing, 3),
  round(bias_gomperz_testing, 3), 
  round(bias_naslund_testing, 3) 
  )
summary_table_testing <- cbind(summary_table_testing, "BIAS" = BIAS_testing)

#----------------------------- Model Evaluation -----------------------------#
# Mean Absolute Error (MAE)
# Training
MAE_power_training <- mae(training$HEIGHT, predictions_power_training)
MAE_chapman_training <- mae(training$HEIGHT, predictions_chapman_training)
MAE_logistics_training <- mae(training$HEIGHT, predictions_logistics_training)
MAE_weibull_training <- mae(training$HEIGHT, predictions_weibull_training)
MAE_gomperz_training <- mae(training$HEIGHT, predictions_gomperz_training)
MAE_naslund_training <- mae(training$HEIGHT, predictions_naslund_training)
MAE_training <- c( 
  round(MAE_power_training, 3), 
  round(MAE_chapman_training, 3), 
  round(MAE_logistics_training, 3), 
  round(MAE_weibull_training, 3),
  round(MAE_gomperz_training, 3), 
  round(MAE_naslund_training, 3)
  )
summary_table_training <- cbind(summary_table_training, "MAE" = MAE_training)

#----------------------------- Model Validation -----------------------------#
# Mean Absolute Error (MAE)
# Testing
MAE_power_testing <- mae(testing$HEIGHT, predictions_power_testing)
MAE_chapman_testing <- mae(testing$HEIGHT, predictions_chapman_testing)
MAE_logistics_testing <- mae(testing$HEIGHT, predictions_logistics_testing)
MAE_weibull_testing <- mae(testing$HEIGHT, predictions_weibull_testing)
MAE_gomperz_testing <- mae(testing$HEIGHT, predictions_gomperz_testing)
MAE_naslund_testing <- mae(testing$HEIGHT, predictions_naslund_testing)
MAE_testing <- c(
  round(MAE_power_testing, 3), 
  round(MAE_chapman_testing, 3), 
  round(MAE_logistics_testing, 3), 
  round(MAE_weibull_testing, 3),
  round(MAE_gomperz_testing, 3), 
  round(MAE_naslund_testing, 3)
  )
summary_table_testing <- cbind(summary_table_testing, "MAE" = MAE_testing)

#----------------------------- Model Evaluation -----------------------------#
# Calculate AIC
AIC_power <- AIC(model_power)
AIC_chapman <- AIC(model_chapman)
AIC_logistics <- AIC(model_logistics)
AIC_weibull <- AIC(model_weibull)
AIC_gomperz <- AIC(model_gomperz)
AIC_naslund <- AIC(model_naslund)
AIC <- c(
    round(AIC_power, 3),
    round(AIC_chapman, 3),
    round(AIC_logistics, 3),
    round(AIC_weibull, 3),
    round(AIC_gomperz, 3),
    round(AIC_naslund, 3)
  )
summary_table_training <- cbind(summary_table_training, AIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, AIC)


# calculate BIC
BIC_power <- BIC(model_power)
BIC_chapman <- BIC(model_chapman)
BIC_logistics <- BIC(model_logistics)
BIC_weibull <- BIC(model_weibull)
BIC_gomperz <- BIC(model_gomperz)
BIC_naslund <- BIC(model_naslund)
BIC <- c(
  round(BIC_power, 2),
  round(BIC_chapman, 2),
  round(BIC_logistics, 2),
  round(BIC_weibull, 2),
  round(BIC_gomperz, 2),
  round(BIC_naslund, 2)
  )
summary_table_training <- cbind(summary_table_training, BIC)

#----------------------------- Model Validation -----------------------------#
summary_table_testing <- cbind(summary_table_testing, BIC)

#----------------------------- Model Evaluation -----------------------------#
# Accuracy
# Training
accuracy_power_training <- accuracy(training$HEIGHT, prediction_frame_training$Power_Predictions)
accuracy_chapman_training <- accuracy(training$HEIGHT, prediction_frame_training$Chapman_Predictions)
accuracy_logistics_training <- accuracy(training$HEIGHT, prediction_frame_training$Logistics_Predictions)
accuracy_weibull_training <- accuracy(training$HEIGHT, prediction_frame_training$Weibull_Predictions)
accuracy_gomperz_training <- accuracy(training$HEIGHT, prediction_frame_training$Gomperz_Predictions)
accuracy_naslund_training <- accuracy(training$HEIGHT, prediction_frame_training$Naslund_Predictions)
accuracy_training <- c( 
  round((accuracy_power_training * 100), 2), 
  round((accuracy_chapman_training * 100), 2), 
  round((accuracy_logistics_training * 100), 2), 
  round((accuracy_weibull_training * 100), 2),
  round((accuracy_gomperz_training * 100), 2), 
  round((accuracy_naslund_training * 100), 2)
  )
summary_table_training <- cbind(summary_table_training, "Accuracy(%)" = accuracy_training)

#----------------------------- Model Validation -----------------------------#
# Accuracy
# Testing
accuracy_power_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Power_Predictions)
accuracy_chapman_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Chapman_Predictions)
accuracy_logistics_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Logistics_Predictions)
accuracy_weibull_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Weibull_Predictions)
accuracy_gomperz_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Gomperz_Predictions)
accuracy_naslund_testing <- accuracy(testing$HEIGHT, prediction_frame_testing$Naslund_Predictions)
accuracy_testing <- c( 
  round((accuracy_power_testing * 100), 2), 
  round((accuracy_chapman_testing * 100), 2), 
  round((accuracy_logistics_testing * 100), 2), 
  round((accuracy_weibull_testing * 100), 2),
  round((accuracy_gomperz_testing * 100), 2),
  round((accuracy_naslund_testing * 100), 2)
  )
summary_table_testing <- cbind(summary_table_testing, "Accuracy(%)" = accuracy_testing)

rownames(summary_table_training) <- c("Power", "Chapman", "Logistics", "Weibull", "Gomperz", "Naslund")
rownames(summary_table_testing) <- c("Power", "Chapman", "Logistics", "Weibull", "Gomperz", "Naslund")


# Scattered Plot (Model Performance Line/Curve)
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_chapman_training, colour = "Chapman")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_logistics_training, colour = "Logistics")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_weibull_training, colour = "Weibull")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_gomperz_training, colour = "Gomperz")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_naslund_training, colour = "Naslund")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Lines", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Points"="#9b2339", 
    "Power"="#0f0f10",
    "Chapman"="#eb7de3",
    "Logistics"="#eb1919",
    "Weibull"="#c9b424",
    "Gomperz"="#2533d3", 
    "Naslund"="#69ef79"
    )) 

# Scattered Plot (Model Performance Line) -- Power
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_power_training, colour = "Power")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Line (Power)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Power"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Chapman
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_chapman_training, colour = "Chapman")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Chapman)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Chapman"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Logistics
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_logistics_training, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Logistics"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Weibull
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_weibull_training, colour = "Weibull")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Weibull)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Weibull"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Gomperz
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_gomperz_training, colour = "Gomperz")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Gomperz)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Gomperz"="#120c4c")) 

# Scattered Plot (Model Performance Curve) -- Naslund
ggplot() +
  geom_point(aes(x = training$DIAMETER, y = training$HEIGHT, colour = "Points")) +
  geom_line(aes(x = training$DIAMETER, y = predictions_naslund_training, colour = "Naslund")) +
  labs(x = "Diameter (cm)", y = "Height (m)", title = "Tree Height - Diameter Model Curve (Naslund)", color = "Legend") +
  theme(legend.position = c(0.90, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Naslund"="#120c4c")) 

# Model Residuals Table
residuals_table <- data.frame(
  "Power" = residuals(model_power),
  "Chapman" = residuals(model_chapman),
  "Logistics" = residuals(model_logistics),
  "Weibull" = residuals(model_weibull),
  "Gomperz" = residuals(model_gomperz),
  "Naslund" = residuals(model_naslund)
)


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

# Scattered Plot (Fitted VS Residuals Values) -- Gomperz
ggplot() +
  geom_point(aes(x = fitted(model_gomperz), y = residuals(model_gomperz), colour = "Points")) +
  geom_abline(aes(slope=0, intercept=0, colour = "Gomperz")) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Tree Height - Diameter -- Fitted vs Residuals (Gomperz)", color = "Legend") +
  theme(legend.position = c(0.75, 0.70), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="#a32e2e", "Gomperz"="#4954ed")) 

plot(model_gomperz, col = "black",
     main = "Tree H-D Fitted VS Residuals Values -- Gomperz",
     xlab = "Fitted Values",
     ylab = "Standardized Residuals") 


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Logistics
ggplot(residuals_table, aes(sample = Logistics)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Logistics)")

qqnorm(residuals(model_logistics, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Logistics", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_logistics,type = "pearson"))


# Scattered Plot (Standardized Normal Quantile vs Standardized Residuals) -- Gomperz
ggplot(residuals_table, aes(sample = Gomperz)) +
  geom_qq(color = "#a32e2e") +
  geom_qq_line(color = "#4954ed") +
  labs(x = "Quantiles of Standardized Normal", y = "Standardized Residuals", title = "Tree H-D Standardized Normal Quantile vs Residuals (Gomperz)")

qqnorm(residuals(model_gomperz, type = "pearson"), 
       main = "Tree H-D Standardized Normal Quantile vs Standardized Residuals -- Gomperz", 
       xlab = "Standardized Normal Quantile", ylab = "Standardized Residuals")
qqline(residuals(model_logistics,type = "pearson"))



#------------------------------ Variable Classes --------------------------------#
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


# -------------------------------Export Tables---------------------------- #

# Predict Table
write.table(predict_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\predict_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(predict_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\predict_table.csv", quote = FALSE, row.names = TRUE)

# Training Table
write.table(training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\training_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\training_table.csv", quote = FALSE, row.names = TRUE)

# Testing Table
write.table(testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\testing_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\testing_table.csv", quote = FALSE, row.names = TRUE)

# Summary Table Training
write.table(summary_table_training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_table_training.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(summary_table_training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_table_training.csv", quote = FALSE, row.names = TRUE)

# Summary Table Testing
write.table(summary_table_testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_table_testing.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(summary_table_testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_table_testing.csv", quote = FALSE, row.names = TRUE)

# Prediction Table Training
write.table(prediction_frame_training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\prediction_table_training.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(prediction_frame_training, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\prediction_table_training.csv", quote = FALSE, row.names = TRUE)

# Prediction Table Testing
write.table(prediction_frame_testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\prediction_table_testing.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(prediction_frame_testing, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\prediction_table_testing.csv", quote = FALSE, row.names = TRUE)

# Frequency Table
write.table(frequency_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\frequency_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(frequency_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\frequency_table.csv", quote = FALSE, row.names = TRUE)

# Model Residual Table
write.table(residuals_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables\\residuals_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(residuals_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\residuals_table.csv", quote = FALSE, row.names = TRUE)

# Descriptive Statistcic Table
write.table(descriptive_statistcic_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\descriptive_statistcic_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(descriptive_statistcic_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\descriptive_statistcic_table.csv", quote = FALSE, row.names = TRUE)

# Starting Values Table
write.table(starting_values_tables, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\starting_values_tables.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(starting_values_tables, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\starting_values_tables.csv", quote = FALSE, row.names = TRUE)

# Summary Variable Classes Table
write.table(summary_variable_classes_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_variable_classes_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(summary_variable_classes_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\summary_variable_classes_table.csv", quote = FALSE, row.names = TRUE)

# Species Dist Table
write.table(species_dist_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\species_dist_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(species_dist_table, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\species_dist_table.csv", quote = FALSE, row.names = TRUE)

# Parameters NL Table
write.table(parameters_nls, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\parameters_nls.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(parameters_nls, file = "C:\\Benion\\Benion Programmings\\R\\Benion JOSTUM Project\\project-exported-tables-nl\\parameters_nls.csv", quote = FALSE, row.names = TRUE)


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Remove list from Global Environment
rm(list=ls())

# Clear console
cat("\014")  # ctrl+L
