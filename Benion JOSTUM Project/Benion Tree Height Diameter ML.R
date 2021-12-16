#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, caTools, e1071, hydroGOF, ggplot2, psych)


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\benion-tree-height-diameter-dataset.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))


# EXploratory Data Analysis
ggplot() +
  geom_point(aes(x = dataset$DIAMETER, y = dataset$HEIGHT), colour = "red") +
  ggtitle("Tree Height vs Diameter (Data Distribution)") +
  xlab("Diameter") +
  ylab("Height")

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

# Frequency Table


# Data Splicing
split <- sample.split(dataset, SplitRatio = 0.8)

training <- subset(dataset, split==TRUE)
summary(training)
training_description <- describe(training)

testing <- subset(dataset, split==FALSE)
summary(testing)
testing_description <- describe(testing)


# Model Fitting
model <-  svm(formula = HEIGHT ~ DIAMETER, data = training, type = "eps-regression")
summary(model)
summary_model <- summary(model)
describe(model)


# Making Predictions
predictions = predict(model, testing)

# Manual Predictions
predictor_variable <- data.frame(DIAMETER = 25.9)
y_predict <- round(predict(model, predictor_variable), digits = 1)


# Calculate RMSE 
RMSE <- rmse(predictions,testing$HEIGHT)


# Calculate RSCORE
RSQUARED = cor(testing$HEIGHT, predictions) # Correlation Coefficient
Accuracy = 100 - (mean(abs((testing$HEIGHT - predictions)/testing$HEIGHT)) * 100)


# Table of Real Height vs Predicted Height
data_frame <- data.frame("Real Height" = testing$HEIGHT, "Predicted Height" = round(predictions, digits = 1) )


# EXploratory Data Analysis (Regression Line)
# Scattered Plot (Support Vector Regression)
ggplot() +
  geom_point(aes(x = testing$DIAMETER, y = testing$HEIGHT), colour = "red") +
  geom_line(aes(x = testing$DIAMETER, y = predictions), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Support Vector Regression Line)") +
  xlab("Diameter") +
  ylab("Height")

# Regression Line (Support Vector Regression)
ggplot() +
  geom_line(aes(x = testing$DIAMETER, y = predictions), colour = "blue")+
  ggtitle("Tree Height-Diameter Relationship (Support Vector Regression Line Only)") +
  xlab("Diameter") +
  ylab("Height")

# Histogram (Predicted Height)
ggplot(data_frame, aes(x = predictions)) +
  geom_histogram(bins = 30, colour = "green") +
  labs(x = "", y = "Predicted Height")


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

#----------------------------------- END ----------------------------#