#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ForestFit, ggplot2, lmfor, fitdistrplus, tidyverse, dplyr, psych)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//27 H-Dist")

# Import data set
dataset <-  read.csv("full-data-11.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
heights <- dataset$HEIGHT
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset

#-------------------- EXploratory Data Analysis ----------------------------#

# All Height Distribution Plot
plot(heights, pch=20)
ggplot(dataset, aes(x = HEIGHT)) +
  geom_point()

# All Diameter Histogram
hist(heights)
ggplot(data=dataset) + 
  geom_histogram(mapping=aes(x=HEIGHT ),bins=10, col="black", fill="grey")

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


#------------------------------ Summary Statistic --------------------------------#
# Summary Statistic Table
summary_statistics <- descdist(heights, discrete=FALSE, boot=500)
summary_statistics_table <- data.frame(
  "Tht" = c(round(summary_statistics$mean, 1), round(summary_statistics$sd, 1),
            round(summary_statistics$kurtosis, 1), round(summary_statistics$skewness, 1),
            round(summary_statistics$min, 1), round(summary_statistics$max, 1), length(heights))
)
rownames(summary_statistics_table) <- c("Mean", "Standard Deviation", "Kurtosis",
                                        "Skewness", "Minimum", "Maximum", "Sample Size")


#------------------------------ Species Distribution --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
species_length <- length(frequency_table$Frequency)
species_dist_table <- data.frame(
  "Species" = unique_species_dataset$Unique.Species,
  "Frequency" = frequency_table$Frequency[1:species_length - 1],
  "N/HA" = (as.numeric(frequency_table$Frequency[1:species_length - 1]) / area_of_plot_m2) * one_ha_m2,
  "Proportion(%)" = round(((as.numeric(frequency_table$Frequency[1:species_length - 1])/as.numeric(frequency_table$Frequency[species_length])) * 100), 1)
)

# Fit Distributions

# ----------------------------- Dist Gamma ----------------------------- #
# Dist Gamma Fitting
dist_gamma <- fitdist(heights, "gamma")
summary_dist_gamma <- summary(dist_gamma)
dist_gamma


# ----------------------------- Dist Weibull ----------------------------- #
# Dist Weibull Fitting
dist_weibull <- fitdist(heights, "weibull")
summary_dist_weibull <- summary(dist_weibull)
dist_weibull


# ----------------------------- Dist Lognormal ----------------------------- #
# Dist Lognormal Fitting
dist_lognormal <- fitdist(heights, "lnorm")
summary_dist_lognormal <- summary(dist_lognormal)
dist_lognormal


# ----------------------------- Dist Cauchy ----------------------------- #
# Dist Cauchy Fitting
dist_cauchy <- fitdist(heights, "cauchy")
summary_dist_cauchy <- summary(dist_cauchy)
dist_cauchy


# ----------------------------- Dist  Normal ----------------------------- #
# Dist Normal Fitting
dist_normal <- fitdist(heights, "norm")
summary_dist_normal <- summary(dist_normal)
dist_normal


# ----------------------------- Dist  Exponential ----------------------------- #
# Dist Exponential Fitting
dist_exponential <- fitdist(heights, "exp")
summary_dist_exponential <- summary(dist_exponential)
dist_exponential


# ----------------------------- Dist  Logistics ----------------------------- #
# Dist Logistics Fitting
dist_logistics <- fitdist(heights, "logis")
summary_dist_logistics <- summary(dist_logistics)
dist_logistics


# Summary Table
summary_parameter_estimates_table <- data.frame(
  "Shape" = c(round(summary_dist_weibull[["estimate"]][["shape"]], 3), round(summary_dist_gamma[["estimate"]][["shape"]], 2), "-", "-", "-", "-", "-"),
  "Scale" = c(round(summary_dist_weibull[["estimate"]][["scale"]], 3), "-", round(summary_dist_lognormal[["estimate"]][["sdlog"]], 3), round(summary_dist_normal[["estimate"]][["sd"]], 3), round(summary_dist_cauchy[["estimate"]][["scale"]], 3), "-", round(summary_dist_logistics[["estimate"]][["scale"]], 3)),
  "Location" = c("-", "-", round(summary_dist_lognormal[["estimate"]][["meanlog"]], 3), round(summary_dist_normal[["estimate"]][["mean"]], 3), round(summary_dist_cauchy[["estimate"]][["location"]], 3), "-", round(summary_dist_logistics[["estimate"]][["location"]], 3)),
  "Rate" = c("-", round(summary_dist_gamma[["estimate"]][["rate"]], 3), "-", "-", "-", round(summary_dist_exponential[["estimate"]][["rate"]], 3), "-")
)
rownames(summary_parameter_estimates_table) <- c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy", "Exponential", "Logistics")

summary_standard_error_table <- data.frame(
  "Shape" = c(round(summary_dist_weibull[["sd"]][["shape"]], 3), round(summary_dist_gamma[["sd"]][["shape"]], 2), "-", "-", "-", "-", "-"),
  "Scale" = c(round(summary_dist_weibull[["sd"]][["scale"]], 3), "-", round(summary_dist_lognormal[["sd"]][["sdlog"]], 3), round(summary_dist_normal[["sd"]][["sd"]], 3), round(summary_dist_cauchy[["sd"]][["scale"]], 3), "-", round(summary_dist_logistics[["sd"]][["scale"]], 3)),
  "Location" = c("-", "-", round(summary_dist_lognormal[["sd"]][["meanlog"]], 3), round(summary_dist_normal[["sd"]][["mean"]], 3), round(summary_dist_cauchy[["sd"]][["location"]], 3), "-", round(summary_dist_logistics[["sd"]][["location"]], 3)),
  "Rate" = c("-", round(summary_dist_gamma[["sd"]][["rate"]], 3), "-", "-", "-", round(summary_dist_exponential[["sd"]][["rate"]], 3), "-")
)
rownames(summary_standard_error_table) <- c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy", "Exponential", "Logistics")




#-------------------------------- Goodness of Fit Table -----------------------------#
# Goodness of Fit Statistics
goodness_of_fit <- gofstat(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
        fitnames=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                   "Exponential", "Logistics"))

# Goodness of Fit Table --- Kolmogorov Smirnov
kolmogorov_smirnov_values <- round(goodness_of_fit[["ks"]], 3)
kolmogorov_smirnov_table <- data.frame(
  "Statistics" = kolmogorov_smirnov_values,
  "Rank" = rank(kolmogorov_smirnov_values),
  "AIC" = round(goodness_of_fit[["aic"]], 2),
  "BIC" = round(goodness_of_fit[["bic"]], 2))

# Goodness of Fit Table --- Cramer Von Mises
cramer_von_mises_values <- round(goodness_of_fit[["cvm"]], 3)
cramer_von_mises_table <- data.frame(
  "Statistics" = cramer_von_mises_values,
  "Rank" = rank(cramer_von_mises_values),
  "AIC" = round(goodness_of_fit[["aic"]], 2),
  "BIC" = round(goodness_of_fit[["bic"]], 2))

# Goodness of Fit Table --- Anderson Darling
anderson_darling_values <- round(goodness_of_fit[["ad"]], 3)
anderson_darling_table <- data.frame(
  "Statistics" = anderson_darling_values,
  "Rank" = rank(anderson_darling_values),
  "AIC" = round(goodness_of_fit[["aic"]], 2),
  "BIC" = round(goodness_of_fit[["bic"]], 2))


#------------------ Graphical Representation of Data -------------------------#
# Individual Density Comparison
par(mfrow=c(2,2))
denscomp(dist_weibull, legendtext="Weibull",main = "",xlab = "Tht (m)")
denscomp(dist_gamma, legendtext="Gamma",main = "",xlab = "Tht (m)")
denscomp(dist_lognormal, legendtext="Lognormal",main = "",xlab = "Tht (m)")
denscomp(dist_normal, legendtext="Normal",main = "",xlab = "Tht (m)")
denscomp(dist_cauchy, legendtext="Cauchy",main = "",xlab = "Tht (m)")
denscomp(dist_exponential, legendtext="Exponential",main = "",xlab = "Tht (m)")
denscomp(dist_logistics, legendtext="Logistics",main = "",xlab = "Tht (m)")
par(mfrow=c(1,1))


# Plot Density Comparison
ggplot(df, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF")) +
  theme_classic()

denscomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "",xlab = "Tht (m)")

# Plot Cummulative Distribution
cdfcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
        legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                     "Exponential", "Logistics"), main = "", ylab = "Cumulative Distribution Function",
        xlab = "Tht (m)")

# Plot Quantiles
qqcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
       legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                    "Exponential", "Logistics"), main = "")

# Plot Probabilities
ppcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
       legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                    "Exponential", "Logistics"), main = "")

# Height Classes
class_1_5_tht <- dataset %>% filter(HEIGHT > 1 & HEIGHT <= 5)
class_5_10_tht <- dataset %>% filter(HEIGHT > 5 & HEIGHT <= 10)
class_10_15_tht <- dataset %>% filter(HEIGHT > 10 & HEIGHT <= 15)
class_15_20_tht <- dataset %>% filter(HEIGHT > 15 & HEIGHT <= 20)
class_20_25_tht <- dataset %>% filter(HEIGHT > 20 & HEIGHT <= 25)
class_25_tht <- dataset %>% filter(HEIGHT > 25)

# Summary Variables Class Table
number_of_plots <- 6
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
density <- area_of_plot_ha * number_of_plots

# THT
class_1_5_tht_density <- length(class_1_5_tht$HEIGHT)
class_5_10_tht_density <- length(class_5_10_tht$HEIGHT)
class_10_15_tht_density <- length(class_10_15_tht$HEIGHT)
class_15_20_tht_density <- length(class_15_20_tht$HEIGHT)
class_20_25_tht_density <- length(class_20_25_tht$HEIGHT)
class_25_tht_density <- length(class_25_tht$HEIGHT)

summary_variable_classes_table <- data.frame(
  "THT.density" = c(class_1_5_tht_density, class_5_10_tht_density, class_10_15_tht_density, class_15_20_tht_density, class_20_25_tht_density, class_25_tht_density),
  "THT.trees.ha" = c((class_1_5_tht_density/density), (class_5_10_tht_density/density), (class_10_15_tht_density/density), (class_15_20_tht_density/density), (class_20_25_tht_density/density), (class_25_tht_density/density))
)
summary_variable_classes_table <- rbind(summary_variable_classes_table, 
                                        c(sum(summary_variable_classes_table$THT.density), sum(summary_variable_classes_table$THT.trees.ha)
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
