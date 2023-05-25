#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ForestFit, ggplot2, lmfor, fitdistrplus, tidyverse, dplyr, psych)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//3 D-Dist")

# Clear List
rm(list=ls())

# Import data set
dataset <-  read.csv("full-data-2.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
diameters <- dataset$DIAMETER
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset

#-------------------- EXploratory Data Analysis ----------------------------#

# All Diameter Distribution Plot
plot(diameters, pch=20)
ggplot(dataset, aes(x = DIAMETER)) +
  geom_point()

# All Diameter Histogram
hist(diameters)
ggplot(data=dataset) + 
  geom_histogram(mapping=aes(x=DIAMETER ),bins=10, col="black", fill="grey")

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
summary_statistics <- descdist(diameters, discrete=FALSE, boot=500)
summary_statistics_table <- data.frame(
  "DBH" = c(round(summary_statistics$mean, 1), round(summary_statistics$sd, 1),
            round(summary_statistics$kurtosis, 1), round(summary_statistics$skewness, 1),
            round(summary_statistics$min, 1), round(summary_statistics$max, 1), length(diameters))
)
rownames(summary_statistics_table) <- c("Mean", "Standard Deviation", "Kurtosis",
                                        "Skewness", "Minimum", "Maximum", "Sample Size")


#------------------------------ Species Distribution --------------------------------#

species_dist_table <- data.frame(
  "Species" = unique_species_dataset$Unique.Species,
  "Frequency" = frequency_table$Frequency[1:30],
  "N/HA" = (as.numeric(frequency_table$Frequency[1:30]) / (50 * 50)) * 100000,
  "Proportion(%)" = round(((as.numeric(frequency_table$Frequency[1:30])/as.numeric(frequency_table$Frequency[31])) * 100), 1)
)

# Fit Distributions

# ----------------------------- Dist Gamma ----------------------------- #
# Dist Gamma Fitting
dist_gamma <- fitdist(diameters, "gamma")
summary_dist_gamma <- summary(dist_gamma)
dist_gamma


# ----------------------------- Dist Weibull ----------------------------- #
# Dist Weibull Fitting
dist_weibull <- fitdist(diameters, "weibull")
summary_dist_weibull <- summary(dist_weibull)
dist_weibull


# ----------------------------- Dist Lognormal ----------------------------- #
# Dist Lognormal Fitting
dist_lognormal <- fitdist(diameters, "lnorm")
summary_dist_lognormal <- summary(dist_lognormal)
dist_lognormal


# ----------------------------- Dist Cauchy ----------------------------- #
# Dist Cauchy Fitting
dist_cauchy <- fitdist(diameters, "cauchy")
summary_dist_cauchy <- summary(dist_cauchy)
dist_cauchy


# ----------------------------- Dist  Normal ----------------------------- #
# Dist Normal Fitting
dist_normal <- fitdist(diameters, "norm")
summary_dist_normal <- summary(dist_normal)
dist_normal


# ----------------------------- Dist  Exponential ----------------------------- #
# Dist Exponential Fitting
dist_exponential <- fitdist(diameters, "exp")
summary_dist_exponential <- summary(dist_exponential)
dist_exponential


# ----------------------------- Dist  Logistics ----------------------------- #
# Dist Logistics Fitting
dist_logistics <- fitdist(diameters, "logis")
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
denscomp(dist_weibull, legendtext="Weibull",main = "",xlab = "Dbh (cm)")
denscomp(dist_gamma, legendtext="Gamma",main = "",xlab = "Dbh (cm)")
denscomp(dist_lognormal, legendtext="Lognormal",main = "",xlab = "Dbh (cm)")
denscomp(dist_normal, legendtext="Normal",main = "",xlab = "Dbh (cm)")
denscomp(dist_cauchy, legendtext="Cauchy",main = "",xlab = "Dbh (cm)")
denscomp(dist_exponential, legendtext="Exponential",main = "",xlab = "Dbh (cm)")
denscomp(dist_logistics, legendtext="Logistics",main = "",xlab = "Dbh (cm)")
par(mfrow=c(1,1))


# Plot Density Comparison
ggplot(df, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF")) +
  theme_classic()

denscomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
         legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                      "Exponential", "Logistics"), main = "",xlab = "Dbh (cm)")

# Plot Cummulative Distribution
cdfcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
        legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                     "Exponential", "Logistics"), main = "", ylab = "Cumulative Distribution Function",
        xlab = "Dbh (cm)")

# Plot Quantiles
qqcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
       legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                    "Exponential", "Logistics"), main = "")

# Plot Probabilities
ppcomp(list(dist_weibull, dist_gamma, dist_lognormal, dist_normal, dist_cauchy, dist_exponential, dist_logistics), 
       legendtext=c("Weibull", "Gamma", "Lognormal", "Normal", "Cauchy",
                    "Exponential", "Logistics"), main = "")

# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
