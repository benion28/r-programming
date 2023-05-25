#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ForestFit, ggplot2, lmfor, fitdistrplus, tidyverse, dplyr, psych, gridExtra)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//9 D-Dist")

# Import data set
dataset <-  read.csv("full-data-4.csv", head=TRUE, sep=",")
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

species_length <- length(frequency_table$Frequency)
species_dist_table <- data.frame(
  "Species" = unique_species_dataset$Unique.Species,
  "Frequency" = frequency_table$Frequency[1:species_length - 1],
  "N/HA" = (as.numeric(frequency_table$Frequency[1:species_length - 1]) / (50 * 50)) * 100000,
  "Proportion(%)" = round(((as.numeric(frequency_table$Frequency[1:species_length - 1])/as.numeric(frequency_table$Frequency[species_length])) * 100), 1)
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

#---------------------    GGPlot Density Comparison  -------------------------------#
# GGPlot Density Comparison
dencomp_table <- data.frame(
  "x" = diameters,
  "weibull" = dweibull(diameters, shape = as.numeric(summary_parameter_estimates_table$Shape[1]), scale = as.numeric(summary_parameter_estimates_table$Scale[1])),
  "gamma" = dgamma(diameters, shape = as.numeric(summary_parameter_estimates_table$Shape[2]), rate = as.numeric(summary_parameter_estimates_table$Rate[2])),
  "lognormal" = dlnorm(diameters, meanlog = as.numeric(summary_parameter_estimates_table$Location[3]), sdlog = as.numeric(summary_parameter_estimates_table$Scale[3])),
  "normal" = dnorm(diameters, mean = as.numeric(summary_parameter_estimates_table$Location[4]), sd = as.numeric(summary_parameter_estimates_table$Scale[4])),
  "cauchy" = dcauchy(diameters, location = as.numeric(summary_parameter_estimates_table$Location[5]), scale = as.numeric(summary_parameter_estimates_table$Scale[5])),
  "exponential" = dexp(diameters, rate = as.numeric(summary_parameter_estimates_table$Rate[6]), log = FALSE),
  "logistics" = dlogis(diameters, location = as.numeric(summary_parameter_estimates_table$Location[7]), scale = as.numeric(summary_parameter_estimates_table$Scale[7]))
)

# convert data to long format for plotting
dencomp_table_long <- dencomp_table %>% pivot_longer(-x, names_to = "model", values_to = "density")


# ggplot density comparison for combined model
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$weibull, colour = "Weibull")) +
  geom_line(aes(x = diameters, y = dencomp_table$gamma, colour = "Gamma")) +
  geom_line(aes(x = diameters, y = dencomp_table$lognormal, colour = "Lognormal")) +
  geom_line(aes(x = diameters, y = dencomp_table$normal, colour = "Normal")) +
  geom_line(aes(x = diameters, y = dencomp_table$cauchy, colour = "Cauchy")) +
  geom_line(aes(x = diameters, y = dencomp_table$exponential, colour = "Exponential")) +
  geom_line(aes(x = diameters, y = dencomp_table$logistics, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Models", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Exponential"="#f29012", 
    "Gamma"="#0f0f10",
    "Lognormal"="#eb7de3",
    "Logistics"="#eb1919",
    "Cauchy"="#f0d70c",
    "Normal"="#2533d3", 
    "Weibull"="#69ef79"
  ))

# ggplot density comparison for each model
plot_1 <- ggplot(dencomp_table_long, aes(x = x, y = density)) +
  geom_line() +
  facet_wrap(~ model, nrow = 2, scales = "free") +
  theme_bw()
# arrange plots in a grid
grid.arrange(plot_1, ncol = 1)

# GGPlot Density Comparison of Model ---  (Weibull)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$weibull, colour = "Weibull")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Weibull)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Weibull" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Gamma)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$gamma, colour = "Gamma")) +
  labs(x = "Diameter (cm)", y = "Density (m)", title = "Density Comparison of Model (Gamma)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Gamma" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Lognormal)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$lognormal, colour = "Lognormal")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Lognormal)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Lognormal" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Normal)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$normal, colour = "Normal")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Normal)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Normal" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Cauchy)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$cauchy, colour = "Cauchy")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Cauchy)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Cauchy" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Exponential)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$exponential, colour = "Exponential")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Exponential)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Exponential" = "#7b0f0f")) 

# GGPlot Density Comparison of Model ---  (Logistics)
ggplot() +
  geom_line(aes(x = diameters, y = dencomp_table$logistics, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Density", title = "Density Comparison of Model (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.30), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Logistics" = "#7b0f0f")) 


#---------------------    GGPlot Cumulative Distribution Comparison  -------------------------------#
# GGPlot Density Comparison
cdfcomp_table <- data.frame(
  "x" = diameters,
  "weibull" = pweibull(diameters, shape = as.numeric(summary_parameter_estimates_table$Shape[1]), scale = as.numeric(summary_parameter_estimates_table$Scale[1])),
  "gamma" = pgamma(diameters, shape = as.numeric(summary_parameter_estimates_table$Shape[2]), rate = as.numeric(summary_parameter_estimates_table$Rate[2])),
  "lognormal" = plnorm(diameters, meanlog = as.numeric(summary_parameter_estimates_table$Location[3]), sdlog = as.numeric(summary_parameter_estimates_table$Scale[3])),
  "normal" = pnorm(diameters, mean = as.numeric(summary_parameter_estimates_table$Location[4]), sd = as.numeric(summary_parameter_estimates_table$Scale[4])),
  "cauchy" = pcauchy(diameters, location = as.numeric(summary_parameter_estimates_table$Location[5]), scale = as.numeric(summary_parameter_estimates_table$Scale[5])),
  "exponential" = pexp(diameters, rate = as.numeric(summary_parameter_estimates_table$Rate[6]), log = FALSE),
  "logistics" = plogis(diameters, location = as.numeric(summary_parameter_estimates_table$Location[7]), scale = as.numeric(summary_parameter_estimates_table$Scale[7]))
)

# ggplot density comparison for combined model
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$weibull, colour = "Weibull")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$gamma, colour = "Gamma")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$lognormal, colour = "Lognormal")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$normal, colour = "Normal")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$cauchy, colour = "Cauchy")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$exponential, colour = "Exponential")) +
  geom_line(aes(x = diameters, y = cdfcomp_table$logistics, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cumulative Distribution of Models", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Exponential"="#f29012", 
    "Gamma"="#0f0f10",
    "Lognormal"="#eb7de3",
    "Logistics"="#eb1919",
    "Cauchy"="#f0d70c",
    "Normal"="#2533d3", 
    "Weibull"="#69ef79"
  ))

# convert data to long format for plotting
cdfcomp_table_long <- cdfcomp_table %>% pivot_longer(-x, names_to = "model", values_to = "probability")

# ggplot CDF comparison for each model
plot_2 <- ggplot(cdfcomp_table_long, aes(x = x, y = probability)) +
  geom_line() +
  facet_wrap(~ model, nrow = 2, scales = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()
# arrange plots in a grid
grid.arrange(plot_2, ncol = 1)

# GGPlot Cummulative Distribution of Model ---  (Gamma)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$gamma, colour = "Gamma")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Gamma)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Gamma" = "#7b0f0f")) 

# GGPlot Cummulative Distribution of Model ---  (Lognormal)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$lognormal, colour = "Lognormal")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Lognormal)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Lognormal" = "#7b0f0f")) 

# GGPlot Cummulative Distribution of Model ---  (Normal)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$normal, colour = "Normal")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Normal)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Normal" = "#7b0f0f")) 

# GGPlot Cummulative Distribution of Model ---  (Cauchy)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$cauchy, colour = "Cauchy")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Cauchy)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Cauchy" = "#7b0f0f")) 

# GGPlot Cummulative Distribution of Model ---  (Exponential)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$exponential, colour = "Exponential")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Exponential)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Exponential" = "#7b0f0f")) 

# GGPlot Cummulative Distribution of Model ---  (Logistics)
ggplot() +
  geom_line(aes(x = diameters, y = cdfcomp_table$logistics, colour = "Logistics")) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cummulative Distribution of Model (Logistics)", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Logistics" = "#7b0f0f")) 

#---------------------    GGPlot Quantile Quantile  -------------------------------#
# GGPlot Quantile Quantile Plot
qqcomp_table <- data.frame(
  "x" = diameters,
  "weibull" = qweibull(seq(0.01, 0.99, length.out = 100), shape = as.numeric(summary_parameter_estimates_table$Shape[1]), scale = as.numeric(summary_parameter_estimates_table$Scale[1])),
  "gamma" = qgamma(seq(0.01, 0.99, length.out = 100), shape = as.numeric(summary_parameter_estimates_table$Shape[2]), rate = as.numeric(summary_parameter_estimates_table$Rate[2])),
  "lognormal" = qlnorm(seq(0.01, 0.99, length.out = 100), meanlog = as.numeric(summary_parameter_estimates_table$Location[3]), sdlog = as.numeric(summary_parameter_estimates_table$Scale[3])),
  "normal" = qnorm(seq(0.01, 0.99, length.out = 100), mean = as.numeric(summary_parameter_estimates_table$Location[4]), sd = as.numeric(summary_parameter_estimates_table$Scale[4])),
  "cauchy" = qcauchy(seq(0.01, 0.99, length.out = 100), location = as.numeric(summary_parameter_estimates_table$Location[5]), scale = as.numeric(summary_parameter_estimates_table$Scale[5])),
  "exponential" = qexp(seq(0.01, 0.99, length.out = 100), rate = as.numeric(summary_parameter_estimates_table$Rate[6]), log = FALSE),
  "logistics" = qlogis(seq(0.01, 0.99, length.out = 100), location = as.numeric(summary_parameter_estimates_table$Location[7]), scale = as.numeric(summary_parameter_estimates_table$Scale[7]))
)

# convert data to long format for plotting
qqcomp_table_long <- qqcomp_table %>% pivot_longer(-x, names_to = "model", values_to = "quantile")

# plot QQ comparison for each model
plot_3 <- ggplot(qqcomp_table_long, aes(sample = x)) +
  stat_qq(aes(sample = quantile), distribution = qweibull, geom = "point") +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ model, nrow = 2, scales = "free") +
  theme_bw()
# arrange plots in a grid
grid.arrange(plot_3, ncol = 1)

# ggplot quantile quantile for combined model
ggplot() +
  stat_qq(aes(sample = qqcomp_table$weibull, colour = "Weibull"), distribution = qweibull, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$gamma, colour = "Gamma"), distribution = qgamma, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$lognormal, colour = "Lognormal"), distribution = qlnorm, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$normal, colour = "Normal"), distribution = qnorm, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$cauchy, colour = "Cauchy"), distribution = qcauchy, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$exponential, colour = "Exponential"), distribution = qexp, geom = "Point") +
  stat_qq(aes(sample = qqcomp_table$logistics, colour = "Logistics"), distribution = qlogis, geom = "Point") +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Diameter (cm)", y = "Probability", title = "Cumulative Distribution of Models", color = "Legend") +
  theme(legend.position = c(0.90, 0.02), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c(
    "Point"="#9f0841",
    "Exponential"="#f29012", 
    "Gamma"="#0f0f10",
    "Lognormal"="#eb7de3",
    "Logistics"="#eb1919",
    "Cauchy"="#f0d70c",
    "Normal"="#2533d3", 
    "Weibull"="#69ef79"
  ))

#---------------------    GGPlot Quantile Quantile  -------------------------------#


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
