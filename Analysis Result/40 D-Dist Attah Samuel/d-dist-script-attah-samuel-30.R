#==============================================================#
#   DIAMETER DISTRIBUTION ANALYSIS - Refactored R Script
#==============================================================#

#------------------------- Load Libraries ----------------------#
pacman::p_load(
  pacman, ForestFit, ggplot2, lmfor, fitdistrplus,
  tidyverse, dplyr, psych
)

#--------------------- Set Working Directory -------------------#
setwd("G://Benion//Benion Programmings//R//Analysis Result//40 D-Dist Attah Samuel")

#-------------------------- Import Data ------------------------#
dataset <- read.csv("full-data-18.csv", header = TRUE, sep = ",")
head(dataset, 10)
summary(dataset)

# Data Description
data_description <- describe(dataset)
diameters <- dataset$DIAMETER
# Remove missing or non-finite values
diameters <- diameters[is.finite(diameters)]
unique_species <- unique(dataset$SPECIES)
unique_species_dataset <- data.frame("Unique Species" = unique_species)

#------------------- Exploratory Data Analysis -----------------#
# Diameter Distribution Plot
plot(diameters, pch = 20, main = "Diameter Distribution", xlab = "Index", ylab = "Diameter (cm)")
ggplot(dataset, aes(x = seq_along(DIAMETER), y = DIAMETER)) +
  geom_point(color = "steelblue") +
  labs(title = "Diameter Distribution", x = "Index", y = "Diameter (cm)") +
  theme_minimal()

# Histogram
hist(diameters, main = "Histogram of Diameters", xlab = "Diameter (cm)", col = "grey", border = "black")
ggplot(dataset, aes(x = DIAMETER)) +
  geom_histogram(bins = 10, color = "black", fill = "grey") +
  theme_minimal() + labs(title = "Diameter Histogram")

#-------------------- Frequency Distribution -------------------#
frequency_table <- dataset %>%
  group_by(SPECIES) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(desc(Frequency))

# Add Total Row
frequency_table <- rbind(frequency_table, data.frame(SPECIES = "Total", Frequency = sum(frequency_table$Frequency)))

#----------------------- Summary Statistics --------------------#
summary_stats <- descdist(diameters, discrete = FALSE, boot = 500)
summary_statistics_table <- data.frame(
  DBH = c(
    round(summary_stats$mean, 1),
    round(summary_stats$sd, 1),
    round(summary_stats$kurtosis, 1),
    round(summary_stats$skewness, 1),
    round(summary_stats$min, 1),
    round(summary_stats$max, 1),
    length(diameters)
  )
)
rownames(summary_statistics_table) <- c("Mean", "Standard Deviation", "Kurtosis", "Skewness", "Minimum", "Maximum", "Sample Size")

#-------------------- Species Distribution ---------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
species_length <- nrow(frequency_table) - 1

species_dist_table <- frequency_table[1:species_length, ] %>%
  mutate(
    `N/HA` = (Frequency / area_of_plot_m2) * one_ha_m2,
    `Proportion(%)` = round((Frequency / sum(Frequency)) * 100, 1)
  )

#---------------------- Distribution Fitting -------------------#
fit_list <- list(
  Weibull = fitdist(diameters, "weibull"),
  Gamma = fitdist(diameters, "gamma"),
  Lognormal = fitdist(diameters, "lnorm"),
  Normal = fitdist(diameters, "norm"),
  Cauchy = fitdist(diameters, "cauchy"),
  Exponential = fitdist(diameters, "exp"),
  Logistics = fitdist(diameters, "logis")
)

# Display Summary for Each
lapply(fit_list, summary)

#----------------- Parameter Estimate Summary Tables ------------#
extract_params <- function(fit, param) {
  if (param %in% names(fit$estimate)) round(fit$estimate[[param]], 3) else "-"
}

extract_sd <- function(fit, param) {
  if (param %in% names(fit$sd)) round(fit$sd[[param]], 3) else "-"
}

dist_names <- names(fit_list)

summary_parameter_estimates_table <- data.frame(
  Shape = sapply(fit_list, extract_params, "shape"),
  Scale = sapply(fit_list, extract_params, "scale"),
  Location = sapply(fit_list, extract_params, "location"),
  Rate = sapply(fit_list, extract_params, "rate")
)
rownames(summary_parameter_estimates_table) <- dist_names

summary_standard_error_table <- data.frame(
  Shape = sapply(fit_list, extract_sd, "shape"),
  Scale = sapply(fit_list, extract_sd, "scale"),
  Location = sapply(fit_list, extract_sd, "location"),
  Rate = sapply(fit_list, extract_sd, "rate")
)
rownames(summary_standard_error_table) <- dist_names

#---------------------- Goodness of Fit --------------------------#
goodness_of_fit <- gofstat(
  fit_list,
  fitnames = dist_names
)

# Kolmogorov-Smirnov Table
kolmogorov_smirnov_table <- data.frame(
  Statistics = round(goodness_of_fit$ks, 3),
  Rank = rank(goodness_of_fit$ks),
  AIC = round(goodness_of_fit$aic, 2),
  BIC = round(goodness_of_fit$bic, 2)
)

# Cramer-von Mises Table
cramer_von_mises_table <- data.frame(
  Statistics = round(goodness_of_fit$cvm, 3),
  Rank = rank(goodness_of_fit$cvm),
  AIC = round(goodness_of_fit$aic, 2),
  BIC = round(goodness_of_fit$bic, 2)
)

# Anderson-Darling Table
anderson_darling_table <- data.frame(
  Statistics = round(goodness_of_fit$ad, 3),
  Rank = rank(goodness_of_fit$ad),
  AIC = round(goodness_of_fit$aic, 2),
  BIC = round(goodness_of_fit$bic, 2)
)

#---------------- Graphical Representations ----------------------#

# Individual Density Comparisons
par(mfrow = c(2, 2))
lapply(fit_list, function(f) denscomp(f, legendtext = f$distname, main = "", xlab = "DBH (cm)"))
par(mfrow = c(1, 1))

# Combined Density Comparison
denscomp(fit_list, legendtext = dist_names, main = "", xlab = "DBH (cm)")

# Combined CDF
cdfcomp(fit_list, legendtext = dist_names, main = "", ylab = "Cumulative Distribution Function", xlab = "DBH (cm)")

# Combined Quantile Plot
qqcomp(fit_list, legendtext = dist_names, main = "")

# Combined Probability Plot
ppcomp(fit_list, legendtext = dist_names, main = "")


# ------------------------------- Export Tables ---------------------------- #

# Define export directory
export_dir <- "G:\\Benion\\Benion Programmings\\R\\Analysis Result\\40 D-Dist Attah Samuel\\exported-tables"

# Create the folder if it doesn't exist
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

# Helper function for exporting
export_table <- function(data, name) {
  txt_path <- file.path(export_dir, paste0(name, ".txt"))
  csv_path <- file.path(export_dir, paste0(name, ".csv"))
  
  write.table(data, file = txt_path, sep = ",", quote = FALSE, row.names = FALSE)
  write.csv(data, file = csv_path, quote = FALSE, row.names = TRUE)
}

# Export all relevant tables
export_table(frequency_table, "frequency_table")
export_table(summary_statistics_table, "summary_statistics_table")
export_table(species_dist_table, "species_distribution_table")
export_table(summary_parameter_estimates_table, "summary_parameter_estimates_table")
export_table(summary_standard_error_table, "summary_standard_error_table")
export_table(kolmogorov_smirnov_table, "kolmogorov_smirnov_table")
export_table(cramer_von_mises_table, "cramer_von_mises_table")
export_table(anderson_darling_table, "anderson_darling_table")

# Confirmation message
cat("✅ All analysis tables have been exported successfully to:\n", export_dir, "\n")


#---------------------- Cleanup Section --------------------------#
p_unload(all)
rm(list = ls())
if (dev.cur() > 1) dev.off()
cat("\014")  # Clear console

#==============================================================#
#   END OF SCRIPT
#==============================================================#
