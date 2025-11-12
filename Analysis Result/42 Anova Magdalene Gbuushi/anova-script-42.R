#----------------------------------- START -----------------------------------#

# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych, agricolae, car)

#----------------------------------- Export Directories ----------------------#
export_dir <- "G:\\Benion\\Benion Programmings\\R\\Analysis Result\\42 Anova Magdalene Gbuushi"
tables_dir <- file.path(export_dir, "exported-tables")
plots_dir <- file.path(export_dir, "exported-figures")

if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# Helper function for exporting
export_table <- function(data, name) {
  txt_path <- file.path(tables_dir, paste0(name, ".txt"))
  csv_path <- file.path(tables_dir, paste0(name, ".csv"))
  
  write.table(data, file = txt_path, sep = ",", quote = FALSE, row.names = FALSE)
  write.csv(data, file = csv_path, quote = FALSE, row.names = TRUE)
  
  print(data)
}

#----------------------------------- Load Data -------------------------------#
setwd(export_dir)
dataset <- read.csv("full-data-20.csv", head = TRUE, sep = ",")
head(dataset, n = 10)
summary(dataset)
data_description <- describe(dataset)

#----------------------------------- Boxplots -------------------------------#
for (var in c("DIAMETER", "HEIGHT", "NO.LEAVES")) {
  p <- ggplot(dataset, aes(x = TREATMENT, y = .data[[var]])) +
    geom_boxplot(fill = "gray") +
    labs(x = "Treatments", y = paste("Seedling", tolower(var))) +
    theme_minimal()
  
  print(p)  # Display interactively
  ggsave(file.path(plots_dir, paste0(var, "_boxplot.png")), plot = p, width = 7, height = 5)
}

#----------------------------------- Summary Statistics ----------------------#
dataset$TREATMENT <- as.factor(dataset$TREATMENT)

summary_stats <- dataset %>%
  group_by(TREATMENT) %>%
  summarise(
    Mean_Diameter = mean(DIAMETER, na.rm = TRUE),
    SD_Diameter = sd(DIAMETER, na.rm = TRUE),
    Min_Diameter = min(DIAMETER, na.rm = TRUE),
    Max_Diameter = max(DIAMETER, na.rm = TRUE),
    Mean_Height = mean(HEIGHT, na.rm = TRUE),
    SD_Height = sd(HEIGHT, na.rm = TRUE),
    Min_Height = min(HEIGHT, na.rm = TRUE),
    Max_Height = max(HEIGHT, na.rm = TRUE),
    Mean_Leaves = mean(NO.LEAVES, na.rm = TRUE),
    SD_Leaves = sd(NO.LEAVES, na.rm = TRUE),
    Min_Leaves = min(NO.LEAVES, na.rm = TRUE),
    Max_Leaves = max(NO.LEAVES, na.rm = TRUE)
  )
export_table(summary_stats, "summary_statistics")

#----------------------------------- Shapiro-Wilk Normality Test -------------#
shapiro_tests <- list(
  DIAMETER = shapiro.test(dataset$DIAMETER),
  HEIGHT = shapiro.test(dataset$HEIGHT),
  NO.LEAVES = shapiro.test(dataset$NO.LEAVES)
)

shapiro_table <- data.frame(
  "Variable" = names(shapiro_tests),
  "W.value" = sapply(shapiro_tests, function(x) x$statistic),
  "P.value" = sapply(shapiro_tests, function(x) x$p.value)
)
export_table(shapiro_table, "shapiro_wilk_test")

#----------------------------------- Homogeneity of Variance -----------------#
levene_results <- list(
  DIAMETER = leveneTest(DIAMETER ~ TREATMENT, data = dataset),
  HEIGHT = leveneTest(HEIGHT ~ TREATMENT, data = dataset),
  NO.LEAVES = leveneTest(NO.LEAVES ~ TREATMENT, data = dataset)
)

# Save Levene test results
invisible(lapply(names(levene_results), function(name) {
  capture.output(levene_results[[name]],
                 file = file.path(tables_dir, paste0("levene_", name, ".txt")))
}))

# Save Levene test results as CSV
for (var in names(levene_results)) {
  levene_df <- as.data.frame(levene_results[[var]])
  write.csv(levene_df, file.path(tables_dir, paste0("levene_", var, ".csv")), row.names = TRUE)
}

#----------------------------------- ANOVA Tests -----------------------------#
raw_anova_diameter <- aov(DIAMETER ~ TREATMENT, data = dataset)
aov_models <- list(
  DIAMETER = raw_anova_diameter,
  HEIGHT = aov(HEIGHT ~ TREATMENT, data = dataset),
  NO.LEAVES = aov(NO.LEAVES ~ TREATMENT, data = dataset)
)
anova_results <- lapply(aov_models, summary)

# Extract ANOVA summaries
anova_summary <- lapply(aov_models, function(model) {
  summary(model)[[1]]  # Extract the first table from summary
})

# Access individual summaries
anova_diameter <- anova_summary$DIAMETER
anova_height   <- anova_summary$HEIGHT
anova_no_leaves <- anova_summary$NO.LEAVES

# Save
export_table(anova_diameter, "anova_diameter")
export_table(anova_height, "anova_height")
export_table(anova_no_leaves, "anova_no_leaves")

#----------------------------------- Extract F and p-values -----------------------------#
anova_summary_table <- lapply(names(aov_models), function(var) {
  summary_obj <- summary(aov_models[[var]])[[1]]
  data.frame(
    Variable = var,
    F_value = summary_obj$`F value`[1],
    p_value = summary_obj$`Pr(>F)`[1]
  )
}) %>%
  do.call(rbind, .)

export_table(anova_summary_table, "anova_f_scores")

# Export ANOVA results as .txt
invisible(lapply(names(anova_results), function(name) {
  capture.output(anova_results[[name]],
                 file = file.path(tables_dir, paste0("anova_", name, ".txt")))
}))

# F-Critical Value
f_critical <- qf(1 - 0.05, summary(raw_anova_diameter)[[1]][1, 1], summary(raw_anova_diameter)[[1]][2, 1])

#----------------------------------- Post Hoc Tests (Tukey + HSD) ------------#
for (var in names(aov_models)) {
  model <- aov_models[[var]]
  
  # Get ANOVA summary and p-value for treatment
  anova_summary <- summary(model)[[1]]
  p_value <- anova_summary["TREATMENT", "Pr(>F)"]
  
  cat("\nVariable:", var, "\n")
  print(anova_summary)
  
  if (p_value < 0.05) {
    cat("P-VALUE is less than 0.05 (p =", round(p_value, 4), "), running post hoc tests...\n")
  } else {
    cat("P-VALUE is not less than 0.05 (p =", round(p_value, 4), "), skipping post hoc tests.\n")
  }
  
  # Only run post hoc if ANOVA is significant
  # if (p_value < 0.05) {
  if (TRUE) {
    cat("ANOVA is significant (p =", round(p_value, 4), "), running post hoc tests...\n")
    
    # --- Tukey Test ---
    tukey <- TukeyHSD(model)
    tukey_df <- data.frame(tukey$TREATMENT)
    export_table(tukey_df, paste0("tukey_", var))
    
    # Display Tukey plot interactively (no duplicate 'main' arg)
    print(plot(tukey, las = 1, col = "blue"))
    
    # Save Tukey plot to file safely
    png(file.path(plots_dir, paste0("tukey_", var, ".png")), width = 700, height = 500)
    plot(tukey, las = 1, col = "blue")
    dev.off()
    
    # --- HSD Test ---
    hsd <- HSD.test(model, "TREATMENT")
    export_table(hsd$means, paste0("hsd_means_", var))
    export_table(hsd$groups, paste0("hsd_groups_", var))
    
    # Display HSD summary interactively
    print(hsd$groups)
    
  } else {
    cat("ANOVA is not significant (p =", round(p_value, 4), "), skipping post hoc tests.\n")
  }
}


#----------------------------------- Clean Up --------------------------------#
# Unload Packages
p_unload(all)

# Remove objects from Global Environment
rm(list = ls())

# Clear Plots
if (length(dev.list()) > 0) dev.off()

# Clear Console
cat("\014")  # Equivalent to Ctrl + L

#----------------------------------- END ------------------------------------#
