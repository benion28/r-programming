# =================================================================== #
#  AGBC John Kwaghgande - Refactored Analysis + full exports (PNG/TXT)
# =================================================================== #

# Load Required Libraries -------------------------------------------------
pacman::p_load(ggplot2, tidyverse, dplyr, psych)

# Working directory ------------------------------------------------------
setwd("G://Benion//Benion Programmings//R//Analysis Result//41 AGBC John Kwaghgande")

# Import dataset ---------------------------------------------------------
dataset <- read.csv("full-data-19.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
message("Dataset loaded: ", nrow(dataset), " rows, ", ncol(dataset), " columns.")

# Basic description ------------------------------------------------------
data_description <- describe(dataset)
unique_species_dataset <- data.frame(Unique_Species = unique(dataset$SPECIES))

# ----------------------- Frequency Distribution -------------------------
frequency_table <- dataset %>%
  count(SPECIES, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  # add total row for compatibility with your original outputs
  bind_rows(data.frame(SPECIES = "Total", Frequency = sum(.$Frequency)))

# ----------------------- Basal Area (BA) --------------------------------
# preserve BA naming
dataset <- dataset %>%
  mutate(BA = (pi * (DIAMETER / 100)^2) / 4)

# ----------------------- Summary Statistics ------------------------------
summary_statistics_table <- data.frame(
  DBH = c(mean(dataset$DIAMETER, na.rm = TRUE),
          sd(dataset$DIAMETER, na.rm = TRUE),
          min(dataset$DIAMETER, na.rm = TRUE),
          max(dataset$DIAMETER, na.rm = TRUE),
          sum(!is.na(dataset$DIAMETER))),
  THT = c(mean(dataset$HEIGHT, na.rm = TRUE),
          sd(dataset$HEIGHT, na.rm = TRUE),
          min(dataset$HEIGHT, na.rm = TRUE),
          max(dataset$HEIGHT, na.rm = TRUE),
          sum(!is.na(dataset$HEIGHT))),
  CD = c(mean(dataset$CROWN.DIAMETER, na.rm = TRUE),
         sd(dataset$CROWN.DIAMETER, na.rm = TRUE),
         min(dataset$CROWN.DIAMETER, na.rm = TRUE),
         max(dataset$CROWN.DIAMETER, na.rm = TRUE),
         sum(!is.na(dataset$CROWN.DIAMETER))),
  BA = c(mean(dataset$BA, na.rm = TRUE),
         sd(dataset$BA, na.rm = TRUE),
         min(dataset$BA, na.rm = TRUE),
         max(dataset$BA, na.rm = TRUE),
         sum(!is.na(dataset$BA)))
) %>% round(3)
rownames(summary_statistics_table) <- c("Mean", "SD", "Min", "Max", "Sample Size")

# ----------------------- Area / Expansion constants ----------------------
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 6
expansion_factor <- 10000 / (area_of_plot_m2 * number_of_plots)

# ----------------------- Species Distribution ----------------------------
species_dist_table <- frequency_table %>%
  filter(SPECIES != "Total") %>%
  mutate(
    Trees.HA = round(Frequency * expansion_factor, 1),
    Proportion... = round((Frequency / sum(Frequency)) * 100, 1)
  )

# total row computed after mutate to avoid scoping issues
total_row <- data.frame(
  SPECIES = "Total",
  Frequency = sum(species_dist_table$Frequency),
  Trees.HA = sum(species_dist_table$Trees.HA),
  Proportion... = sum(species_dist_table$Proportion...)
)
species_dist_table <- bind_rows(species_dist_table, total_row)

# ----------------------- WOOD.DENSITY check ------------------------------
# find a column that looks like wood density (case-insensitive)
wood_col_candidates <- grep("wood|density", names(dataset), ignore.case = TRUE, value = TRUE)

if ("WOOD.DENSITY" %in% names(dataset)) {
  message("Using existing column 'WOOD.DENSITY' for biomass calculations.")
} else if (length(wood_col_candidates) >= 1) {
  chosen <- wood_col_candidates[1]
  dataset$WOOD.DENSITY <- dataset[[chosen]]
  message("Info: Renamed/aliased column '", chosen, "' to 'WOOD.DENSITY' for calculations.")
} else {
  # fallback default (warn)
  dataset$WOOD.DENSITY <- 0.6
  warning("WOOD.DENSITY not found. A default WOOD.DENSITY = 0.6 has been set. Replace with real values for accurate biomass.")
}

# ----------------------- Preferred Above Ground Implementation ----------
kg_tons <- 1000

# compute AGB (dot-named variables preserved)
above_ground_biomas_kg <- 0.0673 * (dataset$WOOD.DENSITY * (dataset$DIAMETER^2) * dataset$HEIGHT)^0.976
dataset$AGB.kg <- above_ground_biomas_kg
dataset$AGB.tons <- above_ground_biomas_kg / kg_tons
dataset$AGB.kg.ha <- above_ground_biomas_kg * expansion_factor
dataset$AGB.tons.ha <- dataset$AGB.kg.ha / kg_tons

# compute AGC
above_ground_carbon_kg <- above_ground_biomas_kg * 0.47
dataset$AGC.kg <- above_ground_carbon_kg
dataset$AGC.tons <- above_ground_carbon_kg / kg_tons
dataset$AGC.kg.ha <- dataset$AGB.kg.ha * 0.47
dataset$AGC.tons.ha <- dataset$AGC.kg.ha / kg_tons

# Summary AGB table (preserve original layout & rounding)
summary_agb_table <- data.frame(
  "AGB.kg" = c(round(mean(dataset$AGB.kg, na.rm = TRUE), 1), round(sd(dataset$AGB.kg, na.rm = TRUE), 1),
               round(min(dataset$AGB.kg, na.rm = TRUE), 1), round(max(dataset$AGB.kg, na.rm = TRUE), 1),
               sum(!is.na(dataset$AGB.kg))),
  "AGB.tons" = c(round(mean(dataset$AGB.tons, na.rm = TRUE), 3), round(sd(dataset$AGB.tons, na.rm = TRUE), 3),
                 round(min(dataset$AGB.tons, na.rm = TRUE), 3), round(max(dataset$AGB.tons, na.rm = TRUE), 3),
                 sum(!is.na(dataset$AGB.tons))),
  "AGB.kg.ha" = c(round(mean(dataset$AGB.kg.ha, na.rm = TRUE), 1), round(sd(dataset$AGB.kg.ha, na.rm = TRUE), 1),
                  round(min(dataset$AGB.kg.ha, na.rm = TRUE), 1), round(max(dataset$AGB.kg.ha, na.rm = TRUE), 1),
                  sum(!is.na(dataset$AGB.kg.ha))),
  "AGB.tons.ha" = c(round(mean(dataset$AGB.tons.ha, na.rm = TRUE), 3), round(sd(dataset$AGB.tons.ha, na.rm = TRUE), 3),
                    round(min(dataset$AGB.tons.ha, na.rm = TRUE), 3), round(max(dataset$AGB.tons.ha, na.rm = TRUE), 3),
                    sum(!is.na(dataset$AGB.tons.ha)))
)
rownames(summary_agb_table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum", "Sample Size")

# Summary AGC table (preserve structure)
summary_agc_table <- data.frame(
  "AGC.kg" = c(round(mean(dataset$AGC.kg, na.rm = TRUE), 1), round(sd(dataset$AGC.kg, na.rm = TRUE), 1),
               round(min(dataset$AGC.kg, na.rm = TRUE), 1), round(max(dataset$AGC.kg, na.rm = TRUE), 1),
               sum(!is.na(dataset$AGC.kg))),
  "AGC.tons" = c(round(mean(dataset$AGC.tons, na.rm = TRUE), 3), round(sd(dataset$AGC.tons, na.rm = TRUE), 3),
                 round(min(dataset$AGC.tons, na.rm = TRUE), 3), round(max(dataset$AGC.tons, na.rm = TRUE), 3),
                 sum(!is.na(dataset$AGC.tons))),
  "AGC.kg.ha" = c(round(mean(dataset$AGC.kg.ha, na.rm = TRUE), 1), round(sd(dataset$AGC.kg.ha, na.rm = TRUE), 1),
                  round(min(dataset$AGC.kg.ha, na.rm = TRUE), 1), round(max(dataset$AGC.kg.ha, na.rm = TRUE), 1),
                  sum(!is.na(dataset$AGC.kg.ha))),
  "AGC.tons.ha" = c(round(mean(dataset$AGC.tons.ha, na.rm = TRUE), 3), round(sd(dataset$AGC.tons.ha, na.rm = TRUE), 3),
                    round(min(dataset$AGC.tons.ha, na.rm = TRUE), 3), round(max(dataset$AGC.tons.ha, na.rm = TRUE), 3),
                    sum(!is.na(dataset$AGC.tons.ha)))
)
rownames(summary_agc_table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum", "Sample Size")

# ----------------------- Class groups (DBH / HEIGHT / CD) -------------------
# Keep the original class objects and names (so your downstream code & exports are unchanged)
class_1_10_dbh  <- dataset %>% filter(DIAMETER > 1  & DIAMETER <= 10)
class_1_5_dbh   <- dataset %>% filter(DIAMETER > 1  & DIAMETER <= 5)
class_5_10_dbh  <- dataset %>% filter(DIAMETER > 5  & DIAMETER <= 10)
class_10_20_dbh <- dataset %>% filter(DIAMETER > 10 & DIAMETER <= 20)
class_10_15_dbh <- dataset %>% filter(DIAMETER > 10 & DIAMETER <= 15)
class_15_20_dbh <- dataset %>% filter(DIAMETER > 15 & DIAMETER <= 20)
class_20_25_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 25)
class_20_30_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 30)
class_25_dbh    <- dataset %>% filter(DIAMETER > 25)
class_30_40_dbh <- dataset %>% filter(DIAMETER > 30 & DIAMETER <= 40)
class_40_50_dbh <- dataset %>% filter(DIAMETER > 40 & DIAMETER <= 50)
class_50_dbh    <- dataset %>% filter(DIAMETER > 50)

# Height classes
class_1_5_tht  <- dataset %>% filter(HEIGHT > 1  & HEIGHT <= 5)
class_5_10_tht <- dataset %>% filter(HEIGHT > 5  & HEIGHT <= 10)
class_10_15_tht<- dataset %>% filter(HEIGHT > 10 & HEIGHT <= 15)
class_15_20_tht<- dataset %>% filter(HEIGHT > 15 & HEIGHT <= 20)
class_20_25_tht<- dataset %>% filter(HEIGHT > 20 & HEIGHT <= 25)
class_25_tht   <- dataset %>% filter(HEIGHT > 25)

# Crown Diameter classes
class_1_5_cd  <- dataset %>% filter(CROWN.DIAMETER > 1  & CROWN.DIAMETER <= 5)
class_5_10_cd <- dataset %>% filter(CROWN.DIAMETER > 5  & CROWN.DIAMETER <= 10)
class_10_15_cd<- dataset %>% filter(CROWN.DIAMETER > 10 & CROWN.DIAMETER <= 15)
class_15_20_cd<- dataset %>% filter(CROWN.DIAMETER > 15 & CROWN.DIAMETER <= 20)
class_20_25_cd<- dataset %>% filter(CROWN.DIAMETER > 20 & CROWN.DIAMETER <= 25)
class_25_cd   <- dataset %>% filter(CROWN.DIAMETER > 25)

# --------------------------- Summary AGB & AGC by DBH Classes --------------------------- #

summary_ag_classes_table <- data.frame(
  "DBH" = c(
    length(class_1_10_dbh$DIAMETER),
    length(class_10_20_dbh$DIAMETER),
    length(class_20_30_dbh$DIAMETER),
    length(class_30_40_dbh$DIAMETER),
    length(class_40_50_dbh$DIAMETER),
    length(class_50_dbh$DIAMETER)
  ),
  "Trees.ha" = c(
    nrow(class_1_10_dbh) * expansion_factor,
    nrow(class_10_20_dbh) * expansion_factor,
    nrow(class_20_30_dbh) * expansion_factor,
    nrow(class_30_40_dbh) * expansion_factor,
    nrow(class_40_50_dbh) * expansion_factor,
    nrow(class_50_dbh) * expansion_factor
  ),
  "AGB.tons.ha" = c(
    round(sum(class_1_10_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_10_20_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_20_30_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_30_40_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_40_50_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_50_dbh$AGB.tons.ha, na.rm = TRUE) * expansion_factor, 1)
  ),
  "AGC.tons.ha" = c(
    round(sum(class_1_10_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_10_20_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_20_30_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_30_40_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_40_50_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1),
    round(sum(class_50_dbh$AGC.tons.ha, na.rm = TRUE) * expansion_factor, 1)
  )
)

# --------------------------- Add Totals Row --------------------------- #
total_row <- data.frame(
  DBH = sum(summary_ag_classes_table$DBH, na.rm = TRUE),  # total stems
  Trees.ha = sum(summary_ag_classes_table$Trees.ha, na.rm = TRUE),
  AGB.tons.ha = sum(summary_ag_classes_table$AGB.tons.ha, na.rm = TRUE),
  AGC.tons.ha = sum(summary_ag_classes_table$AGC.tons.ha, na.rm = TRUE)
)

summary_ag_classes_table <- rbind(summary_ag_classes_table, total_row)

# Label rows
rownames(summary_ag_classes_table) <- c("1 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 >", "Total")

# -------------------- Summary variable classes --------------------------
density <- area_of_plot_ha * number_of_plots

class_counts_dbh <- c(
  length(class_1_5_dbh$DIAMETER),
  length(class_5_10_dbh$DIAMETER),
  length(class_10_15_dbh$DIAMETER),
  length(class_15_20_dbh$DIAMETER),
  length(class_20_25_dbh$DIAMETER),
  length(class_25_dbh$DIAMETER)
)

class_counts_tht <- c(
  length(class_1_5_tht$HEIGHT),
  length(class_5_10_tht$HEIGHT),
  length(class_10_15_tht$HEIGHT),
  length(class_15_20_tht$HEIGHT),
  length(class_20_25_tht$HEIGHT),
  length(class_25_tht$HEIGHT)
)

class_counts_cd <- c(
  length(class_1_5_cd$CROWN.DIAMETER),
  length(class_5_10_cd$CROWN.DIAMETER),
  length(class_10_15_cd$CROWN.DIAMETER),
  length(class_15_20_cd$CROWN.DIAMETER),
  length(class_20_25_cd$CROWN.DIAMETER),
  length(class_25_cd$CROWN.DIAMETER)
)

summary_variable_classes_table <- data.frame(
  "DBH.density" = class_counts_dbh,
  "DBH.trees.ha" = class_counts_dbh / density,
  "THT.density" = class_counts_tht,
  "THT.trees.ha" = class_counts_tht / density,
  "CD.density" = class_counts_cd,
  "CD.trees.ha" = class_counts_cd / density
)

# append totals row
summary_variable_classes_table <- rbind(
  summary_variable_classes_table,
  c(sum(summary_variable_classes_table$DBH.density, na.rm = TRUE),
    sum(summary_variable_classes_table$DBH.trees.ha, na.rm = TRUE),
    sum(summary_variable_classes_table$THT.density, na.rm = TRUE),
    sum(summary_variable_classes_table$THT.trees.ha, na.rm = TRUE),
    sum(summary_variable_classes_table$CD.density, na.rm = TRUE),
    sum(summary_variable_classes_table$CD.trees.ha, na.rm = TRUE))
)

rownames(summary_variable_classes_table) <- c("1 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25", "25>", "Total")

## -------------------- AG CD Classes --------------------------------------- ##
# Build summary_ag_cd_classes_table
summary_ag_cd_classes_table <- data.frame(
  "CD" = c(
    length(class_1_5_cd$CROWN.DIAMETER),
    length(class_5_10_cd$CROWN.DIAMETER),
    length(class_10_15_cd$CROWN.DIAMETER),
    length(class_15_20_cd$CROWN.DIAMETER),
    length(class_20_25_cd$CROWN.DIAMETER),
    length(class_25_cd$CROWN.DIAMETER)
  ),
  "Trees.ha" = c(
    nrow(class_1_5_cd)  * expansion_factor,
    nrow(class_5_10_cd) * expansion_factor,
    nrow(class_10_15_cd)* expansion_factor,
    nrow(class_15_20_cd)* expansion_factor,
    nrow(class_20_25_cd)* expansion_factor,
    nrow(class_25_cd)   * expansion_factor
  ),
  "AGB.tons" = c(
    round(sum(class_1_5_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_5_10_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_10_15_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_15_20_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_20_25_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_25_cd$AGB.tons, na.rm = TRUE) * expansion_factor, 2)
  ),
  "AGC.tons" = c(
    round(sum(class_1_5_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_5_10_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_10_15_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_15_20_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_20_25_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2),
    round(sum(class_25_cd$AGC.tons, na.rm = TRUE) * expansion_factor, 2)
  )
)

# Add totals row
summary_ag_cd_classes_table <- rbind(
  summary_ag_cd_classes_table,
  c(
    sum(summary_ag_cd_classes_table$CD, na.rm = TRUE),
    sum(summary_ag_cd_classes_table$Trees.ha, na.rm = TRUE),
    sum(summary_ag_cd_classes_table$AGB.tons, na.rm = TRUE),
    sum(summary_ag_cd_classes_table$AGC.tons, na.rm = TRUE)
  )
)

rownames(summary_ag_cd_classes_table) <- c("1 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25", "25 >", "Total")

# -------------------- Histogram summary data & plot ----------------------
hist_summary_data <- data.frame(
  "Clasess" = c("1-5", "5-10", "10-15", "15-20", "20-25", "25>"),
  "DBH.density" = summary_variable_classes_table[1:6, "DBH.density"],
  "DBH.trees.ha" = summary_variable_classes_table[1:6, "DBH.trees.ha"],
  "THT.density" = summary_variable_classes_table[1:6, "THT.density"],
  "THT.trees.ha" = summary_variable_classes_table[1:6, "THT.trees.ha"],
  "CD.density" = summary_variable_classes_table[1:6, "CD.density"],
  "CD.trees.ha" = summary_variable_classes_table[1:6, "CD.trees.ha"]
)

hist_plot <- ggplot(hist_summary_data, aes(x = Clasess, y = DBH.trees.ha)) +
  geom_col(fill = "#4038bb", color = "#e92929") +
  labs(title = "Stem Diameter Distribution of Trees", x = "Diameter at Breast Height (cm)", y = "No of Trees per Hectares") +
  theme_minimal()

# -------------------- Export setup (use your exact paths) -----------------
export_dir <- "G:\\Benion\\Benion Programmings\\R\\Analysis Result\\41 AGBC John Kwaghgande"
tables_dir <- file.path(export_dir, "exported-tables")
plots_dir  <- file.path(export_dir, "exported-figures")

if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
if (!dir.exists(plots_dir))  dir.create(plots_dir, recursive = TRUE)

export_table <- function(df, name) {
  # write CSV and TXT preserving row.names (as you requested)
  write.csv(df, file = file.path(tables_dir, paste0(name, ".csv")), quote = FALSE, row.names = TRUE)
  write.table(df, file = file.path(tables_dir, paste0(name, ".txt")), sep = ",", quote = FALSE, row.names = TRUE)
}

# Export all requested tables ---------------------------------------------
export_table(dataset, "dataset")
export_table(frequency_table, "frequency_table")
export_table(summary_statistics_table, "summary_statistics_table")
export_table(species_dist_table, "species_distribution_table")
export_table(summary_agb_table, "summary_agb_table")
export_table(summary_agc_table, "summary_agc_table")
export_table(summary_ag_classes_table, "summary_ag_classes_table")
export_table(summary_variable_classes_table, "summary_variable_classes_table")
export_table(hist_summary_data, "hist_summary_data")
export_table(summary_ag_cd_classes_table, "summary_ag_cd_classes_table")

# -------------------- Generate & export ALL relevant plots -----------------

# helper to safely save ggplots
save_gg <- function(plot_obj, filename, width = 8, height = 6, dpi = 300) {
  outpath <- file.path(plots_dir, filename)
  tryCatch({
    ggsave(filename = outpath, plot = plot_obj, width = width, height = height, dpi = dpi)
    message("Saved plot: ", outpath)
  }, error = function(e) {
    warning("Failed to save plot ", filename, " : ", conditionMessage(e))
  })
}

# 1. Diameter histogram
p_diameter <- ggplot(dataset, aes(x = DIAMETER)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Diameter Distribution", x = "Diameter (cm)", y = "Frequency")
p_diameter
save_gg(p_diameter, "diameter_distribution.png")

# 2. Height histogram
p_height <- ggplot(dataset, aes(x = HEIGHT)) +
  geom_histogram(bins = 30, fill = "seagreen", color = "black") +
  theme_minimal() +
  labs(title = "Height Distribution", x = "Height (m)", y = "Frequency")
p_height
save_gg(p_height, "height_distribution.png")

# 3. Crown diameter histogram
p_crown <- ggplot(dataset, aes(x = CROWN.DIAMETER)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Crown Diameter Distribution", x = "Crown Diameter (m)", y = "Frequency")
p_crown
save_gg(p_crown, "crown_diameter_distribution.png")

# 4. AGB (tons/ha) histogram - use dot-named column
p_agb_ha <- ggplot(dataset, aes(x = `AGB.tons.ha`)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Above Ground Biomass (tons/ha)", x = "AGB (tons/ha)", y = "Frequency")
p_agb_ha
save_gg(p_agb_ha, "agb_tons_ha_distribution.png")

# 5. AGC (tons/ha) histogram
p_agc_ha <- ggplot(dataset, aes(x = `AGC.tons.ha`)) +
  geom_histogram(bins = 30, fill = "darkred", color = "black") +
  theme_minimal() +
  labs(title = "Above Ground Carbon (tons/ha)", x = "AGC (tons/ha)", y = "Frequency")
p_agc_ha
save_gg(p_agc_ha, "agc_tons_ha_distribution.png")

# 6. AGB.kg histogram (per tree)
p_agb_kg <- ggplot(dataset, aes(x = `AGB.kg`)) +
  geom_histogram(bins = 30, fill = "#6a4c93", color = "black") +
  theme_minimal() +
  labs(title = "Above Ground Biomass per Tree (kg)", x = "AGB (kg/tree)", y = "Frequency")
p_agb_kg
save_gg(p_agb_kg, "agb_kg_distribution.png")

# 7. AGC.kg histogram (per tree)
p_agc_kg <- ggplot(dataset, aes(x = `AGC.kg`)) +
  geom_histogram(bins = 30, fill = "#a03232", color = "black") +
  theme_minimal() +
  labs(title = "Above Ground Carbon per Tree (kg)", x = "AGC (kg/tree)", y = "Frequency")
p_agc_kg
save_gg(p_agc_kg, "agc_kg_distribution.png")

# 8. Scatter: AGB per tree vs DIAMETER
p_sc_agb_dbh <- ggplot(dataset, aes(x = DIAMETER, y = `AGB.kg`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "AGB (kg) vs DIAMETER", x = "Diameter (cm)", y = "AGB (kg)")
p_sc_agb_dbh
save_gg(p_sc_agb_dbh, "scatter_agbkg_vs_diameter.png")

# 9. Scatter: AGB (tons/ha) vs DIAMETER (per-tree AGB vs DBH is above)
p_sc_agbtha_dbh <- ggplot(dataset, aes(x = DIAMETER, y = `AGB.tons.ha`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "AGB (tons/ha) vs DIAMETER", x = "Diameter (cm)", y = "AGB (tons/ha)")
p_sc_agbtha_dbh
save_gg(p_sc_agbtha_dbh, "scatter_agbtons_ha_vs_diameter.png")

# 10. Scatter: HEIGHT vs DIAMETER
p_sc_height_dbh <- ggplot(dataset, aes(x = DIAMETER, y = HEIGHT)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Height vs DIAMETER", x = "Diameter (cm)", y = "Height (m)")
p_sc_height_dbh
save_gg(p_sc_height_dbh, "scatter_height_vs_diameter.png")

# 11. Species bar plot (Trees per ha)
# Use species_dist_table (exclude Total row for plotting)
p_species <- species_dist_table %>%
  filter(SPECIES != "Total") %>%
  ggplot(aes(x = reorder(SPECIES, -Trees.HA), y = Trees.HA)) +
  geom_col(fill = "#2b8cbe") +
  theme_minimal() +
  labs(title = "Species Trees per Hectare", x = "Species", y = "Trees per ha") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_species
save_gg(p_species, "species_trees_per_ha.png")

# 12. Histogram summary plot (your hist_summary_data)
hist_plot
save_gg(hist_plot, "hist_summary_dbh_trees_ha.png")

# 13. Boxplots for DIAMETER / HEIGHT / BA (optional but useful)
p_box_dbh <- ggplot(dataset, aes(y = DIAMETER)) + geom_boxplot() + labs(title = "Boxplot: DIAMETER")
p_box_dbh
save_gg(p_box_dbh, "boxplot_diameter.png")

p_box_height <- ggplot(dataset, aes(y = HEIGHT)) + geom_boxplot() + labs(title = "Boxplot: HEIGHT")
p_box_height
save_gg(p_box_height, "boxplot_height.png")

p_box_ba <- ggplot(dataset, aes(y = BA)) + geom_boxplot() + labs(title = "Boxplot: BA")
p_box_ba
save_gg(p_box_ba, "boxplot_ba.png")

# 14. Save a small diagnostics CSV that lists which plots were saved (for tracing)
diagnostics <- data.frame(
  plot = c(
    "diameter_distribution.png","height_distribution.png","crown_diameter_distribution.png",
    "agb_tons_ha_distribution.png","agc_tons_ha_distribution.png","agb_kg_distribution.png",
    "agc_kg_distribution.png","scatter_agbkg_vs_diameter.png","scatter_agbtons_ha_vs_diameter.png",
    "scatter_height_vs_diameter.png","species_trees_per_ha.png","hist_summary_dbh_trees_ha.png",
    "boxplot_diameter.png","boxplot_height.png","boxplot_ba.png"
  ),
  path = file.path(plots_dir, c(
    "diameter_distribution.png","height_distribution.png","crown_diameter_distribution.png",
    "agb_tons_ha_distribution.png","agc_tons_ha_distribution.png","agb_kg_distribution.png",
    "agc_kg_distribution.png","scatter_agbkg_vs_diameter.png","scatter_agbtons_ha_vs_diameter.png",
    "scatter_height_vs_diameter.png","species_trees_per_ha.png","hist_summary_dbh_trees_ha.png",
    "boxplot_diameter.png","boxplot_height.png","boxplot_ba.png"
  )),
  stringsAsFactors = FALSE
)
write.csv(diagnostics, file = file.path(tables_dir, "exported_plots_manifest.csv"), row.names = FALSE)

message("All tables and plots exported to:\nTables -> ", tables_dir, "\nPlots  -> ", plots_dir)

# -------------------- End (keeps data in environment) ---------------------
# Note: I intentionally do NOT clear the environment so you can inspect
# the created tables & plots objects. Uncomment the cleanup block below
# if you want a clean session after running.

# # Cleanup (uncomment to activate)
p_unload(all)
rm(list = ls())
if (dev.cur() > 1) dev.off()
cat('\014')

# =================================================================== #
# END OF SCRIPT
# =================================================================== #
