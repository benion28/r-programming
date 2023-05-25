#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//11 ABGC")

# Import data set
dataset <-  read.csv("full-data-2.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))
unique_species_dataset

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


# Basal area
m_cm <- 100
basal_area <- (pi*((dataset$DIAMETER/m_cm)^2))/4
dataset <- cbind(dataset, "BA" = basal_area)
data_description <- describe(dataset)

#------------------------------ Summary Statistic --------------------------------#
# Summary Statistic Table
summary_statistics_table <- data.frame(
  "DBH" = c(round(mean(dataset$DIAMETER), 1), round(sd(dataset$DIAMETER), 1),
            round(min(dataset$DIAMETER), 1), round(max(dataset$DIAMETER), 1),
            length(dataset$DIAMETER)),
  "THT" = c(round(mean(dataset$HEIGHT), 1), round(sd(dataset$HEIGHT), 1),
            round(min(dataset$HEIGHT), 1), round(max(dataset$HEIGHT), 1),
            length(dataset$HEIGHT)),
  "CD" = c(round(mean(dataset$CROWN.DIAMETER), 1), round(sd(dataset$CROWN.DIAMETER), 1),
           round(min(dataset$CROWN.DIAMETER), 1), round(max(dataset$CROWN.DIAMETER), 1),
           length(dataset$CROWN.DIAMETER)),
  "BA" = c(round(mean(dataset$BA), 3), round(sd(dataset$BA), 3),
           round(min(dataset$BA), 3), round(max(dataset$BA), 3),
           length(dataset$BA))
)
rownames(summary_statistics_table) <- c("Mean", "Standard Deviation", "Minimum",
                                        "Maximum", "Sample Size")


#------------------------------ Number of Trees per Hectar --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 10
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
  "Frequency" = species_frequency,
  "Trees.HA" = round(tree_per_ha, 1),
  "Proportion(%)" = round(species_proportion, 1)
)
species_dist_table <- rbind(species_dist_table, c("Total", sum(species_frequency), sum(tree_per_ha), sum(species_proportion)))


#------------------------------ Above Ground --------------------------------#
kg_tons <- 1000
# AGB
above_ground_biomas_kg <- 0.0673*(dataset$WOOD.DENSITY*(dataset$DIAMETER^2)*dataset$HEIGHT)^0.976
dataset <- cbind(dataset, "AGB.kg" = above_ground_biomas_kg)
above_ground_biomas_tons <- above_ground_biomas_kg / kg_tons
dataset <- cbind(dataset, "AGB.tons" = above_ground_biomas_tons)
above_ground_biomas_kg_ha <- above_ground_biomas_kg * expansion_factor
dataset <- cbind(dataset, "AGB.kg.ha" = above_ground_biomas_kg_ha)
above_ground_biomas_tons_ha <- above_ground_biomas_kg_ha / kg_tons
dataset <- cbind(dataset, "AGB.tons.ha" = above_ground_biomas_tons_ha)
# Summary AGB
summary_agb_table <- data.frame(
  "AGB.kg" = c(round(mean(dataset$AGB.kg), 1), round(sd(dataset$AGB.kg), 1), round(min(dataset$AGB.kg), 1), round(max(dataset$AGB.kg), 1), length(dataset$AGB.kg)), 
  "AGB.tons" = c(round(mean(dataset$AGB.tons), 3), round(sd(dataset$AGB.tons), 3), round(min(dataset$AGB.tons), 3), round(max(dataset$AGB.tons), 3), length(dataset$AGB.tons)),
  "AGB.kg.ha" = c(round(mean(dataset$AGB.kg.ha), 1), round(sd(dataset$AGB.kg.ha), 1), round(min(dataset$AGB.kg.ha), 1), round(max(dataset$AGB.kg.ha), 1), length(dataset$AGB.kg.ha)), 
  "AGB.tons.ha" = c(round(mean(dataset$AGB.tons.ha), 3), round(sd(dataset$AGB.tons.ha), 3), round(min(dataset$AGB.tons.ha), 3), round(max(dataset$AGB.tons.ha), 3), length(dataset$AGB.tons.ha))
)
rownames(summary_agb_table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum", "Sample Size")

#AGC
above_ground_carbon_kg <- above_ground_biomas_kg * 0.47
dataset <- cbind(dataset, "AGC.kg" = above_ground_carbon_kg)
above_ground_carbon_tons <- above_ground_carbon_kg /kg_tons
dataset <- cbind(dataset, "AGC.tons" = above_ground_carbon_tons)
above_ground_carbon_kg_ha <- above_ground_biomas_kg_ha * expansion_factor
dataset <- cbind(dataset, "AGC.kg.ha" = above_ground_carbon_kg_ha)
above_ground_carbon_tons_ha <- above_ground_carbon_kg_ha /kg_tons
dataset <- cbind(dataset, "AGC.tons.ha" = above_ground_carbon_tons_ha)
# Summary AGB
summary_agc_table <- data.frame(
  "AGC.kg" = c(round(mean(dataset$AGC.kg), 1), round(sd(dataset$AGC.kg), 1), round(min(dataset$AGC.kg), 1), round(max(dataset$AGC.kg), 1), length(dataset$AGC.kg)), 
  "AGC.tons" = c(round(mean(dataset$AGC.tons), 3), round(sd(dataset$AGC.tons), 3), round(min(dataset$AGC.tons), 3), round(max(dataset$AGC.tons), 3), length(dataset$AGC.tons)),
  "AGC.kg.ha" = c(round(mean(dataset$AGC.kg.ha), 1), round(sd(dataset$AGC.kg.ha), 1), round(min(dataset$AGC.kg.ha), 1), round(max(dataset$AGC.kg.ha), 1), length(dataset$AGC.kg.ha)), 
  "AGC.tons.ha" = c(round(mean(dataset$AGC.tons.ha), 3), round(sd(dataset$AGC.tons.ha), 3), round(min(dataset$AGC.tons.ha), 3), round(max(dataset$AGC.tons.ha), 3), length(dataset$AGC.tons.ha))
)
rownames(summary_agc_table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum", "Sample Size")


# Diameter Classes
class_1_10_dbh <- dataset %>% filter(DIAMETER > 1 & DIAMETER <= 10)
class_1_5_dbh <- dataset %>% filter(DIAMETER > 1 & DIAMETER <= 5)
class_5_10_dbh <- dataset %>% filter(DIAMETER > 5 & DIAMETER <= 10)
class_10_20_dbh <- dataset %>% filter(DIAMETER > 10 & DIAMETER <= 20)
class_10_15_dbh <- dataset %>% filter(DIAMETER > 10 & DIAMETER <= 15)
class_15_20_dbh <- dataset %>% filter(DIAMETER > 15 & DIAMETER <= 20)
class_20_25_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 25)
class_20_30_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 30)
class_25_dbh <- dataset %>% filter(DIAMETER > 25)
class_30_40_dbh <- dataset %>% filter(DIAMETER > 30 & DIAMETER <= 40)
class_40_50_dbh <- dataset %>% filter(DIAMETER > 40 & DIAMETER <= 50)
class_50_dbh <- dataset %>% filter(DIAMETER > 50)

# Height Classes
class_1_5_tht <- dataset %>% filter(HEIGHT > 1 & HEIGHT <= 5)
class_5_10_tht <- dataset %>% filter(HEIGHT > 5 & HEIGHT <= 10)
class_10_15_tht <- dataset %>% filter(HEIGHT > 10 & HEIGHT <= 15)
class_15_20_tht <- dataset %>% filter(HEIGHT > 15 & HEIGHT <= 20)
class_20_25_tht <- dataset %>% filter(HEIGHT > 20 & HEIGHT <= 25)
class_25_tht <- dataset %>% filter(HEIGHT > 25)

# Height Classes
class_1_5_cd <- dataset %>% filter(CROWN.DIAMETER > 1 & CROWN.DIAMETER <= 5)
class_5_10_cd <- dataset %>% filter(CROWN.DIAMETER > 5 & CROWN.DIAMETER <= 10)
class_10_15_cd <- dataset %>% filter(CROWN.DIAMETER > 10 & CROWN.DIAMETER <= 15)
class_15_20_cd <- dataset %>% filter(CROWN.DIAMETER > 15 & CROWN.DIAMETER <= 20)
class_20_25_cd <- dataset %>% filter(CROWN.DIAMETER > 20 & CROWN.DIAMETER <= 25)
class_25_cd <- dataset %>% filter(CROWN.DIAMETER > 25)

# Summary AG Class Table
summary_ag_classes_table <- data.frame(
  "Trees.ha" = c((length(class_1_10_dbh$DIAMETER) * expansion_factor), (length(class_10_20_dbh$DIAMETER) * expansion_factor), (length(class_20_30_dbh$DIAMETER) * expansion_factor), (length(class_30_40_dbh$DIAMETER) * expansion_factor), (length(class_40_50_dbh$DIAMETER) * expansion_factor), (length(class_50_dbh$DIAMETER) * expansion_factor)),
  "AGB.tons.ha" = c(round((sum(class_1_10_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_10_20_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_20_30_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_30_40_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_40_50_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_50_dbh$AGB.tons.ha) * expansion_factor), 1)),
  "AGC.tons.ha" = c(round((sum(class_1_10_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_10_20_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_20_30_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_30_40_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_40_50_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_50_dbh$AGC.tons.ha) * expansion_factor), 1))
)
summary_ag_classes_table <- rbind(summary_ag_classes_table, c(sum(summary_ag_classes_table$Trees.ha), sum(summary_ag_classes_table$AGB.tons.ha), sum(summary_ag_classes_table$AGC.tons.ha)))
rownames(summary_ag_classes_table) <- c("1 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50>", "Total")


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
# CD
class_1_5_cd_density <- length(class_1_5_cd$CROWN.DIAMETER)
class_5_10_cd_density <- length(class_5_10_cd$CROWN.DIAMETER)
class_10_15_cd_density <- length(class_10_15_cd$CROWN.DIAMETER)
class_15_20_cd_density <- length(class_15_20_cd$CROWN.DIAMETER)
class_20_25_cd_density <- length(class_20_25_cd$CROWN.DIAMETER)
class_25_cd_density <- length(class_25_cd$CROWN.DIAMETER)

summary_variable_classes_table <- data.frame(
  "DBH.density" = c(class_1_5_dbh_density, class_5_10_dbh_density, class_10_15_dbh_density, class_15_20_dbh_density, class_20_25_dbh_density, class_25_dbh_density),
  "DBH.trees.ha" = c((class_1_5_dbh_density/density), (class_5_10_dbh_density/density), (class_10_15_dbh_density/density), (class_15_20_dbh_density/density), (class_20_25_dbh_density/density), (class_25_dbh_density/density)),
  "THT.density" = c(class_1_5_tht_density, class_5_10_tht_density, class_10_15_tht_density, class_15_20_tht_density, class_20_25_tht_density, class_25_tht_density),
  "THT.trees.ha" = c((class_1_5_tht_density/density), (class_5_10_tht_density/density), (class_10_15_tht_density/density), (class_15_20_tht_density/density), (class_20_25_tht_density/density), (class_25_tht_density/density)),
  "CD.density" = c(class_1_5_cd_density, class_5_10_cd_density, class_10_15_cd_density, class_15_20_cd_density, class_20_25_cd_density, class_25_cd_density),
  "CD.trees.ha" = c((class_1_5_cd_density/density), (class_5_10_cd_density/density), (class_10_15_cd_density/density), (class_15_20_cd_density/density), (class_20_25_cd_density/density), (class_25_cd_density/density))
)
summary_variable_classes_table <- rbind(summary_variable_classes_table, 
  c(sum(summary_variable_classes_table$DBH.density), sum(summary_variable_classes_table$DBH.trees.ha), 
    sum(summary_variable_classes_table$THT.density), sum(summary_variable_classes_table$THT.trees.ha),
    sum(summary_variable_classes_table$CD.density), sum(summary_variable_classes_table$CD.trees.ha)
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
