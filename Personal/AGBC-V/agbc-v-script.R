#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Personal//AGBC-V")

# Import data set
dataset <-  read.csv("full-data.csv", head=TRUE, sep=",")
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
wood_densities <- c()
for (variable in unique_species_dataset$Unique.Species) {
  item <- species_frequency
  total <- length(dataset$SPECIES[dataset$SPECIES == variable])
  densities <- wood_densities
  wood_density <- unique(dataset$WOOD.DENSITY[dataset$SPECIES == variable])
  wood_densities <- append(densities, wood_density)
  species_frequency <- append(item, total)
}

# Frequency Table
frequency_table <- data.frame(
  "Species" = species_list,
  "Frequency" = species_frequency
)
frequency_table <- cbind(frequency_table, "Wood.Density" = wood_densities)
frequency_table <- rbind(frequency_table, "Total" = c("Total", sum(frequency_table$Frequency) , "-"))


# Basal area
m_cm <- 100
basal_area <- (pi*((dataset$DIAMETER/m_cm)^2))/4  # cm to m
dataset <- cbind(dataset, "BA" = basal_area)

# Volume
volume <- (pi/4) * (dataset$DIAMETER/m_cm)^2 * dataset$HEIGHT # cm to m
dataset <- cbind(dataset, "VOLUME" = volume)
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
  "VOL" = c(round(mean(dataset$VOLUME), 3), round(sd(dataset$VOLUME), 3),
           round(min(dataset$VOLUME), 3), round(max(dataset$VOLUME), 3),
           length(dataset$VOLUME)),
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
number_of_plots <- 15
expansion_factor <- one_ha_m2 / (area_of_plot_m2 * number_of_plots)


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
# VOL
vol_m3 <- dataset$VOLUME
vol_m3_ha <- dataset$VOLUME * expansion_factor
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
# VOL
vol_m3_ha <- dataset$VOLUME * expansion_factor
dataset <- cbind(dataset, "VOL.m3.ha" = vol_m3_ha)
# Summary AGB
summary_agc_table <- data.frame(
  "AGC.kg" = c(round(mean(dataset$AGC.kg), 1), round(sd(dataset$AGC.kg), 1), round(min(dataset$AGC.kg), 1), round(max(dataset$AGC.kg), 1), length(dataset$AGC.kg)), 
  "AGC.tons" = c(round(mean(dataset$AGC.tons), 3), round(sd(dataset$AGC.tons), 3), round(min(dataset$AGC.tons), 3), round(max(dataset$AGC.tons), 3), length(dataset$AGC.tons)),
  "AGC.kg.ha" = c(round(mean(dataset$AGC.kg.ha), 1), round(sd(dataset$AGC.kg.ha), 1), round(min(dataset$AGC.kg.ha), 1), round(max(dataset$AGC.kg.ha), 1), length(dataset$AGC.kg.ha)), 
  "AGC.tons.ha" = c(round(mean(dataset$AGC.tons.ha), 3), round(sd(dataset$AGC.tons.ha), 3), round(min(dataset$AGC.tons.ha), 3), round(max(dataset$AGC.tons.ha), 3), length(dataset$AGC.tons.ha)),
  "VOL.m3" = c(round(mean(dataset$VOLUME), 3), round(sd(dataset$VOLUME), 3), round(min(dataset$VOLUME), 3), round(max(dataset$VOLUME), 3), length(dataset$VOLUME)),
  "VOL.m3.ha" = c(round(mean(dataset$VOL.m3.ha), 3), round(sd(dataset$VOL.m3.ha), 3), round(min(dataset$VOL.m3.ha), 3), round(max(dataset$VOL.m3.ha), 3), length(dataset$VOL.m3.ha))
)
rownames(summary_agc_table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum", "Sample Size")


# Diameter Classes
class_1_20_dbh <- dataset %>% filter(DIAMETER > 1 & DIAMETER <= 20)
class_20_40_dbh <- dataset %>% filter(DIAMETER > 20 & DIAMETER <= 40)
class_40_60_dbh <- dataset %>% filter(DIAMETER > 40 & DIAMETER <= 60)
class_60_80_dbh <- dataset %>% filter(DIAMETER > 60 & DIAMETER <= 80)
class_80_100_dbh <- dataset %>% filter(DIAMETER > 80 & DIAMETER <= 100)
class_100_dbh <- dataset %>% filter(DIAMETER > 100)

# Height Classes
class_1_10_tht <- dataset %>% filter(HEIGHT > 1 & HEIGHT <= 10)
class_10_20_tht <- dataset %>% filter(HEIGHT > 10 & HEIGHT <= 20)
class_20_30_tht <- dataset %>% filter(HEIGHT > 20 & HEIGHT <= 30)
class_30_40_tht <- dataset %>% filter(HEIGHT > 30 & HEIGHT <= 40)
class_40_50_tht <- dataset %>% filter(HEIGHT > 40 & HEIGHT <= 50)
class_50_tht <- dataset %>% filter(HEIGHT > 50)


# Summary AG Class Table
summary_ag_classes_table <- data.frame(
  "Trees.ha" = c((length(class_1_20_dbh$DIAMETER) * expansion_factor), (length(class_20_40_dbh$DIAMETER) * expansion_factor), (length(class_40_60_dbh$DIAMETER) * expansion_factor), (length(class_60_80_dbh$DIAMETER) * expansion_factor), (length(class_80_100_dbh$DIAMETER) * expansion_factor), (length(class_100_dbh$DIAMETER) * expansion_factor)),
  "AGB.tons.ha" = c(round((sum(class_1_20_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_20_40_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_40_60_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_60_80_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_80_100_dbh$AGB.tons.ha) * expansion_factor), 1), round((sum(class_100_dbh$AGB.tons.ha) * expansion_factor), 1)),
  "AGC.tons.ha" = c(round((sum(class_1_20_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_20_40_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_40_60_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_60_80_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_80_100_dbh$AGC.tons.ha) * expansion_factor), 1), round((sum(class_100_dbh$AGC.tons.ha) * expansion_factor), 1)),
  "VOL.m3.ha" = c(round((sum(class_1_20_dbh$VOL.m3.ha)), 2), round((sum(class_20_40_dbh$VOL.m3.ha)), 2), round((sum(class_40_60_dbh$VOL.m3.ha)), 2), round((sum(class_60_80_dbh$VOL.m3.ha)), 2), round((sum(class_80_100_dbh$VOL.m3.ha)), 2), round((sum(class_100_dbh$VOL.m3.ha)), 2))
)
summary_ag_classes_table <- rbind(summary_ag_classes_table, c(sum(summary_ag_classes_table$Trees.ha), sum(summary_ag_classes_table$AGB.tons.ha), sum(summary_ag_classes_table$AGC.tons.ha), sum(summary_ag_classes_table$VOL.m3.ha)))
rownames(summary_ag_classes_table) <- c("1 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100", "100>", "Total")


# Summary Variables Class Table
density <- area_of_plot_ha * number_of_plots
# DBH
class_1_20_dbh_density <- length(class_1_20_dbh$DIAMETER)
class_20_40_dbh_density <- length(class_20_40_dbh$DIAMETER)
class_40_60_dbh_density <- length(class_40_60_dbh$DIAMETER)
class_60_80_dbh_density <- length(class_60_80_dbh$DIAMETER)
class_80_100_dbh_density <- length(class_80_100_dbh$DIAMETER)
class_100_dbh_density <- length(class_100_dbh$DIAMETER)
# THT
class_1_10_tht_density <- length(class_1_10_tht$HEIGHT)
class_10_20_tht_density <- length(class_10_20_tht$HEIGHT)
class_20_30_tht_density <- length(class_20_30_tht$HEIGHT)
class_30_40_tht_density <- length(class_30_40_tht$HEIGHT)
class_40_50_tht_density <- length(class_40_50_tht$HEIGHT)
class_50_tht_density <- length(class_50_tht$HEIGHT)


summary_variable_classes_table <- data.frame(
  "DBH.density" = c(class_1_20_dbh_density, class_20_40_dbh_density, class_40_60_dbh_density, class_60_80_dbh_density, class_80_100_dbh_density, class_100_dbh_density),
  "DBH.trees.ha" = c((class_1_20_dbh_density/density), (class_20_40_dbh_density/density), (class_40_60_dbh_density/density), (class_60_80_dbh_density/density), (class_80_100_dbh_density/density), (class_100_dbh_density/density)),
  "THT.density" = c(class_1_10_tht_density, class_10_20_tht_density, class_20_30_tht_density, class_30_40_tht_density, class_40_50_tht_density, class_50_tht_density),
  "THT.trees.ha" = c((class_1_10_tht_density/density), (class_10_20_tht_density/density), (class_20_30_tht_density/density), (class_30_40_tht_density/density), (class_40_50_tht_density/density), (class_50_tht_density/density))
)
summary_variable_classes_table <- rbind(summary_variable_classes_table, 
                                        c(sum(summary_variable_classes_table$DBH.density), sum(summary_variable_classes_table$DBH.trees.ha), 
                                          sum(summary_variable_classes_table$THT.density), sum(summary_variable_classes_table$THT.trees.ha)
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
