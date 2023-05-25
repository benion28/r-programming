#----------------------------------- START ----------------------------#
	
# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych, abdiv, BiodiversityR)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//14 Spp-Div")

# Import data set
dataset <-  read.csv("full-data-3.csv", head=TRUE, sep=",")
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

# Frequency
species_frequency <- c()
species_basal_area <- c()
for (variable in unique_species_dataset$Unique.Species) {
  item <- species_frequency
  basal_area <- species_basal_area
  total <- length(dataset$SPECIES[dataset$SPECIES == variable])
  area <- sum(dataset$BA[dataset$SPECIES == variable])
  species_frequency <- append(item, total)
  species_basal_area <- append(basal_area, area)
}

# Frequency Table
frequency_table <- data.frame(
  "Species" = species_list,
  "Frequency" = species_frequency,
  "BA" = species_basal_area
)
frequency_table <- rbind(frequency_table, "Total" = c("Total", sum(frequency_table$Frequency), sum(frequency_table$BA)))


#------------------------------ Number of Trees per Hectar --------------------------------#
one_ha_m2 <- 10000
area_of_plot_m2 <- 50 * 50
area_of_plot_ha <- area_of_plot_m2 / one_ha_m2
number_of_plots <- 11
expansion_factor <- one_ha_m2 / (area_of_plot_m2 * number_of_plots)


#------------------------------ Species Distribution --------------------------------#
percentage <- 100
species_names <- unique_species_dataset$Unique.Species
species_length <- length(frequency_table$Frequency)
tree_per_ha <- expansion_factor * species_frequency
total_species <- sum(species_frequency)
total_trees_ha <- expansion_factor * total_species
species_proportion <- ((tree_per_ha/total_trees_ha) * percentage)
species_BA <- as.numeric(frequency_table$BA[1: length(frequency_table$BA) - 1])
species_RF <- (species_frequency/sum(species_frequency) * percentage)
species_RD <- species_proportion
species_RDo <- (species_BA/sum(species_BA) * percentage)
species_IVR <- (species_RF+species_RD+species_RDo)
species_dist_table <- data.frame(
  "Species" = species_names,
  "Frequency" = species_frequency,
  "Trees.HA" = round(tree_per_ha, 1),
  "RF(%)" = round(species_RF, 1),
  "RD(%)" = round(species_RD, 1),
  "RDo(%)" = round(species_RDo, 1),
  "IVI" = round(species_IVR, 1)
)
species_dist_table <- rbind(species_dist_table, 
  c("Total", sum(species_frequency), sum(tree_per_ha), sum(species_RF),
    sum(species_RD), sum(species_RDo), sum(species_IVR))
)


#------------------------------ Locations --------------------------------#
# location_1 <- dataset %>% filter(L1 == "Television")


#------------------------------- Diversity ------------------------------#
species_unique_data <- frequency_table %>% filter(Species != "Total")
species_unique_frequency <- as.numeric(species_unique_data$Frequency)
species_diversity<- rbind(berger_parker = berger_parker_d(species_unique_frequency),
  brillouin_d = brillouin_d(species_unique_frequency), dominance = dominance(species_unique_frequency),
  heip_e = heip_e(species_unique_frequency), invsimpson = invsimpson(species_unique_frequency),
  kempton_taylor_q = kempton_taylor_q(species_unique_frequency), margalef = margalef(species_unique_frequency),
  mcintosh_d = mcintosh_d(species_unique_frequency), mcintosh_e = mcintosh_e(species_unique_frequency),
  menhinick = menhinick(species_unique_frequency), pielou_e = pielou_e(species_unique_frequency),
  richness = richness(species_unique_frequency), shannon = shannon(species_unique_frequency),
  simpson = simpson(species_unique_frequency),simpson_e = simpson_e(species_unique_frequency),
  strong = strong(species_unique_frequency)
)
species_diversity <- data.frame(species_diversity)

#important value index
IVI_plots <- importancevalue.comp(dataset, site="PLOT", species="SPECIES",count="COUNT", basal="BA", factor="PLOT")
IVI_forest <- importancevalue.comp(dataset, site="PLOT", species="SPECIES",count="COUNT", basal="BA", factor="FOREST")
# Table
IVI_forest_table <- data.frame(IVI_forest$FOR)
# Summary
comp_RF <- IVI_forest_table$frequency.percent
comp_RD <- IVI_forest_table$density.percent
comp_RDo <- IVI_forest_table$dominance.percent
comp_IVI <- IVI_forest_table$importance.value
IVI_forest_table <- cbind(IVI_forest_table, "RF" = round(comp_RF, 1))
IVI_forest_table <- cbind(IVI_forest_table, "RD" = round(comp_RD, 1))
IVI_forest_table <- cbind(IVI_forest_table, "RDo" = round(comp_RDo, 1))
IVI_forest_table <- cbind(IVI_forest_table, "IVI" = round(comp_IVI, 1))
IVI_forest_summary_table <- IVI_forest_table %>% select(RF, RD, RDo, IVI)
IVI_forest_summary_table <- rbind(IVI_forest_summary_table, "Total" = c(
  sum(comp_RF), sum(comp_RD), sum(comp_RDo), sum(comp_IVI)
))

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

# Height Classes
class_1_5_cd <- dataset %>% filter(CROWN.DIAMETER > 1 & CROWN.DIAMETER <= 5)
class_5_10_cd <- dataset %>% filter(CROWN.DIAMETER > 5 & CROWN.DIAMETER <= 10)
class_10_15_cd <- dataset %>% filter(CROWN.DIAMETER > 10 & CROWN.DIAMETER <= 15)
class_15_20_cd <- dataset %>% filter(CROWN.DIAMETER > 15 & CROWN.DIAMETER <= 20)
class_20_25_cd <- dataset %>% filter(CROWN.DIAMETER > 20 & CROWN.DIAMETER <= 25)
class_25_cd <- dataset %>% filter(CROWN.DIAMETER > 25)

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
