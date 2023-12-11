# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Random Data")

set.seed(123)  # For reproducibility, set seed

num_trees <- 5000

# Empty vectors to store data for all trees
tree_data <- data.frame(
  Common_Name = character(num_trees),
  Scientific_Name = character(num_trees),
  Tree_Height = numeric(num_trees),
  Aboveground_Biomass = numeric(num_trees),
  Live_Crown_Leaves = numeric(num_trees),
  Tree_Branches = numeric(num_trees),
  Tree_Bark = numeric(num_trees),
  Tree_Stem = numeric(num_trees),
  DBH = numeric(num_trees),
  Crown_Diameter = numeric(num_trees),
  Total_Aboveground_Carbon = numeric(num_trees),
  Foliage_Biomass = numeric(num_trees),
  Foliage_Leaves = numeric(num_trees),
  stringsAsFactors = FALSE
)

# Dictionary mapping common names to scientific names (genus and species)
species_mapping <- list(
  "Oak" = "Quercus robur",
  "Pine" = "Pinus sylvestris",
  "Maple" = "Acer saccharum",
  "Beech" = "Fagus sylvatica",
  "Spruce" = "Picea abies",
  "Ash" = "Fraxinus excelsior",
  "Cherry" = "Prunus avium",
  "Birch" = "Betula pendula",
  "Cedar" = "Cedrus libani",
  "Cypress" = "Cupressus sempervirens",
  "Fir" = "Abies alba",
  "Hemlock" = "Tsuga canadensis",
  "Larch" = "Larix decidua",
  "Locust" = "Robinia pseudoacacia",
  "Magnolia" = "Magnolia grandiflora",
  "Poplar" = "Populus tremula",
  "Willow" = "Salix alba",
  "Yew" = "Taxus baccata",
  "Walnut" = "Juglans regia",
  "Redwood" = "Sequoia sempervirens"
  # Add more species with their genus and species names
)

unique_species <- names(species_mapping)
num_unique_species <- length(unique_species)

# Randomly selecting a tree species and its scientific name (genus and species)
selected_common_name <- sample(names(species_mapping), 1)
selected_scientific_name <- species_mapping[[selected_common_name]]

for (index in 1:num_trees) {
  # Randomly selecting a tree species and its scientific name (genus and species)
  selected_common_name <- sample(unique_species, 1)
  tree_data$Common_Name[index] <- selected_common_name
  
  selected_scientific_name <- species_mapping[[selected_common_name]]
  tree_data$Scientific_Name[index] <- selected_scientific_name
  
  # Generating random values for each parameter for each tree
  tree_data$Tree_Height[index] <- runif(1, min = 5, max = 30)  # in meters
  tree_data$Aboveground_Biomass[index] <- runif(1, min = 100, max = 2000)  # in kilograms
  tree_data$Live_Crown_Leaves[index] <- runif(1, min = 10, max = 500)  # count of leaves
  tree_data$Tree_Branches[index] <- runif(1, min = 50, max = 1000)  # count of branches
  tree_data$Tree_Bark[index] <- runif(1, min = 20, max = 500)  # in kilograms
  tree_data$Tree_Stem[index] <- tree_data$Aboveground_Biomass[index] - tree_data$Tree_Bark[index]  # Calculating stem biomass
  tree_data$DBH[index] <- runif(1, min = 10, max = 100)  # in centimeters
  tree_data$Crown_Diameter[index] <- runif(1, min = 3, max = 15)  # in meters
  
  # Calculating total aboveground carbon
  tree_data$Total_Aboveground_Carbon[index] <- tree_data$Aboveground_Biomass[index] * 0.5  # Assuming carbon is 50% of biomass
  
  # Adding foliage data
  tree_data$Foliage_Biomass[index] <- runif(1, min = 10, max = 200)  # in kilograms
  tree_data$Foliage_Leaves[index] <- runif(1, min = 50, max = 1000)  # count of foliage leaves
}


# Writing Data to File(.csv)
write.csv(
  tree_data, 
  file = "tree_data.csv", 
  quote = FALSE, row.names = TRUE)

# Writing Data to File(.txt)
write.table(
  tree_data, 
  file = "tree_data.txt", 
  sep = ",", quote = FALSE, row.names = FALSE)



# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L