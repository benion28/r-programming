# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Random Data")

set.seed(123)  # For reproducibility, set seed

num_trees <- 5000

# Empty vectors to store data for all trees
tree_species <- character(num_trees)
tree_height <- numeric(num_trees)
aboveground_biomass <- numeric(num_trees)
live_crown_leaves <- numeric(num_trees)
tree_branches <- numeric(num_trees)
tree_bark <- numeric(num_trees)
tree_stem <- numeric(num_trees)
dbh <- numeric(num_trees)
crown_diameter <- numeric(num_trees)
total_aboveground_carbon <- numeric(num_trees)
foliage_biomass <- numeric(num_trees)
foliage_leaves <- numeric(num_trees)

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
# species_mapping[tree_species]

for (i in 1:num_trees) {
  # Randomly selecting a tree species and its scientific name (genus and species)
  selected_common_name <- sample(unique_species, 1)
  tree_species[i] <- selected_common_name
  
  selected_scientific_name <- species_mapping[[selected_common_name]]
  
  # Generating random values for each parameter for each tree
  tree_height[i] <- runif(1, min = 5, max = 30)  # in meters
  aboveground_biomass[i] <- runif(1, min = 100, max = 2000)  # in kilograms
  live_crown_leaves[i] <- runif(1, min = 10, max = 500)  # count of leaves
  tree_branches[i] <- runif(1, min = 50, max = 1000)  # count of branches
  tree_bark[i] <- runif(1, min = 20, max = 500)  # in kilograms
  tree_stem[i] <- aboveground_biomass[i] - tree_bark[i]  # Calculating stem biomass
  dbh[i] <- runif(1, min = 10, max = 100)  # in centimeters
  crown_diameter[i] <- runif(1, min = 3, max = 15)  # in meters
  
  # Calculating total aboveground carbon
  total_aboveground_carbon[i] <- aboveground_biomass[i] * 0.5  # Assuming carbon is 50% of biomass
  
  # Adding foliage data
  foliage_biomass[i] <- runif(1, min = 10, max = 200)  # in kilograms
  foliage_leaves[i] <- runif(1, min = 50, max = 1000)  # count of foliage leaves
}

final_data <- data.frame(
  "common_names"=tree_species,
  "aboveground_biomass"=aboveground_biomass, "aboveground_carbon"=total_aboveground_carbon, 
  "crown_diameter"=crown_diameter, "dbh"=dbh, "foliage_biomass"=foliage_biomass, 
  "foliage_leaves"=foliage_leaves, "live_crown_leaves"=live_crown_leaves, "tree_bark"=tree_bark,
  "tree_branches"=tree_branches, "tree_height"=tree_height, tree_stem=tree_stem)


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L