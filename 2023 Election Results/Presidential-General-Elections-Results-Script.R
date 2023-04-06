# 1. Load Libraries
pacman::p_load(pacman, raster, sp, RColorBrewer, pander, rgdal, e1071, class)

# 1. set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")

# 2. Import data set
dataset <-  read.csv("C://Benion//Benion Programmings//Python//AI & ML//data/presidential-general-elections-results-2023.csv", h = T)
pander(head(dataset), caption = 'Presidential General Elections Results 2023')

# States
all_states <- dataset[1]
all_apc <- dataset[2]
all_lp <- dataset[3]
all_nnpp <- dataset[4]
all_pdp <- dataset[5]
