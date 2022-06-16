####################################################

## Project: Landscape Ecology MOOC, Case Study 2.1 Species Distribution Modelling

## Script purpose: Find a suitable Species Distribution Model for Pinus Sylvestris

## Date: 22th January, 2022

## Author: Bernard Iorver

##################################################


# 1. set working directory ------------------------------------------------------

setwd("C://Benion//Benion Programmings//R//EDX")


# 2. Install packages ----------------------------------------------------------------

install.packages("raster")
install.packages ("sp")
install.packages ("RColorBrewer")
install.packages ("pander")
install.packages ("rgdal")


# 3. load libraries ------------------------------------------------------------------

library(raster)
library (sp)
library (RColorBrewer)
library (pander)
library (rgdal)

# 4. load occurrence data ------------------------------------------------------------------

Occurrences <- read.csv("./downloaded-files/tree_distribution_MOOC_07_19/data/occurrences.csv", h = T)

pander(head(Occurrences), caption = 'This table contains the location of tree occurrences. 
      Botanists have conducted over 12,000 forest inventories across Switzerland, recording the cover of all tree species. 
      Here, the forest plots containing Pinus sylvestris are indicated with a "1", and the cover of the species in the plot is provided. 
      The plots without Pinus sylvestris are indicated with a "0". The coordinates of the plots is provided in the Swiss coordinate system "CH1903". 
      Finally, a few environmental predictors, including elevation, slope and cover are provided.')

# 5. Load the three climatic rasters----------------------------------------------------

# Climate data
# Growing degree days
DDEG <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/ddeg_1km")
# Moisture index (precipitation)
MIND <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/mind_1km")
# Solar radiation
SRAD <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/srad_1km")

# 6. Visualize the climatic maps------------------------------------------------------------

par(mar = c(3, 0.1, 1, 0.1), mfrow = c(2, 2))
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F, legend = F)
plot(DDEG, legend.only = T, horizontal = T, add = T, col = rev(heat.colors(20)), smallplot = c(0.25, 0.75, 0.08, 0.1))
plot(MIND, col = brewer.pal(9, "PuBu"), box = F, axes = F, legend = F)
plot(MIND, legend.only = T, horizontal = T, add = T, col = brewer.pal(9, "PuBu"), smallplot = c(0.25, 0.75, 0.08, 0.1))
plot(SRAD, col = brewer.pal(9, "YlOrRd"), box = F, axes = F, legend = F)
plot(SRAD, legend.only = T, horizontal = T, add = T, col = brewer.pal(9, "YlOrRd"), smallplot = c(0.25, 0.75, 0.14, 0.16))

# 7. save the dataframe "climate"-----------------------------------------------------------

Climate <- cbind(DDEG = extract(DDEG, Occurrences[, c("x", "y")]),
                 MIND = extract(MIND, Occurrences[, c("x", "y")]),
                 SRAD = extract(SRAD, Occurrences [, c("x", "y")]))

# 8. Append climatic data to occurences, discard incomplete observations--------------------

Occurrences <- cbind(Occurrences, Climate)
Occurrences <- na.omit(Occurrences)

# 9. Plot climate raster and abscence/presence data (random sample, otherwise we can't see the map well)

set.seed(1)
table(Occurrences$presence)/nrow(Occurrences) * 100
indx.oc <- sample(which(Occurrences$presence == 1), round(1000 * sum(Occurrences$presence == 1)/nrow(Occurrences)))
indx.ab <- sample(which(Occurrences$presence == 0), round(1000 * sum(Occurrences$presence == 0)/nrow(Occurrences)))
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(Occurrences$x[indx.oc], Occurrences$y[indx.oc], col = "green4", pch = 3, cex = 0.5)
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(Occurrences$x[indx.ab], Occurrences$y[indx.ab], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))

# 11. Fit GLM based on degree days------------------------------------------------
glm.uni <- glm(presence ~ poly(DDEG, 2), data = Occurrences,family = binomial, maxit = 100)
pander(summary(glm.uni)$coefficients, caption = "Summary of GLM model based on degree-days.")

# 12. Plot relationship degree-days and probability of presence---------------------------

par(mfrow = c(1, 1), mar=c(2,2,2,2))
data_plot <- data.frame(cbind(Occurrences$DDEG, predict(glm.uni, type = "response")))
sort1 <- na.omit(data_plot[order(data_plot[, 1], decreasing = FALSE),])
data_plot <- data.frame(cbind(sort1[, 1], sort1[, 2]))
plot(data_plot[, 1], data_plot[, 2], xlab = expression("Degree" - days ~ degree ~ C), ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))

# 13. Add observed response variable, presences in red and absences in blue-----------------

points(Occurrences$DDEG[Occurrences$presence == 1], Occurrences$presence[Occurrences$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(Occurrences$DDEG[Occurrences$presence == 0], Occurrences$presence[Occurrences$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))

# 14. Fit GLM based on moisture index only -------------------------------------------------

glm.uni <- glm(presence ~ poly(MIND, 2), data = Occurrences,family = binomial, maxit = 100)
pander(summary(glm.uni)$coefficients, caption = "Summary of GLM model based on moisture index.")

# 15. Plot a curve probability of presence in dependence of moisture -----------------------

par(mfrow = c(1, 1))
data_plot <- data.frame(cbind(Occurrences$MIND, predict(glm.uni, type = "response")))
sort1 <- na.omit(data_plot[order(data_plot[, 1], decreasing = FALSE),])
data_plot <- data.frame(cbind(sort1[, 1], sort1[, 2]))
plot(data_plot[, 1], data_plot[, 2], xlab ="Moisture index", ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))

# 16. Add observed response variable, presences (red) and absences (blue)-------------------

points(Occurrences$MIND[Occurrences$presence == 1], Occurrences$presence[Occurrences$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(Occurrences$MIND[Occurrences$presence == 0], Occurrences$presence[Occurrences$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))

# 17. Run GLM including three predictors (degree-days, moisture index and solar radiation) using linear and quadratic terms

glm.multi <- glm(presence ~ poly(DDEG, 2) + poly(MIND, 2) + poly(SRAD, 2), data = Occurrences, family = binomial, maxit = 100)
pander(summary(glm.multi)$coefficients, caption = "Summary of GLM model based on three climatic predictors")

# 18. Create a dataframe containing observed presence/absence of Pinus silvestris and the predicted probability of occurence for the response variable at each sampled location:

data_validation <- data.frame(cbind(Occurrences$presence, predict(glm.multi, type = "response")))
colnames(data_validation) <- c("Observed", "Projected")

 # 19. Load Functions.r file with necessary functions for model evaluation and compute the table of accuracy statistics with an arbitrary threshold of 0.5 ----------------------

source("./downloaded-files/Functions.r")
meva.table(data_validation[, 2], data_validation[, 1], 0.5)$CONTINGENCY_TABLE
meva.table(data_validation[, 2], data_validation[, 1], 0.5)$EVALUATION_METRICS[13,]

# 20. Seperate into training (70%) and evaluation sets (30%)-----------------------

set.seed(5)
indx.train <- sample(1:nrow(data_validation), size = round(0.7 * nrow(data_validation)))
indx.eval <- setdiff(1:nrow(data_validation), indx.train)

# 21. Plot threshold-TSS curve for training data using plot.tss function-------------------

plot.tss(data_validation$Projected[indx.train], data_validation$Observed[indx.train])

max_TSS <- max.TSS(data_validation$Projected[indx.train], data_validation$Observed[indx.train])
max_TSS[[2]]

# 22. Compute the table of accuracy statistics for the evaluation data with the optimized threshold of 0.15

(tab <- meva.table(data_validation[indx.eval, 2], data_validation[indx.eval, 1], as.numeric(max_TSS[[2]][2, 2])))$CONTINGENCY_TABLE
(tab <- meva.table(data_validation[indx.eval, 2], data_validation[indx.eval, 1], as.numeric(max_TSS[[2]][2, 2])))$EVALUATION_METRICS[13,]

# 23. Dataframe with x-y coordinates of all cells in the Switzerland raster----------------

CellsXY <- na.omit(as.data.frame(DDEG, xy = T))

# 24. Append the predictor values of these cells ----------------------

Projection <- na.omit(cbind(CellsXY[, c("x", "y")], DDEG = extract(DDEG, CellsXY[, c("x", "y")]), MIND = extract(MIND, CellsXY[, c("x", "y")]), SRAD = extract(SRAD, CellsXY[, c("x", "y")])))

# 25. Project model over all of Switzerland and store results as a raster ------------------

pred.glm.multi <- predict(glm.multi, Projection, type = "response")
proj.glm.multi <- cbind(Projection[, c("x", "y")], pred = pred.glm.multi)
pinus_sylvestris <- rasterFromXYZ(proj.glm.multi[, c("x", "y","pred")])

# 26. Plot a raster of projected probabilities of occurrence: ----------------------

par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
plot(pinus_sylvestris, axes = F, box = F, col = brewer.pal(9, "PuRd"))
points(Occurrences$x[indx.oc], Occurrences$y[indx.oc], col = "green4", pch = 3, cex = 0.5)
plot(pinus_sylvestris, axes = F, box = F, col = brewer.pal(9, "PuRd"))
points(Occurrences$x[indx.ab], Occurrences$y[indx.ab], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))

# 27. Convert probabilities in presence and absence ----------------------

pinus_sylvestris_binary <- pinus_sylvestris > as.numeric(max_TSS[[2]][2,2])
par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
plot(pinus_sylvestris_binary, box = F, axes = F, legend = F, col = c("grey", "green4"))
points(Occurrences$x[indx.oc], Occurrences$y[indx.oc], col = "black", pch = 3, cex = 0.5)
legend("bottomright", legend = c(" observed occurrence", " observed absence"), col = c("black", "red"), pch = 3, bg = "transparent", bty = "n")
plot(pinus_sylvestris_binary, box = F, axes = F, legend = F, col = c("grey", "green4"))
points(Occurrences$x[indx.ab], Occurrences$y[indx.ab], col = "red", pch = 3, cex = 0.5)
legend("topright", legend = c("predicted occurrence", "predicted absence"), fill = c("green4", "grey"), bg = "transparent", bty = "n")

