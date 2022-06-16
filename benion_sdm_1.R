# 1. Load Libraries
pacman::p_load(pacman, raster, sp, RColorBrewer, pander, rgdal, e1071, class)


# 2. Import data set
dataset <-  read.csv("./downloaded-files/tree_distribution_MOOC_07_19/data/occurrences.csv", h = T)
pander(head(dataset), caption = 'Occurence Dataset of Pinus sylvestris')


# 3. Load the three climatic rasters----------------------------------------------------
DDEG <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/ddeg_1km") # Growing degree days
MIND <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/mind_1km") # Moisture index (precipitation)
SRAD <- raster("./downloaded-files/tree_distribution_MOOC_07_19/data/srad_1km") # Solar radiation


# 4. Visualize the climatic maps------------------------------------------------------------
par(mar = c(3, 0.1, 1, 0.1), mfrow = c(2, 2))
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F, legend = F)
plot(DDEG, legend.only = T, horizontal = T, add = T, col = rev(heat.colors(20)), smallplot = c(0.25, 0.75, 0.08, 0.1))
plot(MIND, col = brewer.pal(9, "PuBu"), box = F, axes = F, legend = F)
plot(MIND, legend.only = T, horizontal = T, add = T, col = brewer.pal(9, "PuBu"), smallplot = c(0.25, 0.75, 0.08, 0.1))
plot(SRAD, col = brewer.pal(9, "YlOrRd"), box = F, axes = F, legend = F)
plot(SRAD, legend.only = T, horizontal = T, add = T, col = brewer.pal(9, "YlOrRd"), smallplot = c(0.25, 0.75, 0.14, 0.16))


# 5. save the dataframe "climate"-----------------------------------------------------------
climate_data <- cbind(DDEG = extract(DDEG, dataset[, c("x", "y")]),
                 MIND = extract(MIND, dataset[, c("x", "y")]),
                 SRAD = extract(SRAD, dataset [, c("x", "y")]))


# 6. Append climatic data to dataset, discard incomplete observations--------------------
occurrences_data <- cbind(dataset, climate_data)
occurrences_data <- na.omit(occurrences_data)


# 7. Plot climate raster and abscence/presence data (random sample, otherwise we can't see the map well)
set.seed(1)
table(occurrences_data$presence)/nrow(occurrences_data) * 100
indx.oc <- sample(which(occurrences_data$presence == 1), round(1000 * sum(occurrences_data$presence == 1)/nrow(occurrences_data)))
indx.ab <- sample(which(occurrences_data$presence == 0), round(1000 * sum(occurrences_data$presence == 0)/nrow(occurrences_data)))
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(occurrences_data$x[indx.oc], occurrences_data$y[indx.oc], col = "green4", pch = 3, cex = 0.5)
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(occurrences_data$x[indx.ab], occurrences_data$y[indx.ab], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))


# 11. Fit Models based on degree days------------------------------------------------
#GLM
model_glm <- glm(presence ~ poly(DDEG, 2), data = occurrences_data, family = binomial, maxit = 100)
#SVM
model_svm <- svm(presence ~ poly(DDEG, 2), data = occurrences_data, cost = 100, gamma = 1)
#KNN
model_knn <- knn(presence ~ poly(DDEG, 2), data = occurrences_data, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
