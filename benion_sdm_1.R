# 1. Load Libraries
pacman::p_load(pacman, raster, sp, RColorBrewer, pander, rgdal, e1071, class)

# 1. set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")

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
index_present <- sample(which(occurrences_data$presence == 1), round(1000 * sum(occurrences_data$presence == 1)/nrow(occurrences_data)))
index_absent <- sample(which(occurrences_data$presence == 0), round(1000 * sum(occurrences_data$presence == 0)/nrow(occurrences_data)))
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(occurrences_data$x[index_present], occurrences_data$y[index_present], col = "green4", pch = 3, cex = 0.5)
plot(DDEG, col = rev(heat.colors(20)), box = F, axes = F)
points(occurrences_data$x[index_absent], occurrences_data$y[index_absent], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))


# 11. Fit Models based on degree days------------------------------------------------
#GLM
model_glm_ddeg <- glm(presence ~ poly(DDEG, 2), data = occurrences_data, family = binomial, maxit = 100)
predict_glm_ddeg <- predict(model_glm_ddeg, type = "response")
pander(summary(model_glm_ddeg)$coefficients, caption = "Summary of GLM model based on degree-days.")
#SVM
model_svm_ddeg <- svm(presence ~ poly(DDEG, 2), data = occurrences_data, cost = 100, gamma = 1)
predict_svm_ddeg <- predict(model_svm_ddeg, type = "response")
#KNN
model_knn_ddeg <- knn(presence ~ poly(DDEG, 2), data = occurrences_data, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
predict_knn_ddeg <- predict(model_knn_ddeg, type = "response")


# 12. Plot relationship degree-days and probability of presence---------------------------
#GLM
par(mfrow = c(1, 1), mar=c(2,2,2,2))
data_plot_glm_ddeg_1 <- data.frame(cbind(occurrences_data$DDEG, predict_glm_ddeg))
sort_glm_ddeg_1 <- na.omit(data_plot_glm_ddeg_1[order(data_plot_glm_ddeg_1[, 1], decreasing = FALSE),])
data_plot_glm_ddeg_2 <- data.frame(cbind(sort_glm_ddeg_1[, 1], sort_glm_ddeg_1[, 2]))
plot(data_plot_glm_ddeg_2[, 1], data_plot_glm_ddeg_2[, 2], xlab = expression("Degree" - days ~ degree ~ C), ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))
# Add observed response variable, presences in red and absences in blue-----------------
points(occurrences_data$DDEG[occurrences_data$presence == 1], occurrences_data$presence[occurrences_data$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(occurrences_data$DDEG[occurrences_data$presence == 0], occurrences_data$presence[occurrences_data$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))

#SVM
par(mfrow = c(1, 1), mar=c(2,2,2,2))
data_plot_svm_ddeg_1 <- data.frame(cbind(occurrences_data$DDEG, predict_svm_ddeg))
sort_svm_ddeg_1 <- na.omit(data_plot_svm_ddeg_1[order(data_plot_svm_ddeg_1[, 1], decreasing = FALSE),])
data_plot_svm_ddeg_2 <- data.frame(cbind(sort_svm_ddeg_1[, 1], sort_svm_ddeg_1[, 2]))
plot(data_plot_svm_ddeg_2[, 1], data_plot_svm_ddeg_2[, 2], xlab = expression("Degree" - days ~ degree ~ C), ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))
# Add observed response variable, presences in red and absences in blue-----------------
points(occurrences_data$DDEG[occurrences_data$presence == 1], occurrences_data$presence[occurrences_data$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(occurrences_data$DDEG[occurrences_data$presence == 0], occurrences_data$presence[occurrences_data$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))


# 13. Fit GLM based on moisture index only -------------------------------------------------
#GLM
model_glm_mind <- glm(presence ~ poly(MIND, 2), data = occurrences_data, family = binomial, maxit = 100)
predict_glm_mind <- predict(model_glm_mind, type = "response")
pander(summary(model_glm_mind)$coefficients, caption = "Summary of GLM model based on moisture index.")
#SVM
model_svm_mind <- svm(presence ~ poly(MIND, 2), data = occurrences_data, cost = 100, gamma = 1)
predict_svm_mind <- predict(model_svm_mind, type = "response")


# 14. Plot a curve probability of presence in dependence of moisture -----------------------
#GLM
par(mfrow = c(1, 1))
data_plot_glm_mind_1 <- data.frame(cbind(occurrences_data$MIND, predict_glm_mind))
sort_glm_mind_1 <- na.omit(data_plot_glm_mind_1[order(data_plot_glm_mind_1[, 1], decreasing = FALSE),])
data_plot_glm_mind_2 <- data.frame(cbind(sort_glm_mind_1[, 1], sort_glm_mind_1[, 2]))
plot(data_plot_glm_mind_2[, 1], data_plot_glm_mind_2[, 2], xlab ="Moisture index", ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))
# Add observed response variable, presences (red) and absences (blue)-------------------
points(occurrences_data$MIND[occurrences_data$presence == 1], occurrences_data$presence[occurrences_data$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(occurrences_data$MIND[occurrences_data$presence == 0], occurrences_data$presence[occurrences_data$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))

#SVM
par(mfrow = c(1, 1))
data_plot_svm_mind_1 <- data.frame(cbind(occurrences_data$MIND, predict_svm_mind))
sort_svm_mind_1 <- na.omit(data_plot_svm_mind_1[order(data_plot_svm_mind_1[, 1], decreasing = FALSE),])
data_plot_svm_mind_2 <- data.frame(cbind(sort_svm_mind_1[, 1], sort_svm_mind_1[, 2]))
plot(data_plot_glm_mind_2[, 1], data_plot_svm_mind_2[, 2], xlab ="Moisture index", ylab = "Probability of occurrence", frame.plot = F, type = "l", ylim = c(0, 1))
# Add observed response variable, presences (red) and absences (blue)-------------------
points(occurrences_data$MIND[occurrences_data$presence == 1], occurrences_data$presence[occurrences_data$presence == 1], col = rgb(1, 0, 0, alpha = 0.2), pch = 16)
points(occurrences_data$MIND[occurrences_data$presence == 0], occurrences_data$presence[occurrences_data$presence == 0], col = rgb(0, 0, 1, alpha = 0.2), pch = 16)
legend(0.2, 0.8, col = c("red", "blue", "black"), legend = c("presence", "absence", "probability"), pch = c(16, 16, NA), lty = c(NA, NA, 1))


# 15. Run Model including three predictors (degree-days, moisture index and solar radiation) using linear and quadratic terms
#GLM
model_glm_multi <- glm(presence ~ poly(DDEG, 2) + poly(MIND, 2) + poly(SRAD, 2), data = occurrences_data, family = binomial, maxit = 100)
predict_glm_multi <- predict(model_glm_multi, type = "response")
pander(summary(model_glm_multi)$coefficients, caption = "Summary of GLM model based on three climatic predictors")
#SVM
model_svm_multi <- svm(presence ~ poly(DDEG, 2) + poly(MIND, 2) + poly(SRAD, 2), data = occurrences_data, cost = 100, gamma = 1)
predict_svm_multi <- predict(model_svm_multi, type = "response")


# 15. Create a dataframe containing observed presence/absence of Pinus silvestris and the predicted probability of occurence for the response variable at each sampled location:
#GLM
data_validation_glm <- data.frame(cbind(occurrences_data$presence, predict_glm_multi))
colnames(data_validation_glm) <- c("Observed", "Projected")
#SVM
data_validation_svm <- data.frame(cbind(occurrences_data$presence, predict_svm_multi))
colnames(data_validation_svm) <- c("Observed", "Projected")


# 16. Load Functions.r file with necessary functions for model evaluation and compute the table of accuracy statistics with an arbitrary threshold of 0.5 ----------------------
source("./downloaded-files/Functions.r")
#GLM
contingency_table_glm <- meva.table(data_validation_glm[, 2], data_validation_glm[, 1], 0.5)$CONTINGENCY_TABLE
evaluation_matrics_glm <- meva.table(data_validation_glm[, 2], data_validation_glm[, 1], 0.5)$EVALUATION_METRICS[13,]
#SVM
contingency_table_svm <- meva.table(data_validation_svm[, 2], data_validation_svm[, 1], 0.5)$CONTINGENCY_TABLE
evaluation_matrics_svm <- meva.table(data_validation_svm[, 2], data_validation_svm[, 1], 0.5)$EVALUATION_METRICS[13,]


# 17. Seperate into training (70%) and evaluation sets (30%)-----------------------
#GLM
set.seed(5)
training_glm <- sample(1:nrow(data_validation_glm), size = round(0.7 * nrow(data_validation_glm)))
evaluation_glm <- setdiff(1:nrow(data_validation_glm), training_glm)
#SVM
set.seed(5)
training_svm <- sample(1:nrow(data_validation_svm), size = round(0.7 * nrow(data_validation_svm)))
evaluation_svm <- setdiff(1:nrow(data_validation_svm), training_svm)


# 18. Plot threshold-TSS curve for training data using plot.tss function-------------------
#GLM
plot.tss(data_validation_glm$Projected[training_glm], data_validation_glm$Observed[training_glm])
max_TSS_glm <- max.TSS(data_validation_glm$Projected[training_glm], data_validation_glm$Observed[training_glm])
max_TSS_glm[[2]]
#SVM
plot.tss(data_validation_svm$Projected[training_svm], data_validation_svm$Observed[training_svm])
max_TSS_svm <- max.TSS(data_validation_svm$Projected[training_svm], data_validation_svm$Observed[training_svm])
max_TSS_svm[[2]]


# 19. Compute the table of accuracy statistics for the evaluation data with the optimized threshold of 0.15
#GLM
(accuracy_contingency_table_glm <- meva.table(data_validation_glm[evaluation_glm, 2], data_validation_glm[evaluation_glm, 1], as.numeric(max_TSS_glm[[2]][2, 2])))$CONTINGENCY_TABLE
(accuracy_evaluation_metrics_glm <- meva.table(data_validation_glm[evaluation_glm, 2], data_validation_glm[evaluation_glm, 1], as.numeric(max_TSS_glm[[2]][2, 2])))$EVALUATION_METRICS[13,]
#SVM
(accuracy_contingency_table_svm <- meva.table(data_validation_svm[evaluation_svm, 2], data_validation_svm[evaluation_svm, 1], as.numeric(max_TSS_svm[[2]][2, 2])))$CONTINGENCY_TABLE
(accuracy_evaluation_metrics_svm <- meva.table(data_validation_svm[evaluation_svm, 2], data_validation_svm[evaluation_svm, 1], as.numeric(max_TSS_svm[[2]][2, 2])))$EVALUATION_METRICS[13,]


# 20. Dataframe with x-y coordinates of all cells in the Switzerland raster----------------
CellsXY <- na.omit(as.data.frame(DDEG, xy = T))


# 21. Append the predictor values of these cells ----------------------
Projection <- na.omit(cbind(CellsXY[, c("x", "y")], DDEG = extract(DDEG, CellsXY[, c("x", "y")]), MIND = extract(MIND, CellsXY[, c("x", "y")]), SRAD = extract(SRAD, CellsXY[, c("x", "y")])))


# 22. Project model over all of Switzerland and store results as a raster ------------------
#GLM
pred_glm_multi <- predict(model_glm_multi, Projection, type = "response")
project_glm_multi <- cbind(Projection[, c("x", "y")], pred = pred_glm_multi)
pinus_sylvestris_glm <- rasterFromXYZ(project_glm_multi[, c("x", "y","pred")])
#SVM
pred_svm_multi <- predict(model_svm_multi, Projection, type = "response")
project_svm_multi <- cbind(Projection[, c("x", "y")], pred = pred_svm_multi)
pinus_sylvestris_svm <- rasterFromXYZ(project_svm_multi[, c("x", "y","pred")])


# 26. Plot a raster of projected probabilities of occurrence: ----------------------
#GLM
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 0.1, 0.1))
plot(pinus_sylvestris_glm, axes = F, main = "GLM", box = F, col = brewer.pal(9, "PuRd"))
points(occurrences_data$x[index_present], occurrences_data$y[index_present], col = "green4", pch = 3, cex = 0.5)
plot(pinus_sylvestris_glm, axes = F, main = "GLM", box = F, col = brewer.pal(9, "PuRd"))
points(occurrences_data$x[index_absent], occurrences_data$y[index_absent], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))
#SVM
plot(pinus_sylvestris_svm, axes = F, main = "SVM", box = F, col = brewer.pal(9, "PuRd"))
points(occurrences_data$x[index_present], occurrences_data$y[index_present], col = "green4", pch = 3, cex = 0.5)
plot(pinus_sylvestris_svm, axes = F, main = "SVM", box = F, col = brewer.pal(9, "PuRd"))
points(occurrences_data$x[index_absent], occurrences_data$y[index_absent], col = "blue", pch = 3, cex = 0.5)
legend("bottomright", legend = c("present", "absent"), pch = c(3, 3), col = c("green4", "blue"))


# 27. Convert probabilities in presence and absence ----------------------
#GLM
pinus_sylvestris_binary_glm <- pinus_sylvestris_glm > as.numeric(max_TSS_glm[[2]][2,2])
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
plot(pinus_sylvestris_binary_glm, box = F, axes = F, main = "GLM", legend = F, col = c("grey", "green4"))
points(occurrences_data$x[index_present], occurrences_data$y[index_present], col = "black", pch = 3, cex = 0.5)
legend("bottomright", legend = c(" observed occurrence", " observed absence"), col = c("black", "red"), pch = 3, bg = "transparent", bty = "n")
plot(pinus_sylvestris_binary_glm, box = F, axes = F, main = "GLM", legend = F, col = c("grey", "green4"))
points(occurrences_data$x[index_absent], occurrences_data$y[index_absent], col = "red", pch = 3, cex = 0.5)
legend("bottomright", legend = c("predicted occurrence", "predicted absence"), fill = c("green4", "grey"), bg = "transparent", bty = "n")
#SVM
pinus_sylvestris_binary_svm <- pinus_sylvestris_svm > as.numeric(max_TSS_svm[[2]][2,2])
plot(pinus_sylvestris_binary_svm, box = F, axes = F, main = "SVM", legend = F, col = c("grey", "green4"))
points(occurrences_data$x[index_present], occurrences_data$y[index_present], col = "black", pch = 3, cex = 0.5)
legend("bottomright", legend = c(" observed occurrence", " observed absence"), col = c("black", "red"), pch = 3, bg = "transparent", bty = "n")
plot(pinus_sylvestris_binary_svm, box = F, axes = F, main = "SVM", legend = F, col = c("grey", "green4"))
points(occurrences_data$x[index_absent], occurrences_data$y[index_absent], col = "red", pch = 3, cex = 0.5)
legend("bottomright", legend = c("predicted occurrence", "predicted absence"), fill = c("green4", "grey"), bg = "transparent", bty = "n")
