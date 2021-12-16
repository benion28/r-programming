# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics)


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\HDData.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
description <- describe(dataset)
unique_species_dataset <- data.frame("Unique Species"=unique(dataset$SPECIES))


# Data Splicing (ML)
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)


# -----------------------------SVR Model-------------------- #
# SVR Model Fitting
model_svr <-  svm(formula =H ~ D, data = training, type = "eps-regression")
summary(model_svr)
summary_svr_model <- summary(model_svr)
# Making Predictions
predictions_svr = predict(model_svr, testing)


# -------------------------Linear Model--------------------------- #
# Linear Model Fitting
model_lm <- lm(formula = H ~ D, data = training)
summary(model_lm)
summary_lm <- summary(model_lm)
# Making Predictions
predictions_lm = predict(model_lm, testing)


# -------------------------NLS Model--------------------------- #
# Generating Parameters 
nls_lm_parameters  <- lm(log10(H) ~ log10(D), data = training)
summary_nls_lm_parameters <- summary(nls_lm_parameters)
summary(nls_lm_parameters)
# Intercept and Slope
a_intercept <- nls_lm_parameters[["coefficients"]][["(Intercept)"]]
b_slope <- nls_lm_parameters[["coefficients"]][["log10(D)"]]
# NLS Model Fitting
model_nls <- nls(formula= 1.3 + H ~ a*(D^b), data = training, start=list(a=exp(a_intercept), b=b_slope))
summary_nls <- summary(model_nls)
length_nls <- length(summary(model_nls)$coef[,1])
# Making Predictions
predictions_nls = predict(model_nls, testing)


# Manual Predictions
predictor_variable <- data.frame(D = 25.9)
predict_lm <- predict(model_lm, predictor_variable)
predict_svr <- predict(model_svr, predictor_variable)
predict_nls <- predict(model_nls, predictor_variable)
# Predict Table
predict_table <- data.frame("D" = predictor_variable$D, "LM" = round(predict_lm, 1), "SVR" = round(predict_svr, 1), "NLS" = round(predict_nls, 1))


# Root Mean Squared Errors
RMSE_svr <- rmse(testing$H, predictions_svr)
RMSE_lm <- rmse(testing$H, predictions_lm)
RMSE_nls <- rmse(testing$H, predictions_nls)


# Calculate RSCORE
RSQUARED_svr <- cor(testing$H, predictions_svr) # Correlation Coefficient
RSQUARED_lm <- cor(testing$H, predictions_lm) # Correlation Coefficient
RSQUARED_nls <- cor(testing$H, predictions_nls) # Correlation Coefficient
r_squared_lm = summary_lm[["r.squared"]]
adj_r_squared_lm = summary_lm[["adj.r.squared"]]


# Mean Absolute Percent Error (MAPE)
MAPE_svr <- mape(testing$H, predictions_svr)
MAPE_lm <- mape(testing$H, predictions_lm)
MAPE_nls <- mape(testing$H, predictions_nls)


# BIAS
bias_svr <- bias(testing$H, predictions_svr)
bias_lm <- bias(testing$H, predictions_lm)
bias_nls <- bias(testing$H, predictions_nls)


# Mean Absolute Error (MAE)
MAE_svr <- mae(testing$H, predictions_svr)
MAE_lm <- mae(testing$H, predictions_lm)
MAE_nls <- mae(testing$H, predictions_nls)


# Calculate AIC
AIC_lm <- AIC(model_lm)
AIC_nls <- AIC(model_nls)


# calculate BIC
BIC_lm <- BIC(model_lm)
BIC_nls <- BIC(model_nls)


# Model Comparison Frame
prediction_frame <- data.frame(
  "Real Values"=testing$H, 
  "SVR_Predictions"=round(predictions_svr, 1), 
  "LM_Predictions"=round(predictions_lm, 1), 
  "NLS_Predictions"=round(predictions_nls, 1)
  )

# Accuracy
# Manual
accuracy_m_svr <- 100 - (mean(abs((testing$H - predictions_svr)/testing$H)) * 100)
accuracy_m_lm <- 100 - (mean(abs((testing$H - predictions_lm)/testing$H)) * 100)
accuracy_m_nls <- 100 - (mean(abs((testing$H - predictions_nls)/testing$H)) * 100)
# Package
accuracy_svr <- accuracy(testing$H, prediction_frame$SVR_Predictions)
accuracy_lm <- accuracy(testing$H, prediction_frame$LM_Predictions)
accuracy_nls <- accuracy(testing$H, prediction_frame$NLS_Predictions)

# Summary Table
summary_table <- data.frame(
  "RMSE"=c(round(RMSE_svr, 2), round(RMSE_lm, 2), round(RMSE_nls, 2)),
  "RSQUARED"=c(round(RSQUARED_svr, 2), round(RSQUARED_lm, 2), round(RSQUARED_nls, 2)),
  "Accuracy(100%)"=c(round(accuracy_m_svr, 1), round(accuracy_m_lm, 1), round(accuracy_m_nls, 1)),
  "MAPE"=c(round(MAPE_svr, 4), round(MAPE_lm, 4), round(MAPE_nls, 4)),
  "BIAS"=c(round(bias_svr, 2), round(bias_lm, 2), round(bias_nls, 2)),
  "MAE"=c(round(MAE_svr, 2), round(MAE_lm, 2), round(MAE_nls, 2)),
  "AIC"=c("-", round(AIC_lm, 2), round(AIC_nls, 2)),
  "BIC"=c("-", round(BIC_lm, 2), round(BIC_nls, 2)),
  "Accuracy2(100%)"=c(accuracy_svr * 100, accuracy_lm * 100, accuracy_nls * 100)
  )

rownames(summary_table) <- c("SVR", "LM", "NLS")

# Scattered Plot
ggplot() +
  geom_point(aes(x = testing$D, y = testing$H, colour = "Points")) +
  geom_line(aes(x = testing$D, y = predictions_svr, colour = "SVR")) +
  geom_line(aes(x = testing$D, y = predictions_lm, colour = "LM")) +
  geom_line(aes(x = testing$D, y = predictions_nls, colour = "NLS")) +
  labs(x = "Diameter", y = "Height", title = "Tree Height - Diameter Model Line", color= "Legend") +
  theme(legend.position = c(0.95, 0.10), legend.justification = c("right", "bottom")) +
  scale_color_manual(values = c("Points"="red", "SVR"="blue", "LM"="green", "NLS"="black")) 


# -------------------------------Export Tables---------------------------- #
# Predict Table
write.table(predict_table, file = "exported-tables/predict_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(predict_table, file = "exported-tables/predict_table.csv", quote = FALSE, row.names = TRUE)
# Training Table
write.table(training, file = "exported-tables/training_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(training, file = "exported-tables/training_table.csv", quote = FALSE, row.names = TRUE)
# Testing Table
write.table(testing, file = "exported-tables/testing_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(testing, file = "exported-tables/testing_table.csv", quote = FALSE, row.names = TRUE)
# Summary Table
write.table(summary_table, file = "exported-tables/summary_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(summary_table, file = "exported-tables/summary_table.csv", quote = FALSE, row.names = TRUE)
# Prediction Table
write.table(prediction_frame, file = "exported-tables/prediction_table.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.csv(prediction_frame, file = "exported-tables/prediction_table.csv", quote = FALSE, row.names = TRUE)


ob <- prediction_frame$Real.Values
pre <- prediction_frame$SVR_Predictions
mob <- mean(ob)
ob_pre <- (ob - pre)^2
ob_mob <- (ob - mob)^2
sum_ob_pre <- sum(ob_pre)
sum_ob_mob <- sum(ob_mob)
R2 <- 1 - (sum_ob_pre/sum_ob_mob)


# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
