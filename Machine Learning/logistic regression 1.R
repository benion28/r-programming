data <- read.csv(file = "C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\suv_data.csv", head = TRUE, sep = ",")

library(caTools)
split <- sample.split(data, SplitRatio = 0.8)
training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")

# Binomial = It can take only two values e.g "Yes or No"
# glm = Logistic Regression
model <- glm(Purchased ~ ., training, family = "binomial")
summary(model)

# Null Deviance is when you're only using your intercept or none of your dependent variable
# Residual Deviance is when you include your independent variables in your model
# Model Optimization = removing of the insignificant fields which is "Gender and User.ID" in our case

model_adj <- glm(Purchased ~ . - Gender - User.ID, training, family = "binomial")
summary(model_adj)

# making predictions
response <- predict(model_adj, testing, type = "response")

# Accuracy calculations
confusion_matrix <- table(Actual_Value = testing$Purchased, Predicted_Value = response > 0.5)
accuracy <- (confusion_matrix[1, 1] + confusion_matrix[2, 2])/sum(confusion_matrix)

# You can increase the accuracy by trying different thresholds e.g. "0.5, 0.3"
# or you can simply calculate the threshold

library(ROCR)
response_training <- predict(model, training, type = "response")
ROCRPred <- prediction(response_training, training$Purchased)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

# From the graph we want to minimize the false positive rate so we will go with the "0.3" threshold

confusion_matrix_adj <- table(Actual_Value = testing$Purchased, Predicted_Value = response > 0.3)
accuracy_adj <- (confusion_matrix_adj[1, 1] + confusion_matrix_adj[2, 2])/sum(confusion_matrix_adj)
