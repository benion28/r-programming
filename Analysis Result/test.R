# Data Splicing (ML)
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)

# Writing Training and Testing Data to File(.csv)
write.csv(
  summary_ag_classes_table, 
  file = "summary_ag_classes_table.csv", 
  quote = FALSE, row.names = TRUE)

write.table(
  species_dist_table, 
  file = "species_dist_table.txt", 
  sep = ",", quote = FALSE, row.names = FALSE)

model_1 <- lm(formula =  CROWN.DIAMETER ~ (a  + b * (DIAMETER)), 
              data = training,
              )

# Define the formula
my_formula <- CROWN.DIAMETER ~ a * DIAMETER^b + c

# Define the initial values for a, b, and c
init_values <- list(a = 1, b = 1, c = 0)

# Fit the model with the customized formula and initial values
model_1 <- nls(formula = CROWN.DIAMETER ~ (a * (DIAMETER^b) + c), 
               data = dataset, 
               start = list(a = init_values$a, b = init_values$b, c = init_values$c))

model_2 <- nls(formula = CROWN.DIAMETER ~ ((a + (b * (DIAMETER^2)))), 
               data = dataset, 
               start = list(a = init_values$a, b = init_values$b))

model_3 <- nls(formula = CROWN.DIAMETER ~ (a + b * (log(DIAMETER))), 
               data = dataset, 
               start = list(a = init_values$a, b = init_values$b))

model_4 <- nls(formula = CROWN.DIAMETER ~ (a + b * (DIAMETER) + c * (DIAMETER^2)), 
               data = dataset, 
               start = list(a = init_values$a, b = init_values$b, c = init_values$c))


# Check the summary of the model
summary(model_3)

# Plot the data and the fitted curve
plot(dataset$DIAMETER, dataset$CROWN.DIAMETER, xlab = "Diameter", ylab = "Crown Diameter")
curve(predict(model, newdata = data.frame(DIAMETER = x)), add = TRUE, col = "red")


set.seed(123)
df <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 1), rnorm(100, mean = 2))
)

# plot density comparison using ggplot
ggplot(df, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF")) +
  theme_classic()

start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh=1.3)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh=1.3)


# ----------------------------- Chapman-Richards ----------------------------- #
# Chapman-Richards Model Fitting
model_chapman <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER))^c), 
                     data = training, start=list(
                       a=start_values_chapman["a"], 
                       b=start_values_chapman["b"], 
                       c=start_values_chapman["c"]))
summary_model_chapman <- summary(model_chapman)
length_model_chapman <- length(summary(model_chapman)$coef[,1])
model_chapman

# Making Predictions
predictions_chapman_training = predict(model_chapman, training)
predictions_chapman_testing = predict(model_chapman, testing)


# ----------------------------- Weibull ----------------------------- #
# Weibull Model Fitting
model_weibull <- nls(formula = HEIGHT ~ (1.3 + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(a=start_values_weibull["a"], 
                                                 b=start_values_weibull["b"], 
                                                 c=start_values_weibull["c"]))
summary_model_weibull <- summary(model_weibull)
length_model_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull

# Making Predictions
predictions_weibull_training = predict(model_weibull, training)
predictions_weibull_testing = predict(model_weibull, testing)

