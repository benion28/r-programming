# Data Splicing (ML)
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)

# Writing Training and Testing Data to File(.csv)
write.csv(
  anderson_darling_table, 
  file = "anderson_darling_table-3.csv", 
  quote = FALSE, row.names = TRUE)

write.table(
  anderson_darling_table, 
  file = "anderson_darling_table-3.txt", 
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

