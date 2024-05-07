# Data Splicing (ML)
split <- sample.split(dataset, SplitRatio = 0.8)
training <- subset(dataset, split==TRUE)
testing <- subset(dataset, split==FALSE)

# Writing Training and Testing Data to File(.csv)
write.csv(
  summary_anova_no_leaves, 
  file = "summary_anova_no_leaves.csv", 
  quote = FALSE, row.names = TRUE)

write.table(
  summary_statistic, 
  file = "summary_variable_classes_table.txt", 
  sep = ",", quote = FALSE, row.names = FALSE)

install.packages(c("betareg", "DirichletReg"))
install.packages("")



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



# Create a sample dataset
your_data_frame <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  variable1 = rep(c("A", "B"), 50),
  variable2 = 1:100
)

# Create a basic scatter plot
ggplot(data = your_data_frame) +
  geom_point(aes(x = x, y = y)) +
  facet_grid(rows = variable1, cols = variable2) +
  labs(title = "Grid of Plots", x = "X", y = "Y") +
  theme_bw()




Available 2- and 3- parameter H-D model functions to be used by function fithd.
Description
Nonlinear functions for modeling tree height on diameter. Usually called using fithd.

Usage
HDnaslund(d, a, b, bh=1.3) 
HDcurtis(d, a, b, bh=1.3) 
HDmichailoff(d, a, b, bh=1.3) 
HDmeyer(d, a, b, bh=1.3) 
HDpower(d, a, b, bh=1.3)
HDnaslund2(d, a, b, bh=1.3)
HDnaslund3(d, a, b, bh=1.3)
HDnaslund4(d, a, b, bh=1.3)
HDmicment(d, a, b, bh=1.3) 
HDmicment2(d, a, b, bh=1.3) 
HDwykoff(d, a, b, bh=1.3) 


HDprodan(d, a, b, c, bh=1.3) 
HDlogistic(d, a, b, c, bh=1.3) 
HDrichards(d, a, b, c, bh=1.3) 
HDweibull(d, a, b, c, bh=1.3) 
HDgomperz(d, a, b, c, bh=1.3) 
HDsibbesen(d, a, b, c, bh=1.3) 
HDkorf(d, a, b, c, bh=1.3) 
HDratkowsky(d, a, b, c, bh=1.3) 
HDhossfeldIV(d, a, b, c, bh=1.3)

startHDnaslund(d, h, bh=1.3) 
startHDcurtis(d, h, bh=1.3) 
startHDmichailoff(d, h, bh=1.3) 
startHDmeyer(d, h, bh=1.3) 
startHDpower(d, h, bh=1.3)
startHDnaslund2(d, h, bh=1.3) 
startHDnaslund3(d, h, bh=1.3) 
startHDnaslund4(d, h, bh=1.3) 
startHDmicment(d, h, bh=1.3) 
startHDmicment2(d, h, bh=1.3) 
startHDwykoff(d, h, bh=1.3) 

startHDprodan(d, h, bh=1.3) 
startHDlogistic(d, h, bh=1.3) 
startHDrichards(d, h, bh=1.3, b=0.04) 
startHDweibull(d, h, bh=1.3) 
startHDgomperz(d, h, bh=1.3) 
startHDsibbesen(d, h, bh=1.3, a=0.5) 
startHDkorf(d, h, bh=1.3) 
startHDratkowsky(d, h, bh=1.3, c=5) 
startHDhossfeldIV(d, h, bh=1.3, c=5)
Arguments
d	
A vector of tree diameters, usually in cm

h	
A vector of tree heights, usually in m. The observed heights should be always above or equal to bh.

a, b, c	
Parameters a, b (and c for 3- parameter functions) of the applied function. See details for expressions of different functions.

bh	
The applied height for the measurement of tree diameter (so called breast height). Of the same unit as h.

Details
The available 2- parameter functions are

Naslund: h(d) = bh + d^2/(a + b d)^2

Curtis: h(d) = bh + a (d/(1 + d))^b

Michailoff: bh + a e^(-b d^(-1))

Meyer: h(d) = bh + a (1-exp(-b d))

Power: h(d) = bh + a d^b

Naslund2: h(d) = bh + d^2/(a + exp(b) d)^2

Naslund3: h(d) = bh + d^2/(exp(a) + b d)^2

Naslund4: h(d) = bh + d^2/(exp(a) + exp(b) * d)^2

Michaelis-Menten: h(d) = bh + a d/(b + d)

Michaelis-Menten2: h(d) = bh + d/(a + b * d)

Wykoff: h(d) = bh + exp(a + b/(d + 1))

The available 3- parameter functions are

Prodan: h(d) = bh + d^2/(a + bd + c d^2)

Logistic: h(d) = bh + a/(1 + b exp(-c d))

Chapman-Richards: h(d) = bh + a (1 - exp(-bd))^c

Weibull: h(d) = bh + a (1 - exp(-b d^c))

Gomperz: h(d) = bh + a exp(-b exp(-c d))

Sibbesen: h(d) = bh + a d^(b d^(-c))

Korf: h(d) = bh + a exp(-b d^(-c))

Ratkowsky: h(d) = bh + a exp(-b/(d + c))

Hossfeld IV: h(d) = bh + a/(1 + 1/(b*d^c))

For each model, two functions are provided: one computing the value of the H-D model for given diameters using given values of parameters a, b (and c), and another returning the initial guesses of a, b (and c) for given h-d data.

The initial guesses are in most cases computed by fitting a linearized version of the model into the provided h-d data using lm. For some 3- parameter versions, no straightforward linearization is possible and one of the parameters is set to a fixed sensible constant. Those values can be seen as additional arguments in the corresponding startHD - functions. Details can be seen directly from the function definitions.

The user can define her own functions to be used with fithd. The case-sensitive naming of the functions should follow exactly the naming convention shown above. In addition, the names of the of arguments, as well as their order, should be the same as in the functions above.

The models are named according to references in

Zeide, B. 1993. Analysis of growth equations. Forest Science 39(3):594-616. doi: 10.1093/forestscience/39.3.594

Huang, S., Titus, S.J., and Wiens, D.P. 1992. Comparison of nonlinear height-diameter functiond for major Alberta tree species. Can J. For. Res. 22: 1297-1304. doi: 10.1139/x92-172

Suggestions on naming and references on the functions are welcome.

Value
For functions HDxxx, a vector of tree heights corresponding diameters d is returned. For functions startHDxxx, a named vector of initial estimates of a, b and (c).

Author(s)
Lauri Mehtatalo <lauri.mehtatalo@uef.fi>
  
  References
Mehtatalo, L., Gregoire, T.G., and de Miguel, S. Modeling Height-diameter curves for height prediction. Canadian Journal of Forest Research, 45(7): 826-837, doi: 10.1139/cjfr-2015-0054

Examples
data(spati)
theta<-startHDnaslund(spati$d,spati$h)
plot(spati$d,spati$h)
d<-seq(0,50)
lines(d,HDnaslund(d,a=theta[1],b=theta[2]),col="red",lwd=5)




#y Model 1 -- Chapman (1.3 + a (1 - exp(-bd))^c)
#y Model 2 -- Weibull (1.3 + + a (1 - exp(-b d^c)))
#n Model 3 -- Gomperz (1.3 + + a exp(-b exp(-c d)))
#y Model 4 -- Logistic (1.3 + a/(1 + b exp(-c d)))
#y Model 5 -- Ratkowsky (1.3 + + a exp(-b/(d + c)))
# Model 6 -- Chapman (1.3 + a (1 - exp(-bd))^c)

# Model 1 -- Chapman
# Model 2 -- Weibull
# Model 3 -- Power
# Model 4 -- Korf
# Model 5 -- Prodan
# Model 6 -- Logistic 
# Model 7 -- Hossfeld IV
# Model 8 -- Ratkowsky



# Model 1 --- (1.3 + (a * (1 - exp(-b * DIAMETER))^c))
# Model 2 --- (1.3 + (a * (1 - exp(-b * (DIAMETER)^c))))
# Model 3 --- (1.3 + (a * (exp(-b * (exp(-c * DIAMETER))))))
# Model 4 --- (1.3 + (a/(1 + (b * exp(-c * DIAMETER)))))
# Model 5 --- (1.3 + (exp(a + (b/(DIAMETER + c)))))
# Model 6 --- (exp(a + (b * (log(DIAMETER)))))
# Model 7 --- (1.3 + (DIAMETER/(a + (b * DIAMETER)))^c)


https://doi.org/10.1093/forestscience/39.3.594
https://doi.org/10.1139/x92-172

