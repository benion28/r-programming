# Load Libraries
pacman::p_load(pacman, ggplot2, gridExtra, systemfit, reshape, betareg)

# set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R")

# Using pander with knitr
knitr::opts_chunk$set(echo = TRUE)

# Import data set
dataset <-  read.csv("./Personal/AGBC-V/dataset.csv", h = T)
pander(head(dataset), caption = 'Dataset of Different Tree Species')

## ------------------------------ PART 1 (AGB Models) ------------------------------------------ ##

# Plotting the data
plot_1 = ggplot(data=dataset, aes(x=DIAMETER, y=AGB.kg)) + geom_point() +
  xlab("DBH (cm)") + ylab("Total aboveground biomass (kg)") +
  theme_bw()
plot_1

plot_2 = ggplot(data=dataset, aes(x=HEIGHT, y=AGB.kg)) + geom_point() +
  xlab("Total Tree Height (m)") + ylab("Total aboveground biomass (kg)") +
  theme_bw()
plot_2

# Arrange Plots in Grids
grid.arrange(plot_1, plot_2)
grid.arrange(plot_1, plot_2, nrow=1)


# Fit Models
model_1 = lm(AGB.kg ~ DIAMETER + HEIGHT, data=dataset)
summary(model_1)
model_2 = lm(log(AGB.kg) ~ log(DIAMETER) + log(HEIGHT), data=dataset)
summary(model_2)


# Residuals
residuals_1 = ggplot() +
  aes(fitted(model_1), residuals(model_1)) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot") +
  theme(plot.title = element_text(hjust = 0.5))
residuals_1

residuals_2 = ggplot() +
  aes(fitted(model_2), residuals(model_2)) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot - Transformes Model") +
  theme(plot.title = element_text(hjust = 0.5))
residuals_2

# Grid Residuals Plots
grid.arrange(residuals_1, residuals_2)

# R.squared
summary(model_1)$adj.r.squared #un-transformed model
summary(model_2)$adj.r.squared #log-transformed model




## ----------------------------PART 2 (Component Models) ------------------------------------------ ##
# Plotting the data
stem = ggplot(data=dataset, aes(x=DIAMETER, y=TREE.STEM)) + geom_point() +
  xlab("DBH (cm)") + ylab("Stemwood biomass (kg)") +
  theme_bw()

bark = ggplot(data=dataset, aes(x=DIAMETER, y=TREE.BARK)) + geom_point() +
  xlab("DBH (cm)") + ylab("Stembark biomass (kg)") +
  theme_bw()

branch = ggplot(data=dataset, aes(x=DIAMETER, y=TREE.BRANCHES)) + geom_point() +
  xlab("DBH (cm)") + ylab("Branchwood biomass (kg)") +
  theme_bw()

foliage_biomass = ggplot(data=dataset, aes(x=DIAMETER, y=FOLIAGE.BIOMASS)) + geom_point() +
  xlab("DBH (cm)") + ylab("Foliage biomass (kg)") +
  theme_bw()

grid.arrange(stem, bark, branch, foliage_biomass)

# OLS fit
log_stem = log(TREE.STEM) ~ log(DIAMETER) + log(HEIGHT)
log_bark = log(TREE.BARK) ~ log(DIAMETER) + log(HEIGHT)
log_branch = log(TREE.BRANCHES) ~ log(DIAMETER) + log(HEIGHT)
log_foliage_biomass = log(FOLIAGE.BIOMASS) ~ log(DIAMETER) + log(HEIGHT)
system = list(Stemwood.Model = log_stem,
              Stembark.Model = log_bark,
              Branch.Model = log_branch,
              Foliage.Model = log_foliage_biomass)

fit_OLS = systemfit(system, "OLS", data = dataset)
summary(fit_OLS)

fit_foliage_biomass = lm(log(FOLIAGE.BIOMASS) ~ log(DIAMETER) + log(LIVE.CROWN.LEAVES), data = dataset)
summary(fit_foliage_biomass)

fit_sur = systemfit(system, "SUR", data = dataset)
summary(fit_sur)


## ------------------------------ PART 3 (Beta Regression) ------------------------------------------ ##
data_frame <- dataset[c(1, 5, 4, 21, 25, 22, 23, 24, 12)]
names(data_frame) <- c("id", "dbh", "tht", "lcl", "fol", "bch", "brk", "stm", "tab")
data_frame <- melt(data_frame, id.vars=c("id", "dbh", "tht", "lcl", "tab"))
colnames(data_frame)[6:7] <- c("comp", "mass")
data_frame$percent <- data_frame$mass/data_frame$tab
names(data_frame) <- c("S.N", "DIAMETER", "HEIGHT", "LIVE.CROWN.LEAVES", "AGB.kg", "COMPOSITION", "MASS", "PERCENT")
head(data_frame)

# Beta Regression ----------  (Foliage)  ----------------------------
foliage <- subset(data_frame, COMPOSITION == "fol")
# Linear Model
linear_foliage <- lm(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = foliage)
summary(linear_foliage)
# Beta Model
beta_foliage <- betareg(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = foliage)
summary(beta_foliage)
# Compare fitted lines
foliage_data <- data.frame(LIVE.CROWN.LEAVES=mean(foliage$LIVE.CROWN.LEAVES), DIAMETER=foliage$DIAMETER, PERCENT=foliage$PERCENT)
# Predict
foliage_data$PREDICT.1 <- predict(linear_foliage, newdata=foliage_data)
foliage_data$PREDICT.2 <- predict(beta_foliage, newdata=foliage_data)
# Visualize plot
ggplot(foliage_data, aes(x=DIAMETER, y=PERCENT)) + geom_point() +
  geom_line(aes(x=DIAMETER, y=PREDICT.1, color="OLS"), lwd=1) +
  geom_line(aes(x=DIAMETER, y=PREDICT.2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of Foliage Biomass")

# Beta Regression ----------  (Stem Bark)  ----------------------------
stem_bark <- subset(data_frame, COMPOSITION == "brk")
# Linear Model
linear_bark <- lm(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_bark)
summary(linear_bark)
# Beta Model
beta_bark <- betareg(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_bark)
summary(beta_bark)
# Compare fitted lines
bark_data <- data.frame(LIVE.CROWN.LEAVES=mean(stem_bark$LIVE.CROWN.LEAVES), DIAMETER=stem_bark$DIAMETER, PERCENT=stem_bark$PERCENT)
# Predict
bark_data$PREDICT.1 <- predict(linear_bark, newdata=bark_data)
bark_data$PREDICT.2 <- predict(beta_bark, newdata=bark_data)
# Visualize plot
ggplot(bark_data, aes(x=LIVE.CROWN.LEAVES, y=PERCENT)) + geom_point() +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.1, color="OLS"), lwd=1) +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of Stembark Biomass")

# Beta Regression ----------  (Branch)  ----------------------------
stem_branch <- subset(data_frame, COMPOSITION == "bch")
# Linear Model
linear_branch <- lm(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_branch)
summary(linear_branch)
# Beta Model
beta_branch <- betareg(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_branch)
summary(beta_branch)
# Compare fitted lines
branch_data <- data.frame(LIVE.CROWN.LEAVES=mean(stem_branch$LIVE.CROWN.LEAVES), DIAMETER=stem_branch$DIAMETER, PERCENT=stem_branch$PERCENT)
# Predict
branch_data$PREDICT.1 <- predict(linear_branch, newdata=branch_data)
branch_data$PREDICT.2 <- predict(beta_branch, newdata=branch_data)
# Visualize plot
ggplot(branch_data, aes(x=LIVE.CROWN.LEAVES, y=PERCENT)) + geom_point() +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.1, color="OLS"), lwd=1) +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of Branch Biomass")

# Beta Regression ----------  (Stemwood)  ----------------------------
stem_wood <- subset(data_frame, COMPOSITION == "stm")
# Linear Model
linear_wood <- lm(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_wood)
summary(linear_wood)
# Beta Model
beta_wood <- betareg(PERCENT ~ DIAMETER + LIVE.CROWN.LEAVES, data = stem_wood)
summary(beta_wood)
# Compare fitted lines
wood_data <- data.frame(LIVE.CROWN.LEAVES=mean(stem_wood$LIVE.CROWN.LEAVES), DIAMETER=stem_wood$DIAMETER, PERCENT=stem_wood$PERCENT)
# Predict
wood_data$PREDICT.1 <- predict(linear_wood, newdata=wood_data)
wood_data$PREDICT.2 <- predict(beta_wood, newdata=wood_data)
# Visualize plot
ggplot(wood_data, aes(x=LIVE.CROWN.LEAVES, y=PERCENT)) + geom_point() +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.1, color="OLS"), lwd=1) +
  geom_line(aes(x=LIVE.CROWN.LEAVES, y=PREDICT.2, color="Beta"), lwd=1) +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Proportion of Stemwood Biomass")
