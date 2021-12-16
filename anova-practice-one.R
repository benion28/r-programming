# Load Libraries
pacman::p_load(pacman, caTools, e1071, ggplot2, psych, tidyverse, Metrics)


# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\example-1.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
description <- describe(dataset)


# Total Treatment
tl15 <- c(sum(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 15]))
tl20 <- c(sum(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 20]))
tl25 <- c(sum(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 25]))
tl30 <- c(sum(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 30]))
tl35 <- c(sum(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 35]))

# Means
m15 <- mean(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 15])
m20 <- mean(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 20])
m25 <- mean(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 25])
m30 <- mean(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 30])
m35 <- mean(dataset$Observed.tree.height.cm.[dataset$Treatment.Level... == 35])


# improved data frame
improved_dataset <- data.frame(
  "Treatment-Level"=c(15, 20, 25, 30, 35),
  "Total-Height"=c(tl15, tl20, tl25, tl30, tl35),
  "Means"=c(m15, m20, m25, m30, m35)
  )

# Observations
observations <- dataset$Observed.tree.height.cm.

# Number of Observations
N <- length(observations)

# Number of Replication
n <- length(improved_dataset$Total.Height)

# Number of Treatments
t <- length(improved_dataset$Treatment.Level)

# Grand Total
G2 <- sum(observations)^2

# Sum of X2
x2 <- observations^2
sum_x2 <- sum(x2)

# Total Treatments
# Treatments
t15 <- observations[dataset$Treatment.Level... == 15]
t20 <- observations[dataset$Treatment.Level... == 20]
t25 <- observations[dataset$Treatment.Level... == 25]
t30 <- observations[dataset$Treatment.Level... == 30]
t35 <- observations[dataset$Treatment.Level... == 35]
# Sum of Treatments
sum_t15 <- sum(t15)
sum_t20 <- sum(t20)
sum_t25 <- sum(t25)
sum_t30 <- sum(t30)
sum_t35 <- sum(t35)
# Treatments Squares
sum_T2 <- sum(sum_t15^2, sum_t20^2, sum_t25^2, sum_t30^2, sum_t35^2)

# Total Sum of Squares
SSTO <- sum_x2 - (G2/N)

# Treatments Sum of Squares
SST <- (1/n *(sum_T2)) - (G2/N)

# Sum of Square Errors (Within Groups)
SSE <- SSTO - SST

# Degree of Freedom (Between Treatment Groups)
t_1 <- t - 1

# Degree of Freedom (Within Treatment Groups)
N_t <- N - t

# Mean Square Treatment (Between Groups)
MST <- SST/(t_1)

# Mean Square Error (Within Groups)
MSE <- SSE/(N_t)

# F-Score
F <- MST/MSE

# ANOVA Table
anova_table <- data.frame(
  "Source of Variation"=c("Between Groups", "Within Group(Error)", "Total"),
  "Sum of Squares"=c(round(SST, 2), round(SSE, 2), round(SSTO, 2)),
  "Degree of Freedom"=c(t_1, N_t, t_1+N_t),
  "Mean Square"=c(round(MST, 2), round(MSE, 2), "-"),
  "F"=c(round(F, 2), "-", "-")
  )
