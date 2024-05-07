#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych, agricolae, car)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//37 Anova")

# Import data set
dataset <-  read.csv("full-data-15.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)


#------------------------------ Graphical Representation of Data --------------------------------#
# Box plot  suggests a difference of means
names(dataset)
boxplot(DIAMETER ~ TREATMENT, data = dataset, ylab = " Seedling diameter", xlab = "Treatments", col = "gray") 
boxplot(HEIGHT ~ TREATMENT, data = dataset, ylab = " Seedling height", xlab = "Treatments", col = "gray")
boxplot(NO.LEAVES ~ TREATMENT, data = dataset, ylab = "Number of leaves", xlab = "Treatments", col = "gray")

#------------------------------ Summary Statistics --------------------------------#
treatment_null <- dataset %>% filter(TREATMENT == "TN")
treatment_1 <- dataset %>% filter(TREATMENT == "T1")
treatment_2 <- dataset %>% filter(TREATMENT == "T2")

# Diameter
summary_statistic_diameter <- data.frame(
  "TREATMENT.NULL" = c(round(mean(treatment_null$DIAMETER), 1), round(sd(treatment_null$DIAMETER), 1), round(min(treatment_null$DIAMETER), 1), round(max(treatment_null$DIAMETER), 1)),
  "TREATMENT.1" = c(round(mean(treatment_1$DIAMETER), 1), round(sd(treatment_1$DIAMETER), 1), round(min(treatment_1$DIAMETER), 1), round(max(treatment_1$DIAMETER), 1)),
  "TREATMENT.2" = c(round(mean(treatment_2$DIAMETER), 1), round(sd(treatment_2$DIAMETER), 1), round(min(treatment_2$DIAMETER), 1), round(max(treatment_2$DIAMETER), 1))
)
rownames(summary_statistic_diameter) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")

# Height
summary_statistic_height <- data.frame(
  "TREATMENT.NULL" = c(round(mean(treatment_null$HEIGHT), 1), round(sd(treatment_null$HEIGHT), 1), round(min(treatment_null$HEIGHT), 1), round(max(treatment_null$HEIGHT), 1)),
  "TREATMENT.1" = c(round(mean(treatment_1$HEIGHT), 1), round(sd(treatment_1$HEIGHT), 1), round(min(treatment_1$HEIGHT), 1), round(max(treatment_1$HEIGHT), 1)),
  "TREATMENT.2" = c(round(mean(treatment_2$HEIGHT), 1), round(sd(treatment_2$HEIGHT), 1), round(min(treatment_2$HEIGHT), 1), round(max(treatment_2$HEIGHT), 1))
)
rownames(summary_statistic_height) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")

# No of Leaves
summary_statistic_no_leaves <- data.frame(
  "TREATMENT.NULL" = c(round(mean(treatment_null$NO.LEAVES), 1), round(sd(treatment_null$NO.LEAVES), 1), round(min(treatment_null$NO.LEAVES), 1), round(max(treatment_null$NO.LEAVES), 1)),
  "TREATMENT.1" = c(round(mean(treatment_1$NO.LEAVES), 1), round(sd(treatment_1$NO.LEAVES), 1), round(min(treatment_1$NO.LEAVES), 1), round(max(treatment_1$NO.LEAVES), 1)),
  "TREATMENT.2" = c(round(mean(treatment_2$NO.LEAVES), 1), round(sd(treatment_2$NO.LEAVES), 1), round(min(treatment_2$NO.LEAVES), 1), round(max(treatment_2$NO.LEAVES), 1))
)
rownames(summary_statistic_no_leaves) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")

#------------------------------ Shapiro Wilks test MOR --------------------------------#
names(dataset)
shapiro_diameter <- shapiro.test(dataset$DIAMETER)
shapiro_height <- shapiro.test(dataset$HEIGHT)
shapiro_no_leaves <- shapiro.test(dataset$NO.LEAVES)

# Shapiro Table
shapiro_table <- data.frame(
  "Diameter" = c(shapiro_diameter[["statistic"]][["W"]], shapiro_diameter$p.value),
  "Height" = c(shapiro_height[["statistic"]][["W"]], shapiro_height$p.value),
  "No.Leaves" = c(shapiro_no_leaves[["statistic"]][["W"]], shapiro_no_leaves$p.value)
)
rownames(shapiro_table) <- c("W.value", "P.value")


#------------------------ Homogeneity of variances ---------------------------#
# Check assumption of homoscedasticity 
levene_diameter <- leveneTest(DIAMETER ~ TREATMENT, data = dataset)
levene_height <- leveneTest(HEIGHT ~ TREATMENT, data = dataset)
levene_no_leaves <- leveneTest(NO.LEAVES ~ TREATMENT, data = dataset)


#------------------------ Anova test ---------------------------#
anova_diameter <- aov(DIAMETER ~ TREATMENT, data = dataset)
anova_height <-  aov(HEIGHT ~ TREATMENT, data = dataset)
anova_no_leaves <- aov(NO.LEAVES ~ TREATMENT, data = dataset)
summary_anova_diameter <- summary(anova_diameter)[[1]]  # NO SIG DIFF AMONG THE MEANS
summary_anova_height <- summary(anova_height)[[1]]  # NO SIG DIFF AMONG THE MEANS
summary_anova_no_leaves <- summary(anova_no_leaves)[[1]]  # NO SIG DIFF AMONG THE MEANS
# F-Critical Value
f_critical <- qf(1 - 0.05, summary(anova_diameter)[[1]][1, 1], summary(anova_diameter)[[1]][2, 1])


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
