#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, ggplot2, tidyverse, dplyr, psych, agricolae, car)

# Set Working Directory
setwd("C://Benion//Benion Programmings//R//Analysis Result//10")

# Import data set
dataset <-  read.csv("full-data-5.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)


#------------------------------ Graphical Representation of Data --------------------------------#
# Box plot  suggests a difference of means
names(dataset)
boxplot(D ~ TRT, data = dataset, ylab = " Seedling diameter", xlab = "Treatments", col = "gray") 
boxplot(H ~ TRT, data = dataset, ylab = " Seedling height", xlab = "Treatments", col = "gray")
boxplot(NL ~ TRT, data = dataset, ylab = "Number of leaves", xlab = "Treatments", col = "gray")

#------------------------------ Summary Statistics --------------------------------#
t1 <- dataset %>% filter(TRT == "T1")
t2 <- dataset %>% filter(TRT == "T2")
t3 <- dataset %>% filter(TRT == "T3")
# Diameter
summary_statistic_diameter <- data.frame(
  "T1" = c(round(mean(t1$D), 1), round(sd(t1$D), 1), round(min(t1$D), 1), round(max(t1$D), 1)),
  "T2" = c(round(mean(t2$D), 1), round(sd(t2$D), 1), round(min(t2$D), 1), round(max(t2$D), 1)),
  "T3" = c(round(mean(t3$D), 1), round(sd(t3$D), 1), round(min(t3$D), 1), round(max(t3$D), 1))
)
rownames(summary_statistic_diameter) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")
# Height
summary_statistic_height <- data.frame(
  "T1" = c(round(mean(t1$H), 1), round(sd(t1$H), 1), round(min(t1$H), 1), round(max(t1$H), 1)),
  "T2" = c(round(mean(t2$H), 1), round(sd(t2$H), 1), round(min(t2$H), 1), round(max(t2$H), 1)),
  "T3" = c(round(mean(t3$H), 1), round(sd(t3$H), 1), round(min(t3$H), 1), round(max(t3$H), 1))
)
rownames(summary_statistic_height) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")
# No of Leaves
summary_statistic_no_leaves <- data.frame(
  "T1" = c(round(mean(t1$NL), 1), round(sd(t1$NL), 1), min(t1$NL), max(t1$NL)),
  "T2" = c(round(mean(t2$NL), 1), round(sd(t2$NL), 1), min(t2$NL), max(t2$NL)),
  "T3" = c(round(mean(t3$NL), 1), round(sd(t3$NL), 1), min(t3$NL), max(t3$NL))
)
rownames(summary_statistic_no_leaves) <- c("Mean", "Standard Deviation", "Minimum", "Maximun")

#------------------------------ Shapiro Wilks test MOR --------------------------------#
names(dataset)
shapiro_diameter <- shapiro.test(dataset$D)
shapiro_height <- shapiro.test(dataset$H)
shapiro_no_leaves <- shapiro.test(dataset$NL)

# Shapiro Table
shapiro_table <- data.frame(
  "Diameter" = c(shapiro_diameter[["statistic"]][["W"]], shapiro_diameter$p.value),
  "Height" = c(shapiro_height[["statistic"]][["W"]], shapiro_height$p.value),
  "No.Leaves" = c(shapiro_no_leaves[["statistic"]][["W"]], shapiro_no_leaves$p.value)
)
rownames(shapiro_table) <- c("W.value", "P.value")


#------------------------ Homogeneity of variances ---------------------------#
# Check assumption of homoscedasticity 
levene_diameter <- leveneTest(D ~ TRT, data = dataset)
levene_height <- leveneTest(H ~ TRT, data = dataset)
levene_no_leaves <- leveneTest(NL ~ TRT, data = dataset)


#------------------------ Anova test ---------------------------#
anova_diameter <- aov(D ~ TRT, data = dataset)
anova_height <-  aov(H ~ TRT, data = dataset)
anova_no_leaves <- aov(NL ~ TRT, data = dataset)
summary_anova_diameter <- summary(anova_diameter)[[1]]  # SIG DIFF AMONG THE MEANS
summary_anova_height <- summary(anova_height)[[1]]  # NO SIG DIFF AMONG THE MEANS
summary_anova_no_leaves <- summary(anova_no_leaves)[[1]]  # NO SIG DIFF AMONG THE MEANS


#------------------------ Post HOC ---------------------------#
# Tukey multiple comparisons of means
tukey_diameter <- TukeyHSD(anova_diameter)
tukey_diameter_table <- data.frame(tukey_diameter$TRT)
hsd_test <- HSD.test(anova_diameter, "TRT")
hsd_test_means <- data.frame(hsd_test$means)
hsd_test_groups <- data.frame(hsd_test$groups)


# Unload Packages
p_unload(all)

# Remove list from Global Environment
rm(list=ls())

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
