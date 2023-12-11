#----------------------------------- START ----------------------------#

# Load Libraries
pacman::p_load(pacman, psych, tidyverse, dplyr)


# Set Working Directory
setwd("C://Benion//Benion Programmings//R//School Result")


# Import data set
dataset <-  read.csv("result-data.csv", head=TRUE, sep=",")

head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)

# Select Classes
sss1 <- filter(dataset, dataset$CLASS == "SSS 1")
sss2 <- filter(dataset, dataset$CLASS == "SSS 2")
sss3 <- filter(dataset, dataset$CLASS == "SSS 3")

# Select Subjects
sss1_further_maths <- filter(sss1, sss1$SUBJECT == "FURTHER MATHEMATICS")
sss2_further_maths <- filter(sss2, sss2$SUBJECT == "FURTHER MATHEMATICS")
sss3_further_maths <- filter(sss3, sss3$SUBJECT == "FURTHER MATHEMATICS")
sss1_geography <- filter(sss1, sss1$SUBJECT == "GEOGRAPHY")
sss2_geography <- filter(sss2, sss2$SUBJECT == "GEOGRAPHY")
sss3_geography <- filter(sss3, sss3$SUBJECT == "GEOGRAPHY")

# SSS 1 Classes
sss1_1_39 <- sss1 %>% filter(SCORE >= 1 & SCORE <= 39)
sss1_40_44 <- sss1 %>% filter(SCORE >= 40 & SCORE <= 44)
sss1_45_49 <- sss1 %>% filter(SCORE >= 45 & SCORE <= 49)
sss1_50_59 <- sss1 %>% filter(SCORE >= 50 & SCORE <= 59)
sss1_60_69 <- sss1 %>% filter(SCORE >= 60 & SCORE <= 69)
sss1_70_79 <- sss1 %>% filter(SCORE >= 70 & SCORE <= 79)
sss1_80 <- sss1 %>% filter(SCORE >= 80)

# SSS 2 Classes
sss2_1_39 <- sss2 %>% filter(SCORE >= 1 & SCORE <= 39)
sss2_40_44 <- sss2 %>% filter(SCORE >= 40 & SCORE <= 44)
sss2_45_49 <- sss2 %>% filter(SCORE >= 45 & SCORE <= 49)
sss2_50_59 <- sss2 %>% filter(SCORE >= 50 & SCORE <= 59)
sss2_60_69 <- sss2 %>% filter(SCORE >= 60 & SCORE <= 69)
sss2_70_79 <- sss2 %>% filter(SCORE >= 70 & SCORE <= 79)
sss2_80 <- sss2 %>% filter(SCORE >= 80)

# SSS 3 Classes
sss3_1_39 <- sss3 %>% filter(SCORE >= 1 & SCORE <= 39)
sss3_40_44 <- sss3 %>% filter(SCORE >= 40 & SCORE <= 44)
sss3_45_49 <- sss3 %>% filter(SCORE >= 45 & SCORE <= 49)
sss3_50_59 <- sss3 %>% filter(SCORE >= 50 & SCORE <= 59)
sss3_60_69 <- sss3 %>% filter(SCORE >= 60 & SCORE <= 69)
sss3_70_79 <- sss3 %>% filter(SCORE >= 70 & SCORE <= 79)
sss3_80 <- sss3 %>% filter(SCORE >= 80)

score_subject_table <- data.frame(
  "SSS 1 F/M" = c(
    length(filter(sss1_1_39, sss1_1_39$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_40_44, sss1_40_44$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_45_49, sss1_45_49$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_50_59, sss1_50_59$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_60_69, sss1_60_69$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_70_79, sss1_70_79$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss1_80, sss1_80$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(sss1_further_maths$SCORE)
    ),
  "SSS 1 GEO" = c(
    length(filter(sss1_1_39, sss1_1_39$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_40_44, sss1_40_44$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_45_49, sss1_45_49$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_50_59, sss1_50_59$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_60_69, sss1_60_69$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_70_79, sss1_70_79$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss1_80, sss1_80$SUBJECT == "GEOGRAPHY")$SCORE),
    length(sss1_geography$SCORE)
  ),
  "SSS 2 F/M" = c(
    length(filter(sss2_1_39, sss2_1_39$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_40_44, sss2_40_44$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_45_49, sss2_45_49$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_50_59, sss2_50_59$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_60_69, sss2_60_69$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_70_79, sss2_70_79$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss2_80, sss2_80$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(sss2_further_maths$SCORE)
  ),
  "SSS 2 GEO" = c(
    length(filter(sss2_1_39, sss2_1_39$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_40_44, sss2_40_44$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_45_49, sss2_45_49$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_50_59, sss2_50_59$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_60_69, sss2_60_69$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_70_79, sss2_70_79$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss2_80, sss2_80$SUBJECT == "GEOGRAPHY")$SCORE),
    length(sss2_geography$SCORE)
  ),
  "SSS 3 F/M" = c(
    length(filter(sss3_1_39, sss3_1_39$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_40_44, sss3_40_44$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_45_49, sss3_45_49$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_50_59, sss3_50_59$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_60_69, sss3_60_69$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_70_79, sss3_70_79$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(filter(sss3_80, sss3_80$SUBJECT == "FURTHER MATHEMATICS")$SCORE),
    length(sss3_further_maths$SCORE)
  ),
  "SSS 3 GEO" = c(
    length(filter(sss3_1_39, sss3_1_39$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_40_44, sss3_40_44$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_45_49, sss3_45_49$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_50_59, sss3_50_59$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_60_69, sss3_60_69$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_70_79, sss3_70_79$SUBJECT == "GEOGRAPHY")$SCORE),
    length(filter(sss3_80, sss3_80$SUBJECT == "GEOGRAPHY")$SCORE),
    length(sss3_geography$SCORE)
  )
)

rownames(score_subject_table) <- c(
  "1 - 39", "40 - 44", "45 - 49", "50 - 59", "60 - 69", "70 - 79", ">80", "Total"
)
