
# Load Libraries
pacman::p_load(pacman, psych)

# Import data set
dataset <-  read.csv("C:\\Benion\\Benion Programmings\\Python\\AI & ML\\data\\raw-data.csv", head=TRUE, sep=",")
head(dataset, n = 10)
summary(dataset)
describe(dataset)
data_description <- describe(dataset)

final_data <- data.frame(
  "n" = data_description$n,
  "Mean" = round(data_description$mean, 2),
  "Std" = round(data_description$sd, 2),
  "Min" = data_description$min,
  "Max" = data_description$max,
  "SE" = round(data_description$se, 2))
rownames(final_data) <- c("R", "S", "T", "T+P", "T+R+P", "T+R+C", "T+R+S")

# Final Table
write.csv(final_data, file = "C:\\Benion\\Benion Programmings\\R\\final-raw-data.csv", quote = FALSE, row.names = TRUE)

