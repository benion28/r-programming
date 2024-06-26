####################################################

## Project: Introduction to Programming using R

## Script purpose: Writing of tutorial scripts

## Date: 31st January, 2023

## Author: Bernard Iorver

##################################################


# set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")


# 3 -- indexing
# use of logical to index vectors
library(dslabs)
data(murders)
murder_rate <- (murders$total / murders$population) * 100000
l_index <- murder_rate < 0.71
l_index <- murder_rate <= 0.71
l_index
murders$state[l_index]
# count the true(1) or false(0) entries
sum(l_index)
# two logical conditions
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE
# two logical conditions with the vectors
west <- murders$region == "West"
safe <- murder_rate <= 1
ll_index <- safe & west
murders$state[ll_index]

# indexing functions
r <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(r)
w_index <- which(murders$state == "Massachusetts")
w_index
murder_rate[w_index]
# or we can just use normal indexing
w_index <- murders$state == "Massachusetts"
murder_rate[w_index]
# match -- returns the index needed to access them
m_index <- match(c("New York", "Florida", "Texas"), murders$state)
m_index
murders$state[m_index]
murder_rate[m_index]
# %in% -- to check if the element of the first is in the second
p <- c("a", "b", "c", "d", "e")
q <- c("a", "d", "f")
q %in% p
(c("Boston", "Dakota", "Washington") %in% murders$state)

# Basic data wrangling
library("dplyr")
# add murder rate to our table
murders <- mutate(murders, rate=total/population * 100000)
# only show the entries lower with murder rate than 0.71
filter(murders, rate <= 0.71)
# to select columns we want to work with
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)
# we can use pipe to select, filter and insert into table
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# Basic plots
# scatter plots
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)
# histogram
hist(murders$rate)
murders$state[which.max(murders$rate)]
# boxplot
boxplot(rate ~ region, data = murders)

# summarizing data
library("dplyr")
library("tidyverse")
# access resulting values in the table
# arrange data after sorting
# provide summary statistic into readable code
murders <- mutate(murders, rate=(total / population) * 10^5)
# minimum, median and maximum for states in the western region
ss <- murders %>% filter(region == "West") %>% summarise(
  minimum = min(rate),
  median = median(rate),
  maximum = max(rate)
)
# compute murder rate for the whole country
us_murder_rate <- murders %>% summarize(rate = (sum(total) / sum(population)) * 10^5)

# summarizing data in data table
library(dslabs)
library(data.table)
data(heights)
heights <- setDT(heights)
heights
# summarize
sss <- heights %>% summarize(
  average = mean(height),
  standard_deviation = sd(height)
)
# alternative of the above
library(data.table)
sss_a <- heights[, .(average = mean(height), standard_deviation = sd(height))]
# computer multiple summary
median_min_max <- function(x) {
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}
heights[, .(median_min_max(height))]
# compute summary for different groups
library(data.table)
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

# summarize with more than one value
x <- 1:5 
quantile(x, c(0, 0.5, 1))
ss <- murders %>% filter(region == "West") %>% summarise(
  range = quantile(rate, c(0, 0.5, 1)),
)

# introduction to data.table
library(data.table)
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()    #(Error)
# add new column "rate"
murders[, rate := (total/population) * 100000]   #(Error)
head(murders)
# add new multiple columns
murders[, ":=" (rate = ((total/population) * 100000), rank = rank(population))]   #(Error)
# this not a copy of x, but another name for x
xd <- data.table(a = 1)
yd <- xd
# until x is changed before y becomes another copy and vice versa 
# (":=" --- update by reference)
xd[, a := 2]
yd
yd[, a := 1]
xd
# the "copy" forces the copy of the variable
xd <- data.table(a = 1)
yd <- copy(xd)
xd[, a := 2]
yd

# subsetting with "data.table"
# in "dply" we filter like this:
filter(murders, rate <= 0.7)
# then in "data.table"
library(data.table)
murders[rate <= 0.7]  #(ERROR)
# we can combine multiple filter options
murders[rate <= 0.7, .(state, rate)]  #(ERROR)
# compare with the "dplr" approach
library(dplyr)
murders %>% filter(rate <= 0.7) %>% select(state, rate)

# creating data frames
grades <- data.frame(
  names = c("John", "Juan", "Jean", "Yao"),
  exam_1 = c(95, 80, 90, 85),
  exam_2 = c(90, 85, 85, 90)
)
grades
# by default "data.frame" turns characters to "factors"
class(grades$names)
# to prevent this we add an additional option
grades <- data.frame(
  names = c("John", "Juan", "Jean", "Yao"),
  exam_1 = c(95, 80, 90, 85),
  exam_2 = c(90, 85, 85, 90),
  stringsAsFactors = FALSE
)
class(grades$names)

# pull access columns
class(us_murder_rate)
# we want access directly to the "rate" column using "pull"
library(dplyr)
us_murder_rate %>% pull(rate)
# we want to save the value directly
us_murder_rate_value <- murders %>% 
  summarize(rate = (sum(total) / sum(population)) * 10^5) %>%
  pull(rate)
us_murder_rate_value
# value is now numeric
class(us_murder_rate_value)

# grouping then summarize
# medium murder rate in each region of the USA
library(dplyr)
murders %>% group_by(region)
# this is now a special data frame called "a grouped" data frame
murders %>% group_by(region) %>% summarize(median = median(rate))

# the dot placeholder
# use of the "." to immitate the "pull"
us_murder_rate_value_d <- murders %>% 
  summarize(rate = (sum(total) / sum(population)) * 10^5) %>%
  .$rate
us_murder_rate_value_d
# value is now numeric
class(us_murder_rate_value_d)



# Unload Packages
library(pacman)
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
