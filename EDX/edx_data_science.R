####################################################

## Project: Introduction to Programming using R

## Script purpose: Writing of tutorial scripts

## Date: 31st January, 2023

## Author: Bernard Iorver

##################################################


# 1. set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")

# Install "dslabs" Package
install.packages("dslabs")

# Load "dslabs" Package
library("dslabs")

# Load "murders" data
data("murders")

# Data Types
a <- 1
class(a)
class(ls)
class(murders)

# structure of an object
str(murders)

# first six lines of a dataset
head(murders)

# names of variables
names(murders)

# Accessor
murders$population

# numeric vector
pop <- murders$population
length(pop)
class(pop)

# character vector
st <- murders$state
class(st)

# logical vectors
z <- 3 == 2
z
class(z)

# factors
reg <- murders$region
class(reg)
levels(reg)

# objects
# quadratic equation  -- x^2 + x - 1 = 0   x^2 - 4x + 4 = 0
a <- 1
b <- 1
c  <- -1
solution_1 <- (-b + (sqrt((b^2) - (4*a*c))))/(2*a)
solution_2 <- (-b - (sqrt((b^2) - (4*a*c))))/(2*a)

# list of objects saved in the workspace
a
print(a)
ls()

# Functions
log(a)
exp(1)
log(2.718282)
log(exp(1))

# help
help("log")
?log
args(log)
log(8, base=2)
log(x=8, base=2)
log(8, 2)

# arithmetic operators
help("+")
?"+"

# available data for practice
data()
CO2
pi
Inf

# 2 vectors -------
country <- c("italy", "canada", "egypt")
# without quotes
codes <- c(italy=380, canada=124, egypt=818)
# with quotes
codes <- c("italy"=380, "canada"=124, "egypt"=818)
codes
class(codes)
# another method
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")
names(codes) <- country
codes

# sequences
seq(1, 10)
seq(1, 10, 2)
# or we can use
1:10

# sub-setting
codes[2]
codes[c(1, 3)]
codes[1:2]
codes["canada"]
codes[c("egypt", "italy")]

# vector arithmetic
heights <- c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)
heights * 2.54
heights - 69

# vector coercion
# r will coerce 1 and 3 to characters
w <- c(1, "canada", 3)
w
class(w)
# other functions for coercing data
x <- 1:5 
y <- as.character(x)
y
as.numeric(y)

# missing data (NA - Not Available)
t <- c("1", "b", "3")
# by coercion b will be converted to NA
as.numeric(t)

# sorting
# sort -- in increasing order
sort(murders$total)
# order -- returns the indices that sorts the vector
s <- c(31, 4, 15, 92, 65)
s
sort(s)
order(s)
index <- order(s)
s[index]
# order the state name by murder total
index <- order(murders$total)
murders$state[index]
murders$abb[index]
# rank -- returns the index in order of the highest value
rank(s)
# summary
data.frame(original=s, sort=sort(s), order=order(s), rank=rank(s))

# max returns the highest value
max(murders$total)
# which.max returns the variable with highest value
i_max <- which.max(murders$total)
murders$state[i_max]
# we can also do the same for the minimum
min(murders$total)
i_min <- which.min(murders$total)
i_min
murders$state[i_min]

# state with the highest population
murders$state[which.max(murders$population)]
# how many people does it have
max(murders$population)
# compute the murder rate for every state
murder_rate <- (murders$total / murders$population) * 100000
murder_rate
# order the states by murder rate
murders$state[order(murder_rate, decreasing = TRUE)]

# sorting data frame
# order the data set base on the population of states (ERROR)
murders[order(population)] |> head()
murders[order(region, rate)]

# sorting data tables (ERROR)
library("dplyr")
# orders in accending order
murders %>% arrange(rate) %>% head()
# orders in decending order
murders %>% arrange(desc(rate)) %>% head()
# to see a larger proportion
murders %>% top_n(10, rate)
# sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)

# 3 -- indexing
# use of logical to index vectors
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

# 4 basic conditional statements -----------------
# general structure of an "if/else" statement
if(a != 0) {
  print(1/a)
} else {
  print("No reciprocal for 0.")
}
library(dslabs)
data("murders")
murder_rate <- (murders$total / murders$population) * 100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5) {
  print(murders$state[ind])
} else {
  print("No state has a murder rate that lower than 0.5")
}
# if we change the level we get the alternative
if(murder_rate[ind] < 0.25) {
  print(murders$state[ind])
} else {
  print("No state has a murder rate that lower than 0.25")
}
# another relative function is the "ifelse"
al_1 <- 0
ifelse(al_1 > 0, 1/al_1, NA)
# vector use cases
al_2 <- c(0, 1, 2, -4, 5)
result <- ifelse(al_2 > 0, 1/al_2, NA)
# we can check this using another data
data("na_example")
sum(is.na(na_example))
# lets create a vector with no "nas"
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))
# any returns true if any of the conditions is true
zl_1 <- c(TRUE, TRUE, FALSE)
any(zl_1)
zl_2 <- c(FALSE, FALSE, FALSE)
any(zl_2)
zl_3 <- c(TRUE, TRUE, TRUE)
# all returns true if all of the conditions is true
all(zl_1)
all(zl_2)
all(zl_3)

# basic functions
# simple function that performs the "average"
avg_1 <- function(x) {
  s <- sum(x)
  n <- length(x)
  s/n
}
x_a <- 1:100
avg_1(x_a)
# we can show it is thesame answer with the mean
identical(mean(x_a), avg_1(x_a))
# many variables
avg_2 <- function(x, arithmetic = TRUE) {
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/2))
}

# for loops
# shows the sum of any number (for 3 == "1 + 2 + 3")
compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}
compute_s_n(3)
compute_s_n(100)
compute_s_n(2017)
# basic for loop
for (i in 1:5) {
  print(i)
} 
# the value of i is the last number in the loop
i
# shows the sum for various value of n 
m <- 25
# create empty vector
s_n <- vector(length = m)
for (n in 1:m) {
  s_n[n] <- compute_s_n(n)
}
s_n
# we can make a plot
n <- 1:m
plot(n, s_n)
# we can then add line to the plot
lines(n, (n*(n + 1)/2))

# other functions
# apply, sapply, tapply and mapply are commonly used in place of for loops
# other functions are split, cut, quantile, reduce, identical, unique and many others

# Tibbles
library(dplyr)
murders %>% group_by(region)
# we can see the differences by comparint the two output version of the gapminder
library(dslabs)
data("gapminder")
# it is difficult to read like this
gapminder
# so lets convert it to tibble, it becomes much more readable
as_tibble(gapminder)
# subsetting a data frame may return a non data frame
class(murders[, 1])
# with tibble this does not happen
class(as_tibble(murders)[, 1])
# to get the vector of a column on tibble
class(as_tibble(murders)$state)
# tibbles gives warning when a column does not exist
murders$State
as_tibble(murders)$State
# to create a tibble
tibble(id = c(1, 2, 3), func = c("mean", "median", "sd"))
# this returns a special kind of tibble



# Unload Packages
library(pacman)
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
