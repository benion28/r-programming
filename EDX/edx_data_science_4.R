####################################################

## Project: Introduction to Programming using R

## Script purpose: Writing of tutorial scripts

## Date: 31st January, 2023

## Author: Bernard Iorver

##################################################


# set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")


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
