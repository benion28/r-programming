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


# Unload Packages
library(pacman)
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
