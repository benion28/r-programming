####################################################

## Project: Introduction to Programming using R

## Script purpose: Writing of tutorial scripts

## Date: 31st January, 2023

## Author: Bernard Iorver

##################################################


# set working directory ------------------------------------------------------
setwd("C://Benion//Benion Programmings//R//EDX")


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
library(dslabs)
data("murders")
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


# Unload Packages
library(pacman)
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L
