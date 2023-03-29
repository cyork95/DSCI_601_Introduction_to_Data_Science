## Probability in R

install.packages("gtools")
library(gtools)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(dplyr)

# Pick a bead at random from a bag that contains three blue beads and two red ones.
# Random number generators permit us to mimic the process of picking at random.

# First, we use the function rep to generate the the bag:

beads <- rep(c('red', 'blue'), times = c(2, 3))
beads

sample(beads, 1)

# to perform our first Monte Carlo Simulation, we use the replicate function,
# which permits rept=10,000 times

rept <- 10000
events <- replicate(rept, sample(beads, 1))

# we can now see if our definition actually is in agreement with this Monte
# Carlo simulation

tab <- table(events)
tab

# prop.table gives us the proportions

prop.table(tab)

# set a random seed

set.seed(1)

# with and without replacement

events <- sample(beads, 10000, replace = TRUE)
prop.table(table(events))

events <- sample(beads, 5, replace = FALSE)
prop.table(table(events))

# combinations and permutations

# first lets construct a deck of cards. for this, we will use the exapand.grid
# and paste . we can paste to create strings by joining smaller things. to do
# this we take the number and suit of a card and create a card.

number <- "3"
suit <- "Hearts"
paste(number, suit)

# here is how to generate a deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Duece", "Three", "Four", "Five", "Six", "Seven", "Eight",
             "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# with the deck constructed we can double check that the probability of a king 
# in the deck is 1/13 by computing the proportion of possible outcomes that 
# satisfy our conditions

kings <- paste("King", suits)
mean(deck %in% kings)

# to compute all the possible ways we can choose two cards when the order 
# matters

hands <- permutations(52, 2, v = deck)

first_card <- hands[, 1]
second_card <- hands[, 2]

kings <- paste("King", suits)

# to get the conditional probability we compute what fraction of these have a 
# king in the hand

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# continuous probability

data(heights)
x <- heights %>%
  filter(sex == "Male") %>%
  pull(height)
m <- mean(x)
s <- sd(x)
1 - pnorm(70.5, m, s)

pnorm(70.9, m, s) - pnorm(70.1, m, s)

x <- seq(-4, 4, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))

# height dataset in dslabs

ggplot(heights, aes(x = height, fill = sex)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(aes(xintercept = mean(height)), color = "blue",
             linetype = "dashed") +
  labs(title = "Height Histogram Plot", x = "Height(foot)", y = "Count") +
  theme_classic()

# change line colors by groups need dpylr 

mu <- dyplr(heights, "sex", summarise, grp.mean = mean(height))
head(mu)