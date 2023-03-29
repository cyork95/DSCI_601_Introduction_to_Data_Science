library(ggplot2)
library(tidyverse)
library(dplyr)

## household example

dpois(3, 2)

## when lambda changes how about the graphs?
set.seed(2)

poisson_data <- data.frame('data' = rpois(1000, 1))

poisson_data %>% ggplot() +
  geom_histogram(aes(x = data,
                     y = stat(count / sum(count))),
                 color = 'black',
                 binwidth = 1) +
  geom_vline(xintercept = 1,
             size = 1,
             linetype = 'dashed',
             color = 'red') +
  theme_bw() +
  labs(x = "Number of successes per period",
       y = "Proportion",
       title = '1,000 samples of Pois(lambda = 1)')

poisson_data <- data.frame('data' = rpois(1000, 10))