# Section 4:
# Make use of any two built in datasets:
# Make use of the summarise, select, group_by functions
# Create at least two visualisations that were not done in the Intro R workshop

#load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

# Used the dataset function to select a built-in dataset. I selected the 'orange' dataset.
Orange <- datasets::Orange

# Referred to the Orange dataset and grouped it by Tree, and then I selected the age column and used the summarise function to project the mean, min, max, median, and variance of the ages of the Trees.
Orange %>%
  group_by(Tree) %>%
  select(age) %>% 
  summarise(mean_a = mean(age),
            min_a = min(age),
            max_a = max(age),
            median_a = median(age),
            var_a = var(age))

# Used the dataset function to select a built-in dataset. I selected the 'iris' dataset.
iris <- datasets::iris

# Referred to the iris dataset and grouped it by Species, and then I selected the Sepal. Length column and used the summarise function to project the mean, min, max, median, and variance of the sepal lengths of each species.
iris %>%
  group_by(Species) %>%
  select(Sepal.Length) %>% 
  summarise(mean_sl = mean(Sepal.Length),
            min_sl = min(Sepal.Length),
            max_sl = max(Sepal.Length),
            median_sl = median(Sepal.Length),
            var_sl = var(Sepal.Length))

orange_plot_1 <- ggplot(Orange, aes(x = age, y = circumference, colour = Tree)) +
  geom_density(fill = "orange")



