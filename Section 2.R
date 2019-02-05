# Section 2: 
# 01-02-2019
# Ethan
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error
# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset

# load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

#loaded the ecklonia. csv data into R.
ecklonia <- read_csv("C:/Users/Ethan-UWC/Desktop/R/data/data/ecklonia.csv")

# Used the head function to prodce the first 6 rows of the data.
head(ecklonia)

# Used the tail function to prodce the lst 6 rows of the data.
tail(ecklonia)

# Glimpse function projects a preview of the data in the console  
glimpse(ecklonia)

# Used the names function to project the names of each column in the console.
names(ecklonia)

# The dim functin reflected the dimensions (rows and columns) of the data.
dim(ecklonia) # Dimensions of the data: 26 rows, 12 columns

# Hypothesis : Environmental conditions at Batsata Rock allows better stipe growth of E. maxima compared to those at Boulders Beach.
# Produced a scatter plot with a smoother line using ggplot, geom_point and geom_smooth. I then added a title to the graph using ggtitle.
eck_plot_3 <- ggplot(ecklonia, aes(x = stipe_diameter, y = stipe_length, colour = site)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("Comparison of stipe growth of Ecklonia maxima")

# Hypothesis : Environmental conditions at Batsata Rock allows better frond growth of E. maxima compared to those at Boulders Beach.
# Produced a boxplot using ggplot and geom_boxplot. I then added labels using the labs function and a title to the graph using ggtitle.
eck_plot_2 <- ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass)) +
  geom_boxplot(aes(fill = site)) + 
  labs(x = "frond length", y = "frond_mass") + 
  ggtitle("Comparison of Frond growth pf E. maxima") +
  theme_bw()

# Hypothesis : Environmental conditions at Batsata Rock allows  frond growth of E. maxima to be not only longer but heavier compared to those at Boulders Beach.
# Produced a bargraph using ggplot and geom_col. I then added labels using the labs function and a title to the graph using ggtitle.
eck_plot_1 <- ggplot(ecklonia, aes(x = frond_length, y = frond_mass, colour = site)) +
  geom_col(aes(group = site)) +
  labs(x = "frond length (mm)", y = "frond mass (kg)") +
  ggtitle("frond length of  E. maxima in relation to its mass")

# Used ggarrange function to include all 3 graphs in one pane and arranged them in 3 rows using the nrow command, and assigned a new name of plot_combined.
plot_combined <- ggarrange(eck_plot_1, eck_plot_2, eck_plot_3, nrow = 3)

# referred to ecklonia dataset and grouped it by site, and then I selected the stipe_length column and used the summarise function to project the mean, min, max, median, and variance of the stipe length.
ecklonia %>%
  group_by(site) %>%
  select(stipe_length) %>% 
  summarise(mean_st = mean(stipe_length),
            min_st = min(stipe_length),
            max_st = max(stipe_length),
            median_st = median(stipe_length),
            var_st = var(stipe_length))

# referred to ecklonia dataset and grouped it by site, and then I selected the stipe_diameter column and used the summarise function to project the mean, min, max, median, and variance of the stipe diameter.
ecklonia %>%
  group_by(site) %>%
  select(stipe_diameter) %>% 
  summarise(mean_sd = mean(stipe_diameter),
            min_sd = min(stipe_diameter),
            max_sd = max(stipe_diameter),
            median_sd = median(stipe_diameter),
            var_sd = var(stipe_diameter))

# referred to ecklonia dataset and grouped it by site, and then I  used the summarise function to project the variance and the total amount of entries in the stipe length column. I then used the mutate function to make a new column called se_sl and divided the variance by the total amount of entries in order to get the standard error.
ecklonia %>% 
  group_by(site) %>% # Group the dataframe by site
  summarise(var_sl = var(stipe_length), # Calculate variance
            n_sl = n()) %>%  # Count number of values
  mutate(se_sl = sqrt(var_sl / n_sl)) # Calculate se

# referred to ecklonia dataset and grouped it by site, and then I  used the summarise function to project the variance and the total amount of entries in the stipe diameter column. I then used the mutate function to make a new column called se_sd and divided the variance by the total amount of entries in order to get the standard error.
ecklonia %>% 
  group_by(site) %>% # Group the dataframe by site
  summarise(var_sd = var(stipe_diameter), # Calculate variance
            n_sd = n()) %>%  # Count number of values
  mutate(se_sd = sqrt(var_sd / n_sd)) # Calculate se

# referred to ecklonia dataset and grouped it by site, and then I selected the stipe length column and used the summarise function to project the  min and max values of the stipe length.
ecklonia %>%
  group_by(site) %>%
  select(stipe_length) %>% 
  summarise(min_stl = min(stipe_length),
            max_stl = max(stipe_length))

# referred to ecklonia dataset and grouped it by site, and then I selected the frond length column and used the summarise function to project the  min and max values of the frond length.
ecklonia %>%
  group_by(site) %>%
  select(frond_length) %>% 
  summarise(min_fl = min(frond_length),
            max_fl = max(frond_length))

#Used the summary function for R to project a complete summary of statistical parameters of the ecklonia dataset.
sum_eck <- summary(ecklonia)
  