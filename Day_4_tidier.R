# Day 4
# tidy data 2.0
# 01-02-2019
# Ethan

# load libraries
library(tidyverse)
library(lubridate) 

# Load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0
rm(SACTNmonthly_v4.0) # rm allows you to remove a variable

SACTN %>% 
  filter(site == "Amanzimtoti")

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)
# We asked R to extract a site, but only Pollock beach, so we told R to focus on it by typing 'site =='. We then sorted the sites based on the dates by only selecting December (12) or January (1) (month == 12 | month == 1)

SACTN %>% 
  arrange(depth, temp)
# arranges the selected columns from the lowest to the highest values.

SACTN %>% 
  arrange(desc(temp))
# arranges selected column from highest to lowest

SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)
# Select columns individually by name
# allows us to only select the data entries for the year 1990.




try1 <- SACTN %>% 
  select(site, src, date, temp)

# Select all columns between site and temp like a sequence. (:) this sign represents the word "till"
try2 <- SACTN %>% 
  select(site:temp)

# Select all columns except those stated individually
try3 <- SACTN %>% 
  select(-date, -depth)

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
try4 <- SACTN %>% 
  select(-(date:depth))
# this excludes data from one specified column to the other specified column.

try5 <- SACTN %>% 
  mutate(kelvin = temp + 273.15)
# Here we're asking R to add a column named kelvin by using the data in the temp. column and add 273.15 or kelvin units. 

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))


