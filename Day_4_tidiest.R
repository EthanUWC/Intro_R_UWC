# Day 4 tidiest
# 01-02-2019
# Ethan

#load libraries
library(tidyverse)
library(lubridate)

load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

# To remove the original
rm(SACTNmonthly_v4.0)

SACTN_depth <- SACTN %>% 
  group_by(depth)

# This allows us to calculate mean temperature by depth.
  SACTN_depth_mean <- SACTN %>% 
  group_by(depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            count = n())

# Just running the data
SACTN_depth_mean

# This plot reflects the relationship between depth and temperature
ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  ggtitle( "Relationship between depth and temperature") + 
  labs(x = "depth", y = "mean temperature")

SACTN_30_years <- SACTN %>%
  group_by(site, src) %>%
  filter(n() > 360)
# filter((n) > 360) extracts the data for 30 years (360 months)


selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood")


SACTN %>% 
  filter(site %in% selected_sites) %>% # This line has been changed/shortened
  group_by(site, src) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

# Concatinate in order to group the sequence of sites you want. Filter to only the sites you're focused on.

# [A.A]
# self made comments help when studying for the exam
# More comments are always useful
# Neat script
