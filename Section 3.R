# Section 3:
# 01-02-2019
# Ethan
# Make use of the SACTN_day1 data:
# Here create a graph showing temperature variation between sites
# Select all the temperatures recorded at the site Port Nolloth during August or September.
# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
# Calculate the average temperature by depth
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script

# load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

SACTN_day_1 <- read_csv("C:/Users/Ethan-UWC/Desktop/R/data/data/SACTN_day_1.csv") # loaded SACTN_day_1 data into R by using the read_csv function.

SACTNd1_plot <- ggplot(SACTN_day_1, aes(x = date, y = temp, colour = site)) +
  geom_point() + 
  geom_smooth(method =  "lm") + 
  ggtitle("Temperature variation among sites") +
  labs(x = "time (year)", y = "temperature")  # Generated and Assigned a name to a plot, using the SACTN_day_1 data, by using the ggplot function. The aes function was then used to assign columns/ data to axes on a graph. The data was then plotted on to the graph as points by using the geom_point function, then geom_smooth was used to produce the most accurate straight line for the particular data by smoothing (finding a trendline) the data. 
# ggtitle allowed me to give the graph a title, while the labs function allowed me to assign labels to the axes.
  
SACTN_f <- SACTN_day_1 %>%  # Assigned a new name to the SACTN_day_1 data
  group_by(site, temp) %>%  # Grouped the data by site and temperature
  filter(site == "Port Nolloth", month(date) == 8 | month(date) == 9) # Extracted the data for the site Port Nolloth and specifying that I only wanted the data for the months August and September.

SACTN_f2 <- SACTN_day_1 %>%  # Assigned a new name to the SACTN_day_1 data.
  group_by(site) %>%  # Grouped the data by site and temperature.
  filter(site == "Port Nolloth",year(date) == 1994)  # Extracted the data for the site Port Nolloth and specifying that I only wanted the data for the year 1994.

SACTN_day_1 %>% # Referred to the SACTN_day_1 data
  select(temp) %>%  # Selected the temperature column within the dataset.
  summarise(mean_temp = mean(temp, na.rm = TRUE)) # Asked R to produce a table containing the averaage temperature by depth.
# Average temperature = 15.2  
