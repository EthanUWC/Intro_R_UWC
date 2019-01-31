# Mapping with goole
# Afternoon session
# Ethan
# 31-01-2019

# Load libraries
library(tidyverse)
library(ggmap)

# Load data
load("data/cape_point_sites.RData")

cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
