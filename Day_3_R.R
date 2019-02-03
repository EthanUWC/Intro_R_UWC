# Day 3
# Mapping with ggplot
# Ethan Britz
# 31-01-2019

# load libraries
library(tidyverse)
library(ggpupr)

load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

#homework - code can be found in the workshop document at 3.4

sst <- MUR_low_res # sst stands for sea surface temperature

cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a") 

# "http://tristen.ca/hcl-picker/#/hlc/6/0.95/48B4B6/345363" - can use this link to find codes that project a desired palette.

ChickWeight <- datasets::ChickWeight

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point()

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point() +
  labs(x = "longtde", y = "lat") +
  ggtitle("south africa coastline")
  
# To connect coordinates (e.g. south_africa_coast data) with lines use geom_path 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) # The land mask

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent
# coordequal limits/restricts coordinates to what's specified.

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + # The ocean temperatures
  geom_raster(data = sst, aes(fill = bins)) +  
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #geom_polgyon creates a border
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette. Allows you to label your palette.
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4)) # Fine tune position of legend final map

# [A.A]
# More comments could be added
# Script is neat, simply looked like it is copied from the document
# Self made comments will help with your understanding in future
        