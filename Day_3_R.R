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
  geom_point()
# To connect coordinates (e.g. south_africa_coast data) with lines use geom_path 