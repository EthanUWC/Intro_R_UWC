# Section 1: 
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, glimpse etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things.

#load libraries
library(tidyverse)
library(ggpubr)
library(lubridate)
library(scales)
library(ggsn)
library(dyplr)
library(ggmap)

# load datasets
load("C:/Users/Ethan-UWC/Desktop/R/data/data/rast_aug.RData")
load("C:/Users/Ethan-UWC/Desktop/R/data/data/rast_feb.RData")

# Use head function to produce the first 6 rows of the dataset
head(rast_aug)
head(rast_feb)

# Use tail function to produce the last 6 rows of the dataset
tail(rast_aug)
tail(rast_feb)

# Glimpse function projects a preview of the data in the console
glimpse(rast_aug)
glimpse(rast_feb)



sst <- MUR_low_res # load MUR_low_res data and assign a new name - sst (stands for sea surface temperature)

# Used the website given in class to create a palette.
cols11 <- c("#525F78", "#58A8BC", "#43B2A1", "#65B57B", "#96B259", "#C7A94C", "#F09B5D")

# Used ggplot to produce a primary map, then included the sst data in order to get the sea surfaces temperatures along the coast. Included the palette to create a legend that projects the sea surface temperatures according to colour on the map. I then assigned labels and a title to the map.
rast_aug_map <- ggplot(data = rast_aug, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs(x = "longitude", y = "latitude") +
  ggtitle("rast_august") 
    
# Assigned a new name, r_aug, to the rast_aug_map. Included the labels Atlantic Ocean and Indian Ocean, and position them each on their specified borders of the country. 
  r_aug <- rast_aug_map +
    annotate("text", label = "Atlantic\nOcean", 
             x = 16.5, y = -32.0, 
             size = 4.5, 
             angle = 0, 
             colour = "orange") +
    annotate("text", label = "Indian\nOcean", 
             x = 32.5, y = -31, 
             size = 4.5, 
             angle = 0, 
             colour = "red")
  
# Assigned a new name, r_aug2, to the r_aug map. Included a scale bar and a north arrow and then positioned them into the bottim right corner using the x/y min/max functions.
  r_aug2 <- r_aug +
    scalebar(x.min = 29, x.max = 31, y.min = -33.5, y.max = -34.5, # Set location of bar
             dist = 150, height = 0.5, st.dist = 0.9, st.size = 4, # Set particulars
             dd2km = TRUE, model = "WGS84") + # Set appearance
    north(x.min = 30, x.max = 34, y.min = -34, y.max = -32.2, # Set location of symbol
          scale = 1.2, symbol = 16)
  
# Assigned a new name, r_aug_final, to the r_aug2 map. Included insetting as an extra to the map using the annotation_custom function and positioning it using the x/y min/max functions.
  r_aug_final <-r_aug2 +
    annotation_custom(grob = ggplotGrob(africa_map),
                      xmin = 20.9, xmax = 26.9,
                      ymin = -30, ymax = -28)
    

  # Used ggplot to produce a primary map, then included the sst data in order to get the sea surfaces temperatures along the coast. Included the palette to create a legend that projects the sea surface temperatures according to colour on the map. I then assigned labels and a title to the map.
rast_feb_map <- ggplot(data = rast_feb, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs(x = "longitude", y = "latitude") +
  ggtitle("rast_feb") 

# Assigned a new name, r_feb, to the rast_feb_map. Included the labels Atlantic Ocean and Indian Ocean, and position them each on their specified borders of the country. 
r_feb <- rast_feb_map +
  annotate("text", label = "Atlantic\nOcean", 
           x = 16.5, y = -32.0, 
           size = 4.5, 
           angle = 0, 
           colour = "orange") +
  annotate("text", label = "Indian\nOcean", 
           x = 32.5, y = -31, 
           size = 4.5, 
           angle = 0, 
           colour = "red")


# Assigned a new name, r_feb2, to the r_feb map. Included a scale bar and a north arrow and then positioned them into the bottim right corner using the x/y min/max functions.

r_feb2 <- r_feb +
  scalebar(x.min = 29, x.max = 31, y.min = -33.5, y.max = -34.5, # Set location of bar
           dist = 150, height = 0.5, st.dist = 0.9, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 30, x.max = 34, y.min = -34, y.max = -32.2, # Set location of symbol
        scale = 1.2, symbol = 16)

# Assigned a new name, r_feb_final, to the r_feb2 map. Included insetting as an extra to the map using the annotation_custom function and positioning it using the x/y min/max functions.
r_feb_final <-r_feb2 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,  
                    ymin = -30, ymax = -28)


