#Day_1.R
#Laminaria dataset exploring and learning
#Nasreen Arend
#29 January 2019
#Trial

#Loading libraries

library(tidyverse)
lam <- read_csv("data/laminaria.csv") #explore data. Assign a new name of lam to the laminaria data.

head(lam) #shows first 6 rows
tail(lam) #shows last 6 rows
head(lam, n = 3) #shows first 3 rows

lam_select <- lam %>% # Assigning a new name to the Lam dataset using an assignment operator, then piped.
  select(site, total_length) %>% #selecting only the site and total length columns.
  slice(54:80) #selecting only rows 54 to 80

lam_kom <- lam %>% # Assigning a new name to the Lam dataset using an assignment operator, then piped.
  filter(site == "Kommetjie") # Extract the site Kommetjie from the site column.  

#In the Laminaria dataset select only site and blade_length column
#Filter only for Sea Point

lam_sample <- lam %>% # # Assigning a new name to the Lam dataset using an assignment operator, then piped.
  select(site, blade_length) %>% # Selecting only the site and blade length columns.
  filter(site == "Sea Point") # Extracting only the site Sea point from the site column.

Lam %>% #selecting laminaria dataset.
  filter(total_length == max(total_length)) # Extracting the maximum total length from the total length.  
summary(lam) # Getting a summary of  the lam dataset
dim(lam) # Exploring the dimensions of the lam dataset.

# Calculating the mean, median and standard deviation.
lam %>% # Referring to the lam dataset.
  summarise(avrg_bl = mean(blade_length), # Using the summarise function to
            med_bl = median(blade_length),# get the mean, median and standard
            sd_bl = sd(blade_length)) #deviation of the lam dataset. To do this we assigned a name to each parameter (mean, median, sd) before using its respective function.

# Calculating Standard Error
lam %>% # Referring to the lam dataset.
  group_by(site) %>% # Grouping the data by site. 
  summarize(var_bl = var(blade_length), #Using the summarize function to get 
            n = n()) %>% # the variance (var) and toal number of entries (n = n()) for blade length in the lam data.
  mutate(se = sqrt(var_bl/n)) # Mutate function makes a new column in the data and we used this in order to make a column for the standard error (se = sqrt(var/n)) for the blade length column.


# Removing columns from data.
lam2 <-  lam %>% # # Assigning a new name to the Lam dataset using an assignment operator, then piped.
  select(-blade_thickness, -blade_length) #Removed these two variables from laminaria dataset

# Excluding NA values from the data column.
lam %>%
  select(blade_length) %>% #The blade length data was selected
  na.omit %>% #NA values excluded 
  summarise(n = n())


lam %>% # Referring to the lam dataset
  select(blade_length) %>%  # Selecting blade length column
  summarise(n = n()) # Summarizing to get the total amount of entries in the column.

ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) + # ggplot tells R to make a plot. We referred to the lam data, then used the aes function to assign variables to x and y axes.
  geom_point(shape = 21, colour = "salmon", fill = "white") + # geom_point plots the data as points in the graph, then assigned a shape, colour and colour fill to the points.
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") # Used labs function to assign labels to the graph axes.
# We used (+) instead of piping to continue to the next line as accustomed when using ggplot.
