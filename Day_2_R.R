#plotting in R using ggplot2
# day2
#Ethan
#30/01/2019

#load libraries
library(tidyverse)

lam <- read_csv("~/R/R/data/laminaria.csv")

chicks <- datasets::ChickWeight
??ChickWeight # the "??" tells R to refer you to the help tab

ggplot(data = chicks, aes(x = Time, y = weight)) + # ggplot is a function that allows a graph to be produced
  geom_point() + #geom_point - function that navigates the data points on a graph
  geom_line(aes(group = Chick)) #geom_line - function that navigates lines to points to fill the graph
# aes - function that navigates and distinguishes x and y coordinates. So in this case, aes specifies which data in the 'chick data' to use.
#ggplot is a function within tidyverse. If you are not using the tidyverse package then you plot a graph using ggplot2.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) #Creates a line graph with colour

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) + 
  geom_point() + 
  geom_smooth(method = "lm") # geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm) 

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point( colour = "blue") + # adding the colour description to the geom_point function changes the colour of each point to whatever the user specifies it to.
  geom_line(aes(group = Chick)) 

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + # 'size = weight' -using weight as reference to show the size of a point. This also adds a third variable to the graph.
  geom_smooth(method =  "lm") +
  labs(x = "Days", y = "Weight(kg)") + # labs function allows the user to change or add descriptions to the labels.
  ggtitle("Weight gain based on chick diets")
  theme_bw()
  
  ggplot(lam, aes(x = blade_length, y = total_length, colour = site)) +
    geom_point() + 
    geom_smooth(method =  "lm") +
    ggtitle("blade length vs total length") +
  theme_bw()
  
  ggplot(lam, aes(x = blade_length, y = total_length, colour = site)) +
    geom_point() + 
    geom_line(aes(group = blade_length)) +
    ggtitle("blade length vs total length") +
    theme_bw()
  
  #Facetting in ggplot
  
  
  library(ggpubr) #allow multiple objectives to be plotted on one frame.
  
  ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~Diet, ncol = 1, nrow = 4) #facet_wrap function allows you to wrap multiple plots on one frame. "ncol" means number of columns. "nrow" means number of rows.
  
chicks_2 <- chicks %>% 
  filter(Time == 21)

plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  ggtitle("Chick diets") +
  labs(x = "days", y = "weight(kg)") +
  theme_bw()

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("Weight gain based on chick diets")


plot_3<- ggplot(data = chicks_2, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) + #the 'position = dodge' command allows the histogram to align the bars next to each other instead of on top of each other.
  labs(x = "Final Mass", y = "Count") + 
  ggtitle("Weight gain based on chick diets") +
  theme_bw()

plot_4 <- ggplot(data = chicks_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) + 
  labs(x = "Diet", y = "Final Mass") + 
  ggtitle("Weight gain based on chick diets") +
  theme_bw()
  
plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4) #plot_combined combines the graphs that were generated into ine pane and ggarrange allows you to arrange the graphs in a certian sequence.

## 3rd library
library(boot)
urine <- boot::urine
??urine

urine %>% 
  select(-cond)

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))


# Homework
#---- formulate hypotheses of 3 sets of data. Use the 3 built-in datasets (use the datasets function) to produce 2 graphs per dataset.Calculate the means of one column in each dataset.
#---- Produce 3 graphs using the laminaria dataset 

Airpassengers <- datasets::AirPassengers
beaver <- datasets::beaver1
datasets::fdeaths
austres <- datasets::austres
airmiles <- datasets::airmiles

chick_data <- datasets::ChickWeight

datasets::lynx
plant <- datasets::PlantGrowth

#Beaver - day, time, temp, activ

Beav_plot <- ggplot(beaver, aes(x = time, y = temp, colour = temp)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("Temperature fluctuations over time") +
  labs(x = "time (sec)", y = "temp")


Beav_plot2 <- ggplot(beaver, aes(x = temp, y = activ, colour = activ)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("Activity based on temperature")

#Chick data - weight, Time, Chick, Diet
chick_plot <- ggplot(chick_data, aes(x = Time, y = weight, colour = weight)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("weight over time") 

chick_plot2 <- ggplot(chick_data, aes(x = Time, y = weight, colour = weight)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("weight over time") 
  




