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

#Class assignment
#30-01-2019
#Ethan

#load libraries
library(tidyverse)
library(ggpubr) #allow multiple objectives to be plotted on one frame.

beaver <- datasets::beaver1 # Used the datasets function to select built-in datasets to use for the exercise.

??beaver # Used the help function to understand how to place the data in context

# Beaver data

# 1. Hypothesis: The beaver activity will decrease as body temperatures increase. 
Beav_plot <- ggplot(beaver, aes(x = time, y = temp, colour = activ)) +
  geom_point() + 
  geom_smooth(method =  "lm") + 
  ggtitle("Beaver activity based on temperature") +
  labs(x = "time (sec)", y = "temp")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# Mean beaver temperature
beaver %>% 
  select(temp) %>% 
  summarise(mean_temp = mean(temp))
# The mean body temperature is 36, 86219 degrees Celcius

# 2. Hypothesis:  Beaver body temperature decreased over time.
Beav_plot_2 <- ggplot(beaver, aes(x = time, y = temp)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("Temperature fluctuations over time") +
  labs(x = "time (sec)", y = "temp")
# labs function allows user to specify labels
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# Iris data

iris <- datasets::iris
??iris

# 3. Hypothesis - Petal sizes of species of Iris will be the same.

Iris_plot <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length, colour = Species)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparison of petals of species of Iris") +
  labs(x = "Petal Width", "Petal Length")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# 4. Hypothesis - Sepal lengths and Sepal widths are longer in Iris setova than the other species.

Iris_plot <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Comparison of Sepals of species of Iris") +
  labs(x = "Sepal Width", y = "Petal Length")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

iris %>% 
  select(Sepal.Length) %>% 
  summarise(mean_sepal_Length = mean(Sepal.Length))
# Mean Sepal length = 5, 843333

# Trees data

trees <- datasets::trees
??trees

# 5. Hypothesis - Girth (circumference) of trees increases as height of the tree increases

Trees_plot <- ggplot(trees, aes(x = Girth, y = Height)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Girth vs height") +
  labs(x = "Girth", "Height")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# 6. Hypothesis - The greater the Girth the more timber a tree has.

Trees_plot <- ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Girth vs Volume") +
  labs(x = "Girth", "Volume")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

trees %>% 
  select(Height) %>% 
  summarise(mean_height = mean(Height))
# Mean height = 76


# Laminaria data

# Scatter plot

lam_plot_1 <- ggplot(Lam, aes(x = blade_weight, y = blade_thickness, colour = site)) +
  geom_point() + 
  geom_smooth(method =  "lm") +
  ggtitle("blade Weight vs blade thickness")
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# Histogram


lam_plot_2<- ggplot(Lam, aes(x = blade_weight)) +
  geom_histogram(aes(fill = site), position = "dodge", binwidth = 10) + 
  labs(x = "blade_weight", y = "count") + 
  ggtitle("blade Weight based on blade height") +
  theme_bw()
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.

# Boxplot

lam_plot_3 <- ggplot(Lam, aes(x = total_length, y = stipe_length)) +
  geom_boxplot(aes(fill = site)) + 
  labs(x = "Total length", y = "Stipe length") + 
  ggtitle("Stipe length compared to total length") +
  theme_bw()
# ggplot is a function that allows a graph to be produced
# geom_smooth smooths out the data points to produce a straight line; method = "lm" is a function that directs R to produce a linear model (lm).
# geom_point - function that navigates the data points on a graph
# aes - function that navigates and distinguishes x and y coordinates.
#When using the ggplot function you use a + sign to continue a script instead of the pipe function.
#ggtitle is a function that allows the user to name the graph.
