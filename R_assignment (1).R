# Ethan Britz
#3540409
#R_assignment

#load libraries
library(Stat2Data)
library(fitdistrplus)
library(tidyverse)
library(plotly)
library(fitdistrplus)
library(logspline)
library(dplyr)
library(lubridate)
library(ggpubr)
library(corrplot)
library(logspline)
library(pgirmess)
library(rmarkdown)

data("FruitFlies") # Read in a built-in dataset, contained in the Stat2Data library, on FruitFlies.
help("FruitFlies") # Used the help function to understand what the data meant and how it was collected.

#test if increased reproduction reduces longevity for male fruitflies.
#The five groups are: 
#males forced to live alone, males assigned to live with one or eight interested females, and males assigned to live with one or eight non-receptive females.
#observations on each fly were longevity, thorax length, and the 
#percentage of each day spent sleeping. However, thorax length will not be considered in the analyses as there is already information ahowing that thorax length is positively correlated with longevity.

shapiro.test(as.numeric(FruitFlies$Longevity)) # Did a Shapiro test in order to see whether the data was normally distributed or not. The p- value was 0.4946, thus the data was not normally distributed.


# Produced a bargraph in order to display the relative strength of the count for each group of males with relation to the amount of partners the males had and the resultant longevity.
t_plot1 <- ggplot(FruitFlies, aes(x = as.factor(Partners), y = Longevity, fill = Partners)) + # Used ggplot function to create the plot and the aes command to assign variables to each axis. I used the fill command to distinguish the columns from each other.
  geom_col(aes(fill = Treatment), position = "dodge", colour = "black") + 
  labs(x = "Partners", y = "Longevity (days)") + #Used labs command to assign labels to the axes and assigned the them pubclean to the plot in order to remove the standard grid.
  theme_pubclean()+
  ggtitle("Relationship between the amount of partners and Longevity")#creates title

# Produced a bargraph in order to display the relative strength of the count for each group of males with relation to the amount of partners the males had and the resultant recovery time.
t_plot2 <- ggplot(FruitFlies, aes(x = as.factor(Partners), y = Sleep, fill = Partners)) + # Used ggplot function to create the plot and the aes command to assign variables to each axis. I used the fill command to distinguish the columns from each other.
  geom_col(aes(fill = Treatment), position = "dodge", colour = "black") + 
  labs(x = "Partners", y = "Sleep (% per day)") + #Used labs command to assign labels to the axes and assigned the them pubclean to the plot in order to remove the standard grid.
  theme_pubclean()+
  ggtitle("Relationship between the amount of partners and sleep")#creates title

glimpse(FruitFlies) # The glimpse function allows me preview my data.
head(FruitFlies) # head function projects the first 6  observations in my data.
tail(FruitFlies)#tail function projects the last 6  observations in the data.
summary(FruitFlies) #summary function projects the statistical parameters governing the data.

# I Wanted to test whether an increased number of partners affected longevity in males.
# H0: An increased number of sexual partners has no significant effect on longevity in male D. molenogaster.
# H1: An increased number of sexual partners has a significant effect on longevity in male D. molenogaster.
summary(aov(Longevity ~ Partners, data = FruitFlies)) # Ran an Anova test because I am comparing more than one variable in a continuous dataset. The Pr(>F) value was 0.000592, thus the null hypothesis is rejected as the data for the number of sexual partners is significantly different to that for longevity. However, the Anova test did not tell me if the data for all groups of males was significantly different to each other. Therefore, I did a kruskal test comparing the means for male longevity and the amount of sexual partners they had.

compare_means(Longevity ~ Partners, data = FruitFlies, method = "kruskal.test")
kruskalmc(Longevity ~ Partners, data = FruitFlies)

# The test showed significant differences in the data between each group of males. There was no significant differences in the data for isolated males and males with one sexual partner. However, there were significant differences in the data for each of the other groups.
#obs.dif critical.dif difference
#0-1    4.33     21.24446      FALSE
#0-8   23.57     21.24446       TRUE
#1-8   19.24     17.34603       TRUE


# I tried to transform the data by logging them to see if it produce a normally distributed dataset.

summary(aov(Sleep ~ Partners, data = FruitFlies)) # Ran an Anova test because I am comparing more than one variable in a continuous dataset. The Pr(>F) value was 0.842, thus the null hypothesis is accepted as the data for the number of sexual partners is not significantly different to that for recovery time/ sleep. However, the Anova test did not tell me if the data for all groups of males was significantly different to each other. Therefore, I did a kruskal test comparing the means for male longevity and the amount of sexual partners they had.

compare_means(Sleep ~ Partners, data = FruitFlies, method = "kruskal.test")
kruskalmc(Sleep ~ Partners, data = FruitFlies)
# The results for the kruskal test show that none of the data for each of the groups are significantly different to each other.



FruitFlies %>% # logged the data for the column 'sleep'.
  group_by(Partners) %>% 
  summarise(norm_fly = as.numeric(shapiro.test(Sleep)[2]),
            var_fly = var(Sleep))


FruitFlies %>% 
  group_by(Partners) %>% # logged the data for the column 'longevity'.
  summarise(norm_fly_long = as.numeric(shapiro.test(Longevity)[2]),
            var_fly_long = var(Longevity))

mut_fly <- FruitFlies %>% # Created new columns in the dataset containing the transformed data. 
  mutate(logsleep = log(Sleep),
         loglong = log(Longevity))

mut_fly %>% 
  group_by(Partners) %>% 
  summarise(norm_fly = as.numeric(shapiro.test(logsleep)[2]),
            var_fly = var(logsleep))

mut_fly %>% 
  group_by(Partners) %>% 
  summarise(norm_fly_long = as.numeric(shapiro.test(loglong)[2]),
            var_fly_long = var(loglong))

summary(aov(loglong ~ Partners, data = mut_fly))
summary(aov(logsleep ~ Partners, data = mut_fly))
#The Anova test however projected the same results as the non-transformed data.


# To give a graphical representation of the significant differences within the dataset I produced a notched boxplot which displays the medians and the quartiles for each group. 

t_plot3 <- ggplot(FruitFlies, aes(x = as.factor(Partners), y = Longevity, fill = Partners)) +   # Used ggplot function to create the plot and the aes command to assign variables to each axis. I used the fill command to distinguish the columns from each other.
  geom_boxplot(aes(fill = Treatment), position = "dodge", colour = "black", notch= TRUE) + #Used the command notch= TRUE to impose notches in each boxplot.
  labs(x = "Partners", y = "Longevity (days)") + #Used labs command to assign labels to the axes and assigned the them pubclean to the plot in order to remove the standard grid.
  theme_pubclean()+
  ggtitle("Relationship between the amount of partners and longevity")#creates title


t_plot4 <- ggplot(FruitFlies, aes(x = as.factor(Partners), y = Sleep, fill = Partners)) + # Used ggplot function to create the plot and the aes command to assign variables to each axis. I used the fill command to distinguish the columns from each other.
  geom_boxplot(aes(fill = Treatment), position = "dodge", colour = "black", notch= TRUE) + #Used the command notch= TRUE to impose notches in each boxplot.
  labs(x = "Partners", y = "Sleep (% per day)") + #Used labs command to assign labels to the axes and assigned the them pubclean to the plot in order to remove the standard grid.
  theme_pubclean()+
  ggtitle("Relationship between the amount of partners and sleep")#creates title

final_plot2 <- ggarrange(t_plot2, t_plot4, nrow = 2) # brought the two graphs for comparing partners and sleep on one plot and arranged them in rows.

final_plot3 <- ggarrange(t_plot1, t_plot3, nrow = 2) # brought the two graphs for comparing partners and sleep on one plaot and arranged them in rows.

cor.test(x = FruitFlies$Longevity, FruitFlies$Sleep,
         use = "everything", method = "pearson") #Did a correlation test to see whether there is a correlation between the two variables.

r_print <- paste0("r = ", 
                  round(cor(x =  FruitFlies$Longevity, FruitFlies$Sleep),2))#

#  Created a correlation plot to graphically present the relationship between sleep and longevity.
longSL<- ggplot(data = FruitFlies, aes(x = Sleep, y = Longevity)) +
  geom_smooth(method = "lm", colour = "black", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Sleep (% per day)", y = "Longevity (days)") +
  theme_pubclean() +
  ggtitle("The relationship between Longevity and Sleep")
