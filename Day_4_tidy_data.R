# Day 4
# 01-02-2019 

library(tidyverse)
library(ggpubr)

load("data/SACTN_mangled.RData")

ggplot(SACTN1,aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) + 
  labs(x = "Date", y = "Temp")
# "colour = site" function tells R to give the line for each site a different colour.  
# "group = paste0" function allows the user to group 2 or more variables

SACTN2_tidy <- SACTN2 %>% 
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")

# Problem with SACTN3 data is that one column contains the variable and the next column the values, so we will try to combine it so that values and variable are within the same column.

SACTN3_tidy <- SACTN3 %>%  
  spread(key = var, value = val)

# SACTN4 data contains both the site and the source within the first column

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")
# This code allowed us to seperate the site and the source from the index column using ' col = index ' into 2 seperate columns specifying with ' into = c("site", "src") ' and seperating with ' sep = "/" ' and telling R to separate with a - .

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")
# unite function allows the user to bring together different columns and data into one column
# " col = date " tells R to take the 3 columns and place them in one column named date.
# "sep = "-" " tells R to seperate the values with a dash.

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)
# left_join function detects similar words in different sets of data and joins them based on them. We used it to combine the SACTN4a_tidy and SACTN4b_tidy data.

# [A.A]
# Good comments
# Shows a clear/better understanding
# Neat script
# Nicely done