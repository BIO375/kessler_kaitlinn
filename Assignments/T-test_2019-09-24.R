# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
library(readr)

birth_rate_2019_09_24 <- read_csv("datasets/demos/birth_rate_2019-09-24.csv")
View(birth_rate_2019_09_24)

summary(birth_rate_2019_09_24)

ggplot(birth_rate_2019_09_24)+
  geom_histogram(aes(Birth_diff), bindwith = 10) 
ggplot(birth_rate_2019_09_24)+
  geom_histogram(aes(Birth_diff), bindwith = 30)

ggplot(birth_rate_2019_09_24)+
  geom_boxplot(aes(x = Country, y = Birth_diff), notch = TRUE, varwidth = TRUE)

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>% slice (-105)

summary(data01)

ggplot(data01)+
  geom_histogram(aes(squamosalHornLength), bindwith= 10)

ggplot(data01)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = TRUE, varwidth = TRUE)

# Typo lines 18 & 20: binwidth not bindwith
# Line 23, x="" to indicate no x.
# Missing variance ratio to test for equal variance
# 7/10 pts #####

