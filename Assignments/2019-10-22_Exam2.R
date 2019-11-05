# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

library(readr)

#### CODE BREAKS HERE BC YOU DID NOT PASTE read_csv() ####

feathers <- mutate(feathers, diff = typical - odd)

ggplot(feathers) +
  geom_histogram(aes(diff), binwidth = 5)

ggplot(feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(feathers)+
  geom_qq(aes(sample = diff))

# Two-sided
t.test(feathers$typical, feathers$odd, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# Two-sided
SignTest(feathers$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

library(readr)
baker <- read_csv("datasets/exams/baker.csv")
View(baker)

baker <- mutate(baker, diff = After - Before)

ggplot(baker) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(baker)+
  geom_qq(aes(sample = diff))



# One-sided, HA that afterImplant is less than beforeImplant
SignTest(baker$diff, alternative = "less", mu = 0, conf.level = 0.95)

library(readr)
CO2_treatments <- read_csv("datasets/demos/CO2_treatments.csv")
View(CO2_treatments)

# Look at the summary statistics
summ_CO2 <- CO2_treatments %>%
  group_by(treatment) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())
# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_CO2$sd_growthrate))/(min(summ_CO2$sd_growthrate))

# Look at histograms, box plots, q-q plots
ggplot(CO2_treatments) +
  geom_histogram(aes(growthrate), binwidth = 2)+
  facet_wrap(~treatment)

ggplot(CO2_treatments) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(CO2_treatments)+
  geom_qq(aes(sample = growthrate, color = treatment))

# Two-sided
t.test(growthrate ~ treatment, data = CO2_treatments, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


#### CODE BROKE ONE TIME, 5/6 PTS ####








