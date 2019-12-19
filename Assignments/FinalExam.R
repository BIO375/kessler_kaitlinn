# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
#install.packages(c("haven", "rvest"))

#install.packages(c("cli", "rlang"))

# Scenario 1. 
library(readr)
insulation <- read_csv("datasets/final/insulation.csv")
View(insulation)

model01 <- lm(heat_loss ~ leanness, data = insulation)

autoplot(model01, smooth.colour = NA)

insulation_plus <- augment(model01)
ggplot(data = insulation_plus)+
  geom_point(aes(x = leanness, y= .resid))

summary(model01)

ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model01)))

insulation <- insulation %>%
  mutate(sqrt_heat_loss = sqrt(heat_loss))
model03<-lm(sqrt_heat_loss ~ leanness, data = insulation)
ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model03)))

summary(model03)

# Scenario 2 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


library(readr)
caffeine <- read_csv("datasets/final/caffeine.csv")
View(caffeine)

caffeine <-read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))

head(caffeine)
summary(caffeine)

ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 10)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

model01 <- lm(half_life~group, data = caffeine)

summ_half_life <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())
ratio <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))

autoplot(model01)

summary(model01)

tukey <- glht(model01, linfct = mcp(group = "Tukey"))
summary(tukey)

# Line 106 should be planned comparisons not tukey HSD
### 9/10 pts ####







