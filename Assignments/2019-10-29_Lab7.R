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
Jaffe <- read_csv("datasets/demos/Jaffe.csv")
View(Jaffe)

Jaffe <-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))

head(Jaffe)
summary(Jaffe)

ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 10)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 10)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

model_Aldrin <- lm(Aldrin~Depth, data = Jaffe)

model_HCB <- lm(HCB~Depth, data = Jaffe)

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_growth.rate = n())
ratio_Aldrin <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio_HCB <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

autoplot(model_Aldrin)

autoplot(model_HCB)

anova(model_Aldrin)

anova(model_HCB)

Jaffe$trans_Aldrin <- log10(Jaffe$Aldrin)

model_TransAldrin <- lm(trans_Aldrin~Depth, data = Jaffe)

summ_trans_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_trans_Aldrin = mean(trans_Aldrin),
            sd_trans_Aldrin = sd(trans_Aldrin),
            n_trans_Aldrin = n())
ratio_trans_Aldrin <-(max(summ_trans_Aldrin$sd_trans_Aldrin))/(min(summ_trans_Aldrin$sd_trans_Aldrin))

autoplot(model_TransAldrin)

anova(model_TransAldrin)

tukey <- glht(model_TransAldrin, linfct = mcp(Depth = "Tukey"))
summary(tukey)

tukey <- glht(model_HCB, linfct = mcp(Depth = "Tukey"))
summary(tukey)











