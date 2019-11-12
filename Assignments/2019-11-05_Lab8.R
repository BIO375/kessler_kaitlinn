# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

install.packages("hms")

#### Problem 15-22 ####
# Complete parts a, b, c, d

library(readr)
WalkingStickHeads <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv")
View(WalkingStickHeads)

WalkingStickHeads <-read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", col_types = cols(
  specimen = col_factor() ))

head(WalkingStickHeads)
summary(WalkingStickHeads)

ggplot(WalkingStickHeads, aes(x = "", y = headwidth))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(WalkingStickHeads) +
  geom_histogram(aes(headwidth), binwidth = 0.01)
  
ggplot(WalkingStickHeads)+
  geom_qq(aes(sample = headwidth, color = ""))

model01 <- lm(headwidth~specimen, data = WalkingStickHeads)

summ_headwidth <- WalkingStickHeads %>%
  group_by(specimen) %>% 
  summarise(mean_headwidth = mean(headwidth),
            sd_headwidth = sd(headwidth),
            n_headwidth = n())
ratio <-(max(summ_headwidth$sd_headwidth))/(min(summ_headwidth$sd_headwidth))

autoplot(model01)

## a
anova(model01)

summary(model01)

tukey <- glht(model01, linfct = mcp(specimen = "Tukey"))
summary(tukey)

model_01 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = WalkingStickHeads)

model01_varcomp <- VarCorr(model_01)
model01_varcomp

## b
varAmong  <- as.numeric( model01_varcomp[1,1] )

varWithin <- as.numeric( model01_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

## c 
# repeatability= 0.597

## d
# Femur length has a higher repeatability with a value of 74%. Both studies had a sample
# size of 25 walking-sticks insects, with each specimen measured twice. Both are randomly sampled,
# and therefore both traits are equally affected by measurement error. 

#### Problem 15-23 ####
# Complete parts a and c only

library(readr)
PineCones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv")
View(PineCones)

head(PineCones)
summary(PineCones)

ggplot(PineCones, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() 
 
ggplot(PineCones) +
  geom_histogram(aes(conemass), binwidth = 5)+
  facet_wrap(~habitat)

ggplot(PineCones)+
  geom_qq(aes(sample = conemass, color = habitat))

model02 <- lm(conemass~habitat, data = PineCones)

summ_conemass <- PineCones %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_growth.rate = n())
ratio <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))

autoplot(model02)

anova(model02)

summary(model02)

PineCones <-read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

## a
# planned comparison 

planned <- glht(model02, linfct = 
                  mcp(habitat = c("island.present - island.absent=0")))
                                  
confint(planned)
summary(planned)

## c

model_pinecones <- lme(fixed = conemass ~ 1,
               random = ~1|habitat, data = PineCones)

model_pinecones_varcomp <- VarCorr(model_pinecones)
model_pinecones_varcomp

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

library(readr)
Malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv")
View(Malaria)

Malaria <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))

ggplot(Malaria, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 10)+
  facet_wrap(~treatmentGroup)
ggplot(Malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

model_malaria <- lm(logSporozoiteNumbers~treatmentGroup, data = Malaria)

summ_logSporozoiteNumbers <- Malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

autoplot(model_malaria)

anova(model_malaria)

summary(model_malaria)

tukey_malaria <- glht(model_malaria, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey_malaria)

kruskal.test(logSporozoiteNumbers ~ treatmentGroup, data = Malaria)

oneway.test(logSporozoiteNumbers ~ treatmentGroup, data = Malaria)


#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

library(readr)
FiddlerCrab <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv")
View(FiddlerCrab)

FiddlerCrab <-read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))

FiddlerCrab <- slice(FiddlerCrab, -85)

head(FiddlerCrab)
summary(FiddlerCrab)

ggplot(FiddlerCrab, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(FiddlerCrab) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)
ggplot(FiddlerCrab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

model_crab <- lm(bodyTemperature~crabType, data = FiddlerCrab)

summ_bodyTemperature <- FiddlerCrab %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_growth.rate = n())
ratio <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))

autoplot(model_crab)

anova(model_crab)

summary(model_crab)

FiddlerCrab <- FiddlerCrab %>%
  mutate(crabType = fct_recode(crabType, female = "female",
                               intact_male = "intact male",
                               mm_removed = "male minor removed",
                               mmajor_removed = "male major removed"
                              
  ))

tukey_crab <- glht(model_crab, linfct = mcp(crabType = "Tukey"))
summary(tukey)

kruskal.test(bodyTemperature ~ crabType, data = FiddlerCrab)








