rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
install.packages("Hmisc")
library(Hmisc)

# Chapter 13, #20

# Read in data
library(readr)
SalmonColor <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(SalmonColor)

# Calculate summary statistics
summ_SalmonColor <- SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_skinColor = mean(skinColor),
            sd_skinColor = sd(skinColor),
            n_skinColor = n())

# Calculate the ratio between the standard deviations
ratio <-(max(summ_SalmonColor$sd_skinColor))/(min(summ_SalmonColor$sd_skinColor))

## Ratio = 4.30

# Look at histograms, box plots, q-q plots
ggplot(SalmonColor) +
  geom_histogram(aes(skinColor), binwidth = 2)+
  facet_wrap(~species)

ggplot(SalmonColor) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(SalmonColor)+
  geom_qq(aes(sample = skinColor, color = species))

# Transform data set (logx)
SalmonColor$log_SC <- log(SalmonColor$skinColor)

# Calculate summary statistics
summ_SalmonColor <- SalmonColor %>%
  group_by(species) %>% 
  summarise(mean_log_SC = mean(log_SC),
            sd_log_SC = sd(log_SC),
            n_log_SC = n())

# Histogram, box plot, q-q plot for log transformation
ggplot(SalmonColor) +
  geom_histogram(aes(log_SC), binwidth = 0.5)+
  facet_wrap(~species)

ggplot(SalmonColor) +
  geom_boxplot(aes(x = species, y = log_SC))

ggplot(SalmonColor)+
  geom_qq(aes(sample = log_SC, color = species))

# Calculate the ratio between the standard deviations
ratio <-(max(summ_SalmonColor$sd_log_SC))/(min(summ_SalmonColor$sd_log_SC))

## Ratio = 2.62

# One-sided, HA that Kokanee is greater than Sockeye
t.test(log_SC ~ species, data = SalmonColor, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

# 20(a) 
# Two-sample t-test with log transformation or The Mann-Whitney U-test (Wilcoxan ranked sum)

# 20(b)
# Yes, there is a difference in the mean of the kokanee and sockeye skin color 
# Mean in group kokanee= 0.5326, Mean in group sockeye= -0.0114

# Chapter 13, #25

rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
install.packages("Hmisc")
Yes
library(Hmisc)

# Read in data
library(readr)
Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
View(Clearcuts)

# One-sample t-test 
# Two-sided
t.test(Clearcuts$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sample t-test, two-sided, p-value = 0.3996, fail to reject the null hypothesis
# There is not a significant difference in the change of biomass of rainforest areas followin clear-cutting 

# Chapter 13, #26 

rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
install.packages("Hmisc")
Yes
library(Hmisc)

# Read in data 
library(readr)
ZebraFinchBeaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")
View(ZebraFinchBeaks)

# Identify your response variable using the form dataset$variable_name
y<-ZebraFinchBeaks$preference

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# First step, calculate t_sample.  You will need to define what the null hypothesis mean is.
null_mean <- 0

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# One-sided, HA that sample mean is greater than null mean
t.test(ZebraFinchBeaks$preference, 
       alternative = "greater", mu = 0, conf.level = 0.95)


# P< 0.05, One-sample, one-sided t-test
# HO: Percentage of time the female sat next to the carotenoid-supplemented male </= (less than or equal to) 0 (zero indicates equal time for each brother)
# HA: Percentage of time the female sat next to the carotenoid-supplemented male > 0
# Reject null hypothesis, there is statistical difference between the prefecence females have for the carotenoid-supplemented male versus the non cartenoid-supplemented male. 

# Chapter 13, #16- Assignment Problem 

rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

# Read in data 
library(readr)
BeetleAngiosperms <- read_csv("datasets/abd/chapter13/chap13q16BeetleAngiosperms.csv")
View(BeetleAngiosperms)

# Mutate data
BeetleAngiosperms <- mutate(BeetleAngiosperms, diff = numberSpeciesAngiospermEaters - numberSpeciesGymnospermEaters)

# Look at histograms, boxplots, and q-q plots 
ggplot(BeetleAngiosperms) +
  geom_histogram(aes(diff), binwidth = 500)

ggplot(BeetleAngiosperms) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(BeetleAngiosperms)+
  geom_qq(aes(sample = diff))

# Change "pair" column to be read as a character, rather than a number 
data<-read_csv("datasets/abd/chapter13/chap13q16BeetleAngiosperms.csv",col_names = TRUE,
               col_types = cols(
                 pair = col_character() )
)


# Look at the summary statistics
summ_beetles <- BeetleAngiosperms %>%
  group_by(pair) %>% 
  summarise(mean_diff = mean(diff),
            sd_diff = sd(diff),
            n_diff = n())

ggplot(BeetleAngiosperms) +
  geom_histogram(aes(diff), binwidth = 10000)

ggplot(BeetleAngiosperms) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(BeetleAngiosperms)+
  geom_qq(aes(sample = diff))

# Paired t-test 

# Two-sided
t.test(BeetleAngiosperms$numberSpeciesAngiospermEaters, BeetleAngiosperms$numberSpeciesGymnospermEaters, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
# Two-sided
t.test(BeetleAngiosperms$diff, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)


# #16 (a) 
# No, small sample size, therefore assumptions are not met and not a normal distribution.  

# #16 (b)
# Yes, the groups that eat angiosperms and the groups that eat gymnosperms have different number of species. 
# Non-parametric Sign Test
# Two-sided
SignTest(BeetleAngiosperms$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
SignTest(BeetleAngiosperms$diff, alternative = "greater", mu = 0, conf.level = 0.95)

# I chose one-sided rather than two-sided because it was testing whether the number of species in angiosperm eaters is larger than the number of species of gymnosperm eaters. 

# #16 (c)
# Non-parametric Sign Test, one-sided, p-value = 0.03, reject null hypothesis
# HO: The median difference in number of species equal to or less than zero
# HA: The median  difference in number of species greater than zero
# Because the p-value is less than 0.05, we reject the null hypothesis, the angiosperm-eating group has a greater difference in the number of species than the number of gymnosperm eaters. 
# The sign test has low power, much lower than the one-sample t-test and the paired t-test. 
# Due to a small sample size of 5 observations (10 total), its power is even lower.
# Based on the fact that the angiosperm-eating group had more species in all five pairs, it is correct to reject the null hypothesis. 
# Despite its low power, it is the best alternative to the paired t-test or the one-sample t-test, when assumptions can absolutely not be met. 

# Chapter 13, #16-Review Problem 

rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

# Read in data
library(readr)
ZebraFishBoldness <- read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")
View(ZebraFishBoldness)

# Look at the summary statistics
summ_zebra <- ZebraFishBoldness %>%
  group_by(genotype) %>% 
  summarise(mean_AggressiveActivity = mean(secondsAggressiveActivity),
            sd_AggressiveActivity = sd(secondsAggressiveActivity),
            n_AggressiveActivity = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_zebra$sd_AggressiveActivity))/(min(summ_zebra$sd_AggressiveActivity))

# ration = 1.3914 < 3, assumptions met 

# Look at histograms, box plots, q-q plots
ggplot(ZebraFishBoldness) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 50)+
  facet_wrap(~genotype)

ggplot(ZebraFishBoldness) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))

ggplot(ZebraFishBoldness)+
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))

# For the two-sample t-test with pooled variance, there are additional arguments.  You need to give the 
# formula (response ~ predictor), identify the data, include var.equal = TRUE.

# Two-sided
t.test(secondsAggressiveActivity ~ genotype, data = ZebraFishBoldness, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# Mann-Whitney U test 

# #16 (a)
# HO: The mean rate of aggressive activity for the spd mutant zebrafish is equal to the mean rate of aggressive activity for the wild type zebrafish
# HA: The mean rate of aggressive activity for the spd mutant zebrafish is not equal to the mean rate of aggressive activity for the wild type zebrafish
# Two-sided
wilcox.test(secondsAggressiveActivity ~ genotype, data = ZebraFishBoldness, alternative = "two.sided", conf.level = 0.95)

# HO: The mean rate of activity for the spd mutant zebrafish is equal to or less than the mean rate of aggressive activity for the wild type zebrafish 
# HA: The mean rate of activity for the spd mutant zebrafish is greater than the mean rate of aggressive activity for the wild type zebrafish 
# One-sided, HA that mean aggressive actvity is greater for spd mutant
wilcox.test(mean_AggressiveActivity ~ genotype, data = summ_zebra, alternative = "greater", conf.level = 0.95)

# #16 (b)
# Read in data
library(readr)
paired_test_zebrafish <- read_csv("datasets/demos/paired_test_zebrafish.csv")
View(paired_test_zebrafish)

# diff = rate of aggresive activity in spd mutant - rate of aggressive activity in wild type 

# Paired t-test 
# Two-sided
t.test(paired_test_zebrafish$Spd_mutant, paired_test_zebrafish$Wild_type, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# P-value= 0.0009, reject null hypothesis
# HO: The difference between the two means is equal to zero 
# HA: The difference between the two means is not equal to zero 
# P<0.05, reject the null hypothesis
# Paired t-test, two-sided, p-value = 0.0009, there is a significant diffence between the mean rate of aggressive activity in zebrafish that contained the spd mutation and the mean rate of aggressive activity for wild type zebrafish. 

### 22/24 b/c 16b should not be paired t-test