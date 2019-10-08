Question 1. 

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
X2019_10_01_Lab5 <- read_csv("datasets/demos/2019-10-01_Lab5.csv")
View(X2019_10_01_Lab5)

# First step, calculate t_sample.  You will need to define what the sample mean, null hypothesis mean, sample 
# standard deviation, and sample size are.  
null_mean <- 23.4722

# If you are given the values for the sample mean, sd, and n, you can simply define each value as an object 
# in the environment
sample_mean <- 39.3
sample_sd <- 30.7
sample_n <- 31
df <- sample_n -1

# If you are given raw data, read in the data file and define each summary statistic with a simple equation
# Note: you can't use summarise here because it will create a table instead of named objects.

# Read in data
X2019_10_01_Lab5 <- read_csv("datasets/demos/2019-10-01_Lab5.csv")

# Identify your response variable using the form dataset$variable_name
y<-X2019_10_01_Lab5$Obliquity

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Two-sided
t.test(X2019_10_01_Lab5$Obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

Question 2. 

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

  
HeartAttack_short<-read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
               col_types = cols(
                 group = col_character() )
)

# Identify your response variable using the form dataset$variable_name
y<-HeartAttack_short$cholest

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Look at the summary statistics
summ_HA <- HeartAttack_short %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_HA$sd_cholest))/(min(summ_HA$sd_cholest))

# Look at histograms, box plots, q-q plots
ggplot(HeartAttack_short) +
  geom_histogram(aes(cholest), binwidth = 20)+
  facet_wrap(~group)

ggplot(HeartAttack_short) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(HeartAttack_short)+
  geom_qq(aes(sample = cholest, color = group))

# Two-sided
t.test(cholest ~ group, data = HeartAttack_short, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

wilcox.test(cholest ~ group, data = HeartAttack_short, alternative = "two.sided", conf.level = 0.95)

Question 3. 

library(readr)
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)

# Identify your response variable using the form dataset$variable_name
y<- furness$METRATE

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Look at the summary statistics
summ_FUR <- furness %>%
  group_by(SEX) %>% 
  summarise(mean_metrate = mean(METRATE),
            sd_metrate = sd(METRATE),
            n_metrate = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_FUR$sd_metrate))/(min(summ_FUR$sd_metrate))

# Look at histograms, box plots, q-q plots
ggplot(furness) +
  geom_histogram(aes(METRATE), binwidth = 100)+
  facet_wrap(~SEX)

ggplot(furness) +
  geom_boxplot(aes(x = SEX, y = METRATE))

ggplot(furness)+
  geom_qq(aes(sample = METRATE, color = SEX))

# Two-sided
wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

Question 4. 

library(readr)
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")
View(elgar)

## Plot code breaks b/c you need to put line 164 before line 162

untidy_elgar <- mutate(untidy_elgar, diff = afterHORIZLIG - beforeHORIZDIM)

untidy_elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")

ggplot(untidy_elgar) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(untidy_elgar) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(untidy_elgar)+
  geom_qq(aes(sample = diff))

library(readr)
elgar <- read_csv("datasets/demos/elgar.csv")
View(elgar)

# Two-sided
t.test(untidy_elgar$HORIZDIM, untidy_elgar$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)






