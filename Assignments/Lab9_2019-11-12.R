#### Lab 9: Correlation, Linear Regression #### 

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

library(readr)
fowler <- read_csv("datasets/demos/fowler.csv")
View(fowler)

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

ggplot(data = fowler)+
  geom_histogram(aes(FERTILIZER), binwidth = 50)
ggplot(data = fowler)+
  geom_histogram(aes(YIELD), binwidth = 20)

ggplot(data = fowler)+
  geom_boxplot(aes("", YIELD))
ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

## linear regression 

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

autoplot(model01, smooth.colour = NA)

# Option 1. Use the function augment
fowler_plus <- augment(model01)
ggplot(data = fowler_plus)+
  geom_point(aes(x = YIELD, y= .resid))

# Option 2.  Use the function resid() right in the plotting command
ggplot(data = fowler)+
  geom_point(aes(x = YIELD, y = resid(model01)))

# Option 3.  Use mutate() to add a residuals column to the original data
fowler <- fowler %>%
  mutate(fert_resid = resid(model01))
ggplot(data = fowler) +
  geom_point(aes(x = YIELD, y = fert_resid))

summary(model01)

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")








