### Lab 3. Data manipulation and graphing

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# Read in data file
ward_data<-read_csv("datasets/quinn/chpt3/ward.csv", col_names = TRUE)

library(readr)
ward <- read_csv("datasets/quinn/chpt3/ward.csv")
View(ward)

# Pasted from Import Dataset Tool
# Note that for us, library(readr) is redundant because we loaded it with
# all the other tidyverse packages earlier
library(readr)
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# Read in compensation data file
compensation<-read_csv("~/Documents/Analyses /compensation.xlsx")

# names() tells you the names assigned to each column, generally variable
# names
names(sanchez_csv)

# head() gives you the first six rows of a dataset
head(sanchez_csv)

# dim() gives you the dimensions of your dataset
dim(sanchez_csv)

# str() returns the structure of the dataset
str(sanchez_csv)

# Calculate summary statistics about groups.  I give the general form below
# in comments

# <new_object_name> <- <data> %>%
# group_by(<grouping_variable>) %>%
# summarise(
# mean_resp = mean(<response_variable_name>),
# median_resp = median(<response_variable_name>),
# IQR_resp = IQR(<response_variable_name>),
# sd_resp = sd(<response_variable_name>),
# var_resp = var(<response_variable_name>)
# )

summ_shrimp_data <- shrimp_data
group_by('Form')
summarise(mean_body_length = mean('Body Length'),
median_body_length = median('Body Length'),
IQR_body_length = IQR('Body Length'),
sd_body_length = sd('Body Length'),
var_body_length = var('Body Length')
)

summ_eggs <- ward %>%
group_by(ZONE) %>% 
  summarise(mean_eggs = mean(EGGS),
            median_eggs = median(EGGS),
            IQR_eggs = IQR(EGGS),
            sd_eggs = sd(EGGS),
            var_eggs = var(EGGS))

View(summ_eggs)

summarise(shrimp_data)

summ_shrimp_data <- shrimp_data
group_by(form)
  summarise(mean_body_length = mean(body_length),
            median_body_length = median(body_length),
            IQR_body_length = IQR(body_length),
            sd_body_length = sd(body_length),
            var_body_length = var(body_length))
summarise(summ_shrimp_data)

summarise(shrimp_data)

sum(shrimp_data)


# mutate() adds new variables while preserving existing ones.  General form:
# <dataset_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))
ward<-mutate(ward, squareroot_eggs = sqrt(EGGS))

sanchez_csv<-mutate(sanchez_csv, transformed_beetle_densities = log(x+1) = beetle_density)

summarise(compensation)

# R for Data Science, Chapter 3
# https://r4ds.had.co.nz/data-visualisation.html
# Enter your code here

mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

<<<<<<< HEAD



### MISSING R FOR DATA SCIENCE CODE #####



=======
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
>>>>>>> 9d8850de65bf79e29061b82f0fec3e0016213e1b

# Compare the histograms and boxplots of EGGS and squareroot_eggs
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)
ggplot(ward) +
  geom_histogram(aes(squareroot_eggs), binwidth = 0.5)+
  facet_wrap(~ZONE)

ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = EGGS), notch = TRUE, varwidth = TRUE)
ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = squareroot_eggs), notch = TRUE, varwidth = TRUE)

### Assignment

# Load the sanchez.csv file
# Enter your code here

library(readxl)
sanchez_csv <- read_excel("~/Documents/Analyses /sanchez.csv.xlsx")
View(sanchez_csv)




# Calculate summary statistics
# Enter your code here

summ_beetle_density <- sanchez_csv 
  group_by(`Bird Colony`) 
  summarise(mean_beetle_density = mean(`Beetle Density`),
            median_beetle_density = median(`Beetle Density`),
            IQR_beetle_density = IQR(`Beetle Density`),
            sd_beetle_density = sd(`Beetle Density`),
            var_beetle_density = var(`Beetle Density`))
  
summ_shrimp_data_2 <- shrimp_data
  group_by('Form')
  summarise(mean_body_length = mean('Body Length'),
            median_body_length = median('Body Length'),
            IQR_body_length = IQR('Body Length'),
            sd_body_length = sd('Body Length'),
            var_body_length = var('Body Length'))

# Add a new column of log(y+1) transformed beetle densities to the sanchez dataset
# Enter your code here

library(readxl)
sanchez_csv <- read_excel("~/Documents/Analyses /sanchez.csv.xlsx")
View(sanchez_csv)

sanchez_csv<-mutate(sanchez_csv, log(y+1)(beetle_density))

library(readxl)
shrimp_data <- read_excel("~/Documents/Analyses /shrimp_data.xlsx")
View(shrimp_data)

# Generate histograms of beetle density by colony type before and after data 
# transformation
# Enter your code here

ggplot(sanchez_csv)+
  geom_histogram(aes(beetle_density), binwidth = 2)+
  facet_wrap(~bird_colony)
ggplot(sanchez_csv)+
  geom_histogram(aes(log(y+1)), binwidth = 0.5)+
  facet_wrap(~bird_colony)



# Plot boxplots of beetle density by colony type before and after data 
# transformation
# Enter your code here

<<<<<<< HEAD
#### NOT COMPLETED BY DUE DATE ####
=======
ggplot(sanchez_csv)+
  geom_boxplot(aes(x = Bird_Colony, y = Beetle_Density), notch = TRUE, varwidth = TRUE)
ggplot(sanchez_csv)+
  geom_histogram(aes(x = Bird_Colony, y = log(y=1)), notch = TRUE, varwidth = TRUE)

names(sanchez_csv)
dim(sanchez_csv)
str(sanchez_csv)

col_character(Bird_Colony)
col_double(Beetle_Density)

data01 <- read_csv("datasets/abd/chapter12/chapter12e3HornedLizards.csv")
data01 <- data01 %>% slice(-105)
>>>>>>> 9d8850de65bf79e29061b82f0fec3e0016213e1b
