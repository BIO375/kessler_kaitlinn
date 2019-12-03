library(readr)
aphids <- read_csv("datasets/exams/aphids.csv")
View(aphids)

model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)


### CODE BREAKS B/C DID NOT LOAD LIBRARY NLME ####
model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )

varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability




library(readr)
glucose <- read_csv("datasets/exams/glucose.csv")
View(glucose)




library(readr)
DriverVision <- read_csv("datasets/exams/DriverVision.csv")
View(DriverVision)


### CODE BREAKS BECAUSE DID NOT LOAD LIBRARY GGFORTIFY ####
model01 <- lm(Distance ~ Age, data = DriverVision)
autoplot(model01, smooth.colour = NA)


### CODE BREAKS BECAUSE DID NOT LOAD LIBRARY TIDYVERSE ####
DriverVision <- augment(model01)
ggplot(data = DriverVision)+
  geom_point(aes(x = Age, y= .resid))

summary(model01)

ggplot(data = DriverVision, aes(x = Age, y = Distance)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Age", y = "Distance")

#### Code breaks because libraries not loaded 4/5 ####
