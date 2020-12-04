Data_1_sample = read.csv("https://tinyurl.com/ha-dataset1")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(lmtest)
library(psych)
library(sandwich)
library(lm.beta)

view(Data_1_sample)

data_pain= Data_1_sample

view(data_pain)

# Data and Model diagnostics:
  
Plot1 <- data_pain %>% 
  ggplot() + 
  aes(x = age, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot1
  
Plot2 <- data_pain %>% 
  select(sex, pain) %>% 
  ggplot() + 
  aes(x = sex, 
      y = pain) + 
    geom_boxplot()

Plot2

Plot3 <- data_pain %>% 
  ggplot() + 
  aes(x = STAI_trait, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot3

Plot4 <- data_pain %>% 
  ggplot() + 
  aes(x = pain_cat, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot4

Plot5 <- data_pain %>% 
  ggplot() + 
  aes(x = mindfulness, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot5

Plot6 <- data_pain %>% 
  ggplot() + 
  aes(x = cortisol_serum, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot6

Plot7 <- data_pain %>% 
  ggplot() + 
  aes(x = cortisol_saliva, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot7

grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, nrow = 3)
 
data_pain %>% 
  describe()

data_pain_fixed <- data_pain [-c(93),]

data_pain_fixed %>% 
  describe()

---> Changed a typo mistake on STAI_trait

data_pain_corrected <- data_pain_fixed %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait == "3.9", 39))

view(data_pain_corrected)

data_pain_corrected %>% 
  summary()

Model1 = lm(pain~age + sex, data = data_pain_corrected)

Model2 = lm(pain~age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_corrected)

Model2 %>% 
  summary()

# Model diagnostics

Model2 %>% plot(which = 5)

Model2 %>% plot(which = 4)

data_pain_corrected %>% slice(c(113, 99, 68))

summary(Model2)

lm(formula =pain~age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, 
   data = data_pain_corrected, 
   subset = -113) 

# Assumptions of Normality 

Model2 %>% plot(which = 2 )

Skew and Kurtosis 

residuals_model2 = enframe(residuals(Model2)) 

residuals_model2 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(Model2))

# Assumptions of Linearity

Model2 %>% residualPlots()

# Assumptions of Homoscedasticity

Model2 %>% plot(which = 3)

Model2 %>% ncvTest()

Model2 %>% bptest()

# Assumptions of Multicollinearity

Model2 %>%
  vif()

Model2 %>% 
  summary()

data_pain_corrected %>% select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>% 
  pairs.panels(col = "Blue", lm = T)

Model2_final = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_pain_corrected)

summary(Model2_final)

# Rerun checks of final model: 

Model2_final %>% plot(which = 5)

Model2_final %>% plot(which = 4)

Model2_final %>% plot(which = 2 )
  
residuals_model2_final = enframe(residuals(Model2_final)) 

residuals_model2_final %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(Model2_final))

Model2_final %>% residualPlots()

Model2_final %>% plot(which = 3)

Model2_final %>% ncvTest()

Model2_final %>% bptest()

Model2_final %>%
  vif()


# Reporting the results of model 1 and Model 2 

summary(Model1)

summary(Model2_final)

Model2_final

confint(Model1)

lm.beta(Model1)

lm.beta(Model2_final)

confint(Model2_final)

# Comparing the models

summary(Model1)$adj.r.squared 

summary(Model2_final)$adj.r.squared

AIC(Model1)

AIC(Model2_final)

anova(Model1, Model2_final)

