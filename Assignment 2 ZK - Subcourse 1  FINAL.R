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

data_pain <- Data_1_sample

view(data_pain)

data_pain_exclude <- data_pain [-c(93),]

data_pain_changed <- data_pain_exclude %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait == "3.9", 39))

view(data_pain_changed)

#Data and Model diagnostics:
  
  Plot1 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = age, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot1


Plot2 <- data_pain_changed %>% 
  select(sex, pain) %>% 
  ggplot() + 
  aes(x = sex, 
      y = pain) + 
  geom_boxplot()

Plot2

Plot3 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = STAI_trait, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot3

Plot4 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = pain_cat, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot4

Plot5 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = mindfulness, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot5

Plot6 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = cortisol_serum, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot6

Plot7 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = weight, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot7

Plot8 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = IQ, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot8

Plot9 <- data_pain_changed %>% 
  ggplot() + 
  aes(x = household_income, 
      y = pain) + 
  geom_point() + 
  geom_smooth()

Plot9 

grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, nrow = 3)

data_pain_final <- data_pain_changed %>% 
  mutate(household_income = replace(household_income, household_income == "-3732", 3732))

view(data_pain_final)

full_model <- lm(pain ~age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, 
                 data = data_pain_final)

summary(full_model)

data_pain_final %>% ggplot() + aes(x = household_income, y = pain) + geom_point() + geom_smooth(method = "lm")

full_model %>% 
  plot(which = 5)

full_model %>% 
  plot(which = 4)

data_pain_final %>% slice(c(3, 102, 113))

#Assumptions of Normality 

full_model %>% plot(which = 2 )

#Skew and Kurtosis 

full_model_res = enframe(residuals(full_model)) 

full_model_res %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(full_model))


#Assumptions of Linearity

full_model %>% residualPlots()


#Assumptions of Homoscedasticity

full_model %>% plot(which = 3)

full_model %>% ncvTest()

full_model %>% bptest()


#Assumptions of Multicollinearity

full_model %>%
  vif()

full_model %>% 
  summary()

data_pain_final %>% select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, IQ, weight, household_income) %>% 
  pairs.panels(col = "Blue", lm = T)

AIC(full_model)

# Backwards regression of the full model

full_model_back = step(full_model, direction = "backward")

summary(full_model_back)

backward_model = lm(pain~age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_pain_final)

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_pain_final)

summary(theory_based_model)

# Standardised Beta for the backwardmodel

lm.beta(backward_model)

confint(backward_model)


# Comparing initial model (full model) and the backwardsregression model


summary(full_model)

summary(backward_model)

AIC(full_model)

AIC(backward_model)


# Comparing theory-based model and backward model 

summary(backward_model)$adj.r.squared

summary(theory_based_model)$adj.r.squared

AIC(theory_based_model, backward_model)


summary(backward_model)
summary(theory_based_model)

# Trying the models on a new dataset

Home_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

data_pain2 = Home_sample_2

view(data_pain2)

# Predicted values for each model on a new dataset. 

pred_theorybased_model <- predict(theory_based_model, data_pain2)

pred_backward_model <- predict(backward_model, data_pain2)

# Calculating the Sum of squared residuals

RSS_theorybased = sum((data_pain2[, "pain"] - pred_theorybased_model)^2) 
RSS_backward = sum((data_pain2[, "pain"] - pred_backward_model)^2)
RSS_theorybased
RSS_backward
