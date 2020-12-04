# Packages
library(psych)
library(tidyverse)
library(r2glmm)
library(lme4)
library(lmerTest)
library(optimx)
library(MuMIn)
library(cAIC4)
library(lmtest)
library(gridExtra)
library(optimx)

# Datasets 
Data_sample3 <- read.csv("https://tinyurl.com/ha-dataset3") 

Data_sample4 <- read.csv("https://tinyurl.com/ha-dataset4") 

view(Data_sample3)

view(Data_sample4)

# Getting the standardized beta coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# Assigning hospital sites as a grouping factor 

Data_sample3 %>% mutate(hospital = factor(hospital))

Plot_1 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = age) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

Plot_2 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = sex) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

Plot_3 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = STAI_trait) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

Plot_4 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = pain_cat) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

Plot_5 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)  

Plot_6 <- Data_sample3 %>% 
  ggplot() +
  aes(y = pain, x = mindfulness) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

grid.arrange(Plot_1, Plot_2, Plot_3, Plot_4, Plot_5, Plot_6)

# Changing the errors in the sample (Data file 3) 

Data_sample3_corrected <- Data_sample3 %>% 
  mutate(sex = replace(sex, sex == "femlae", "female"))

view(Data_sample3_corrected)

summary(Data_sample3_corrected)

summary(Data_sample4)

# Explained variability between the hospitals 

Plot_Age = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = age, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_Age

Plot_Sex = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = sex, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_Sex

Plot_STAI = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = STAI_trait, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_STAI

Plot_cat = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = pain_cat, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_cat

Plot_cortisol = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_cortisol

Plot_mind = Data_sample3_corrected %>% ggplot() +
  aes(y = pain, x = mindfulness, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

Plot_mind

grid.arrange(Plot_Age, Plot_STAI, Plot_Sex, Plot_cat, Plot_cortisol, Plot_mind)

# Random intercept (Main effect)
Plot_Age+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

Plot_STAI+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

Plot_cat+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

Plot_cortisol+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

Plot_mind+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

# Building the model w predictors from assignment 1 and the random intercept effect 

Model_fixed = lm(pain ~sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = Data_sample3_corrected)

Model_random_int = lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = Data_sample3_corrected)

summary(Model_random_int)

confint(Model_random_int)

stdCoef.merMod(Model_random_int)

# R squared for Model_random_int including the confidence intervals

r2beta(Model_random_int, method = "nsj", data = Data_sample3_corrected)

r.squaredGLMM(Model_random_int)

summary(Model_random_int)

# Regression equation from data set 3 tested on data set 4

fixed_pred_model <- predict(Model_fixed, newdata = Data_sample4)

fixed_pred_model

mod_mean_data4 <- lm(pain ~ 1, data = Data_sample4)
mod_mean_data4

# The variance explained by the model on data file 4

RSS_data4 = sum((Data_sample4[, "pain"] - fixed_pred_model)^2)

RSS_data4

TSS = sum((Data_sample4$pain - predict(mod_mean_data4))^2)
TSS

R2 = 1-(RSS_data4/TSS)

R2

# Creating an own model with the most influential predictors. The models chosen based on CI of previous Model_rand_int were age and cortisol serum.

slope_plot_cortisol = Data_sample3_corrected%>%
  ggplot() +
aes(y = pain, x = cortisol_serum, color = hospital) + geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE) + xlim(-1, 10)+
  geom_hline(yintercept=0)+ geom_vline(xintercept=0)

slope_plot_cortisol

Model_rand_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), 
                               data = Data_sample3_corrected)

Model_rand_slope

Data_sample_slope = Data_sample3_corrected %>% 
  mutate(pred_slope = predict(Model_rand_slope))

View(Data_sample_slope)

Data_sample_slope %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+ geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+ facet_wrap( ~ hospital, ncol = 2)

