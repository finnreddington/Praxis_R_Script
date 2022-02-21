# GLM 1 - social media use and anxiety

library(tidyverse)
library(lme4)
library(broom.mixed)

# read and format data
data = read_csv('GLM_1.csv')

data$social_media = 
     data$social_media %>%  replace_na(0)

data$social_media =  as_factor(data$social_media)

data$anxiety = 
  data$anxiety %>% replace_na(0)

# split into 2 dataframes
data1 = 
data %>% 
  select(ID_1, social_media) %>% 
  rename(ID = ID_1)

data2 = 
  data %>% 
  select(ID_2, anxiety) %>% 
  rename(ID = ID_2)

data$anxiety =  as_factor(data$anxiety)


# join by common ID re-order and drop NA rows
data = 
  left_join(data1, data2, by="ID") %>% 
  drop_na()

# cross-tabulation
tab = xtabs( ~ anxiety+social_media, data = data)
chisq.test(tab)
# suggests there is a difference

# GLM time

glm = 
  glm(anxiety ~ 1 + social_media, 
          family = binomial(), data = data)

summary(glm)
knitr::kable(broom::tidy(glm))

# positive increase in the log-odds of someone mentioning anxiety
# if they report social media being where they get info from 

# scenario
# first build a scenario

x = as.factor(unique(data$social_media))
# y = as.factor(unique(data$anxiety))
scenario = tidyr::expand_grid(social_media=x)
scenario

round(predict(glm, scenario, type = "response"), 2)

# people who report getting info from social media are 7% more likely to 
# report feeling anxiety in response to climate change