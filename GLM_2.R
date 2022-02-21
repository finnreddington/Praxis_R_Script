# GLM 2 - social media and other emotions

library(tidyverse)
library(lme4)

# read and format data
data = 
  read_csv('GLM_2.csv') %>% 
  drop_na("ID_1") %>% 
  drop_na("ID_2") %>% 
  replace(is.na(.), 0)

data$social_media =  as_factor(data$social_media)

# split into 2 dataframes
data1 = 
  data %>% 
  select(ID_1, social_media) %>% 
  rename(ID = ID_1)

data2 = 
  data %>% 
  select(3:7) %>% 
  rename(ID = ID_2)

# join by common ID re-order and drop NA rows
data3 = 
  left_join(data1, data2, by="ID") %>% 
  drop_na()

# make into factors
cols = colnames(data3)
data3[cols] <- lapply(data3[cols], factor) 
sapply(data3, class)

# GLM time
# anxiety
tab = xtabs( ~ anxiety+social_media, data = data3)
tab
chisq.test(tab)


glm = 
  glm(anxiety ~ 1 + social_media, 
      family = "binomial", data = data3)

summary(glm)
# positive increase in the log-odds of someone mentioning anxiety
# if they report social media being where they get info from 

# scenario
# first build a scenario
x = as.factor(unique(data$social_media))
scenario = tidyr::expand_grid(social_media=x)

scenario
round(predict(glm, scenario, type = "response"), 2)

# people who report getting info from social media are 7% more likely to 
# report feeling anxiety in response to climate change

# anger
tab = xtabs( ~ anger+social_media, data = data3)
tab
chisq.test(tab)

glm = 
  glm(anger ~ 1 + social_media, 
      family = "binomial", data = data3)

summary(glm)
round(predict(glm, scenario, type = "response"), 2)

# reporting of anger is NOT associated with social media

# sadness
tab = xtabs( ~ sad+social_media, data = data3)
tab
chisq.test(tab)

glm = 
  glm(sad ~ social_media, 
      family = "binomial", data = data3)

summary(glm)

round(predict(glm, scenario, type = "response"), 2)

# reporting of sadness is NOT associated with social media

# activeness
tab = xtabs( ~ active + social_media, data = data3)
tab
chisq.test(tab)

glm = 
  glm(active ~ social_media, 
      family = "binomial", data = data3)

summary(glm)
round(predict(glm, scenario, type = "response"), 2)

# reporting of active-ness is NOT associated with social media
