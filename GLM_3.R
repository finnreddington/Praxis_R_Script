# GLM_3 - any news source and any emotion

library(tidyverse)
library(lme4)

# read and format data
data = 
  read_csv('GLM_3.csv') %>% 
  drop_na("ID_1") %>% 
  drop_na("ID_2") %>% 
  replace(is.na(.), 0)

# split into 2 dataframes
data_media = 
  data %>% 
  select(ID_1:science) %>% 
  rename(ID = ID_1)

data_feels = 
  data %>% 
  select(ID_2:responsibility) %>% 
  rename(ID = ID_2)

# join by common ID re-order and drop NA rows
my_data = 
  left_join(data_media, data_feels, by="ID") %>% 
  drop_na() %>% 
  pivot_longer(cols = social_media:science, 
               names_to = "media_type",
               values_to = "value") %>% 
  filter(value==1) %>% 
  select(-value)

cols = colnames(my_data)
my_data[cols] <- lapply(my_data[cols], factor) 
sapply(my_data, class)

# glm - anxiety
mod = glm(anxiety ~ 0 + media_type,
          family="binomial",
          data=my_data)
summary(mod)

scenario = 
  as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
  rename(media_type = 1) %>% 
  arrange(media_type)
  
predictions = round(predict(mod, scenario, type="response"), 2)
scenario$probs = predictions

p_values = coef(summary(mod))[,4]
scenario$p_values = p_values

results_anxiety = scenario %>% 
  filter(p_values<0.05)



# glm - angry

{
mod = glm(angry ~ 0 + media_type,
          family="binomial",
          data=my_data)
summary(mod)

scenario = 
  as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
  rename(media_type = 1) %>% 
  arrange(media_type)

predictions = round(predict(mod, scenario, type="response"), 2)
scenario$probs = predictions

p_values = coef(summary(mod))[,4]
scenario$p_values = p_values

results_anger = scenario %>% 
  filter(p_values<0.05)
}

# glm - helpless

{
  mod = glm(helpless ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_helpless = scenario %>% 
    filter(p_values<0.05)
}
# glm - active

{
  mod = glm(active ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_active = scenario %>% 
    filter(p_values<0.05)
}
# glm - no_emotion

{
  mod = glm(no_emotion ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_no_emotion = scenario %>% 
    filter(p_values<0.05)
}

# glm - future

{
  mod = glm(future ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_future = scenario %>% 
    filter(p_values<0.05)
}

# glm - sad
{
  mod = glm(sad ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_sad = scenario %>% 
    filter(p_values<0.05)
}

# glm - responsibility
{
  mod = glm(responsibility ~ 0 + media_type,
            family="binomial",
            data=my_data)
  summary(mod)
  
  scenario = 
    as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
    rename(media_type = 1) %>% 
    arrange(media_type)
  
  predictions = round(predict(mod, scenario, type="response"), 2)
  scenario$probs = predictions
  
  p_values = coef(summary(mod))[,4]
  scenario$p_values = p_values
  
  results_responsibility = scenario %>% 
    filter(p_values<0.05)
}
