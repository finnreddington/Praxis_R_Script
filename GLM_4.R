# GLM_4 - figuring out contrasts and interpretting

library(tidyverse)
library(lme4)
library(knitr)
library(kableExtra)

{
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
}

# glm - anxiety
# mondel without intercept
mod1 = glm(anxiety ~ 0 + media_type,
          family="binomial",
          data=my_data)

# model with intercept
mod2 = glm(anxiety ~ 1 + media_type,
                 family="binomial",
                 data=my_data)

summary(mod1)
summary(mod2)

scenario = 
  as_tibble(read.csv("GLM_predictions_media1.csv"))%>% 
  rename(media_type = 1) %>% 
  arrange(media_type)

# compare log odds
logs_1 = round(coef(mod1), 2)
logs_2 = round(coef(mod2), 2)

scenario$logs_1 = logs_1
scenario$logs_2 = logs_2

# now compare probabilities
predictions_1 = round(predict(mod1, scenario, type="response"), 2)
scenario$probs1 = predictions_1

predictions_2 = round(predict(mod2, scenario, type="response"), 2)
scenario$probs2 = predictions_2

# compare p values

p_values_1 = coef(summary(mod1))[,4]
p_values_2 = coef(summary(mod2))[,4]
scenario$p_values_1 = p_values_1
scenario$p_values_2 = p_values_2

# compare the 'significant effects'
x = scenario %>% 
  filter(p_values_1 < 0.05)
y = scenario %>% 
  filter(p_values_2<0.05)

# fit as contrast is better (LJW) but harder to intepret
scenario %>% 
  select(media_type, probs1, p_values_1) %>% 
  arrange(desc(probs1)) %>% 
  filter(p_values_1 < 0.05) %>% 
  knitr::kable() %>% 
  kable_styling("striped") %>% 
  save_kable("result.png")

scenario %>% 
  select(media_type, probs1, p_values_1) %>% 
  arrange(desc(probs1)) %>% 
  filter(p_values_1 < 0.05) %>% 
  mutate("Diff_from_Mean" = (probs1-0.407)*100) %>% 
  mutate("Probability" = probs1*100) %>% 
  select(media_type, Probability, Diff_from_Mean) %>% 
  knitr::kable() %>% 
  kable_styling("striped") %>% 
  save_kable("result.jpeg")
  