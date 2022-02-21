# GLM Collinearity

library(tidyverse)
library(corrplot)
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
  rm(data, data_feels, my_data)
}


# another option is chi-square tests
media_names = colnames(data_media[-1])

# social_media correlation
{
CHIS <- lapply(data_media[,-data_media$social_media], function(x) chisq.test(data_media[,"social_media"], x)); CHIS
corr_social_media = as_tibble(rbindlist(lapply(CHIS, tidy), idcol=TRUE)) %>% 
  rename(ID = `.id`) %>% 
  select(ID, p.value)
corr_social_media$p.value = as.numeric(corr_social_media$p.value)
corr_social_media = 
  corr_social_media %>% 
  mutate(p_values = round(p.value, 4)) %>% 
  select(-p.value) %>% 
  filter(p_values<0.05)
}

# bcc correlation
{
  CHIS <- lapply(data_media[,-data_media$bcc], function(x) chisq.test(data_media[,"bcc"], x)); CHIS
corr_bcc = as_tibble(rbindlist(lapply(CHIS, tidy), idcol=TRUE)) %>% 
  rename(ID = `.id`) %>% 
  select(ID, p.value)
corr_bcc$p.value = as.numeric(corr_bcc$p.value)
corr_bcc = 
  corr_bcc %>% 
  mutate(p_values = round(p.value, 4)) %>% 
  select(-p.value) %>% 
  filter(p_values<0.05)
}

# local_news correlation
{
  CHIS <- lapply(data_media[,-data_media$local_news], function(x) chisq.test(data_media[,"local_news"], x)); CHIS
  corr_local_news = as_tibble(rbindlist(lapply(CHIS, tidy), idcol=TRUE)) %>% 
    rename(ID = `.id`) %>% 
    select(ID, p.value)
  corr_local_news$p.value = as.numeric(corr_local_news$p.value)
  corr_local_news = 
    corr_local_news %>% 
    mutate(p_values = round(p.value, 4)) %>% 
    select(-p.value) %>% 
    filter(p_values<0.05)
}

# internet correlation
{
  CHIS <- lapply(data_media[,-data_media$internet], function(x) chisq.test(data_media[,"internet"], x)); CHIS
  corr_internet = as_tibble(rbindlist(lapply(CHIS, tidy), idcol=TRUE)) %>% 
    rename(ID = `.id`) %>% 
    select(ID, statistic, p.value)
  corr_internet$p.value = as.numeric(corr_internet$p.value)
  corr_internet$statistic = as.numeric(corr_internet$statistic)
  
  corr_internet = 
    corr_internet %>% 
    mutate(p_values = round(p.value, 4),
           stat = round(statistic, 3)) %>% 
    select(-p.value, -statistic) %>% 
    filter(p_values<0.05)
}

# knitr 
corr_internet %>% 
  filter(ID != "internet") %>% 
  knitr::kable() %>% 
  kable_styling("striped") %>% 
  save_kable("result.png")

CHIS$social_media
