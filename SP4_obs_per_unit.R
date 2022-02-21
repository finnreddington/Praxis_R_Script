# 4 - number of obs by unit area

library(tidyverse)
library(kableExtra)
library(knitr)

# get data abd tidy
data = read_csv("F:/Praxis/online_data.csv") %>% 
  select(1,3,5,4,7,9,2) %>% 
  rename(
    ID = `Respondent ID`,
    centrality = `Centre/Periphery`,
    wards = `Wards`,
    postcode_3 = `Postcode 3 digits`,
    postcode = `Postcode cleaned`,
    deprivation = X2
  ) %>% 
  slice(-1)

# count by centrality

data %>% 
  filter(centrality != c('#N/A')) %>% 
  group_by(centrality) %>% 
  summarise(n = count(centrality)) %>% 
  slice(-1) %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  kbl(escape = F, digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped"), full_width = TRUE) %>% 
  column_spec(column = 1, bold = TRUE) %>% 
  row_spec(0,bold=TRUE)  %>% 
  save_kable(file = "n_obs_cen.png",
             zoom = 2)


# ward
data %>% 
  filter(wards != c('#N/A')) %>% 
  group_by(wards) %>% 
  summarise(n = count(wards)) %>% 
  slice(-1) %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  kbl(escape = F, digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped"), full_width = TRUE) %>% 
  column_spec(column = 1, bold = TRUE) %>% 
  row_spec(0,bold=TRUE)  %>% 
  save_kable(file = "n_obs_wards.png",
             zoom = 2)


# PC 3
data %>% 
  filter(postcode_3 != c('#N/A')) %>% 
  group_by(postcode_3) %>% 
  summarise(n = count(postcode_3)) %>% 
  slice(-1) %>% 
  arrange(desc(n))%>% 
  slice(1:5) %>% 
  kbl(escape = F, digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped"), full_width = TRUE) %>% 
  column_spec(column = 1, bold = TRUE) %>% 
  row_spec(0,bold=TRUE)  %>% 
  save_kable(file = "n_obs_pc3.png",
             zoom = 2)

# LSOA
data %>% 
  filter(LSOA != c('#N/A')) %>% 
  group_by(LSOA) %>% 
  summarise(n = count(LSOA)) %>% 
  slice(-1) %>% 
  arrange(desc(n))%>% 
  slice(1:5) %>% 
  kbl(escape = F, digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped"), full_width = TRUE) %>% 
  column_spec(column = 1, bold = TRUE) %>% 
  row_spec(0,bold=TRUE)  %>% 
  save_kable(file = "n_obs_lsoa.png",
             zoom = 2)

# deprivation decile

dep = data %>% 
  filter(deprivation != '#N/A') %>% 
  separate(deprivation, into=c('deprivation', 'y'), sep = " - ") %>% 
  select(-y)
  
  
  
dep %>% 
  group_by(deprivation) %>% 
  summarise(n = count(deprivation)) %>% 
  arrange(desc(n))%>% 
  kbl(escape = F, digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped"), full_width = TRUE) %>% 
  column_spec(column = 1, bold = TRUE) %>% 
  row_spec(0,bold=TRUE)  %>% 
  save_kable(file = "n_obs_dep.png",
             zoom = 2)
