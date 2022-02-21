# 1 - tidying data for mapping

library(tidyverse)
library(knitr)
library(kableExtra)

# Get data
# Just keep ID's and geographic data for now (answers need processing)

data = read_csv("F:/Praxis/online_data.csv") %>% 
  select(1,3,5,4,9,2) %>% 
  rename(
    ID = `Respondent ID`,
    centrality = `Centre/Periphery`,
    wards = `Wards`,
    postcode = `Postcode cleaned`,
    deprivation = X2
  ) %>% 
  slice(-1)
