# 6 - missing obsrevations

library(tidyverse)

data = read_csv("F:/Praxis/online_data.csv") %>% 
  select(1,3,5,4,9,2) %>% 
  rename(
    ID = `Respondent ID`,
    centrality = `Centre/Periphery`,
    wards = `Wards`,
    post_code = `Postcode cleaned`,
    deprivation = X2,
    lsoa_name = LSOA
  ) %>% 
  slice(-1) %>% 
  select(ID, post_code, lsoa_name) %>% 
  # select only BS postcodes
  filter(grepl("BS", post_code)) %>% 
# filter postcodes >6
  filter(nchar(post_code) > 6)

data$post_code = str_to_lower(data$post_code, locale = "en")

# what about NAs?
sum(is.na(data$post_code))
# no na values in postcodes

# load shapefiles
england = 
  sf::st_read('spatial/data/England_lsoa_2011/england_lsoa_2011.shp') %>%
  select(-label) %>% 
  rename(lsoa_code = code,
         lsoa_name = name)

# make all lsoa_names lower case 
data$lsoa_name = str_to_lower(data$lsoa_name, locale = "en")
england$lsoa_name = str_to_lower(england$lsoa_name, locale = "en")

# bristol shp file
bristol = 
  sf::st_read('spatial/data/bristol-imd.shp') %>% 
  select(1) %>% 
  rename(lsoa_code = `LSOA11CD`)

# Option 1: join data to shape file by lsoa_name
x = inner_join(data, england, by=c('lsoa_name'='lsoa_name')) 
# failed, no matches

# Option 2: join data to shapefile with lsoa codes
# from postcode lookup

post_codes = read_csv('F:/Praxis/LSOA_lookup/england_output_areas.csv') %>% 
  rename(post_code = PC,
         lsoa_code = lsoa11cd
  ) %>% 
  select(post_code,lsoa_code,lsoa_name)

post_codes$post_code = str_to_lower(post_codes$post_code, locale = "en")

colnames(post_codes)
head(post_codes)
head(data)

# join postcodes to PC lookup
x = inner_join(data, post_codes, by=c('post_code'='post_code'))

# now join to shapefiles
# 1. bristol shapefile
y = inner_join(x, england, by=c('lsoa_code'='lsoa_code')) 

# 2. england shapefile
z = inner_join(x, england, by=c('lsoa_code'='lsoa_code')) 

head(x)
head(z)

# no data is lost at this stage

# summary
# 167 obs are lost by bening i) not in Bristol ii) incomplete postcodes
# 434 are lost as the postcodes in our data set are not in the
# LSOA lookup from gov