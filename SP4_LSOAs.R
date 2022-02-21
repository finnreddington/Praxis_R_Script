# 2 mapping by LSOA

library(tidyverse)
library(sf)
library(mosaic)
library("ggspatial")
library(stringr)

# get data abd tidy
data = read_csv("F:/Praxis/online_data.csv") %>% 
  select(1,4) %>% 
  rename(
    ID = `Respondent ID`,
    lsoa_name = `LSOA`
  ) %>% 
  slice(-1) %>% 
  filter(lsoa_name != '#N/A')

data$lsoa_name = str_to_lower(data$lsoa_name, locale = "en")


# how many LSOAs in data set?
length(unique(data$lsoa_name))
# A = 379

england = 
  #sf::st_read('spatial/data/England_lsoa_2011/england_lsoa_2011.shp') %>%
  england %>% 
  select(-label) %>% 
  rename(lsoa_code = code,
         lsoa_name = name)
  separate(name, into=c("lsoa_name", "y"), sep=" 0") %>% 
  select(-y)

england$lsoa_name = str_to_lower(england$lsoa_name, locale = "en")

colnames(england)
head(england,3)

length(unique(england$lsoa_code))

# extract y from x based on name
x = dplyr::filter(england, grepl("bristol",lsoa_name))




names = as.vector(unique(data$lsoa_name))

x = semi_join(data, england, by=c('lsoa_name'='lsoa_name'))

x = 
  england %>% 
  filter(lsoa_name %in% names)

# join data to lsoa codes by name
  inner_join(bristol, lsoa, by = c('lsoa_code' = 'lsoa_code'))

rm(bristol)

# now filter codes based on lsoa names
data = inner_join(data, lsoa, by=c('lsoa_name'='lsoa_name'))

# summarise
count =
   data %>% 
   group_by(lsoa_code) %>% 
   mutate(n = n()) %>% 
   arrange(desc(lsoa_code)) %>% 
   st_as_sf(sf_column_name = "geometry")

class(count)

# plot number of observation per lsoa

ggplot2::theme_set(theme_bw())

tiff(file='map_LSOA.tif',height=2000,width=2700,res=300)
count %>% 
  ggplot()+
  geom_sf(aes(fill=n))+
  scale_fill_viridis_b(option = "plasma", 
                       name="Responses")+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_orienteering) +
  ggtitle("Number of Climate Surveys by LSOA")+
  theme(plot.title = element_text(size=12),
        #legend.title = element_text(size=9),
        #legend.text = element_text(size=8),
        #legend.key.size = unit(0.4, "cm"),
        #legend.key.width = unit(0.2,"cm"))
)

dev.off()




