# 2 mapping by LSOA

library(tidyverse)
library(sf)
library(mosaic)
library("ggspatial")

# get data abd tidy
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

# Map by LSOA
# drop unwanted cols
data =
  data %>% 
  select(-c(centrality,wards,postcode, )) %>% 
  rename(
    lsoa_name = 2,
    
  )

# get lsao codes, names and geoms

bristol = 
  sf::st_read('spatial/data/bristol-imd.shp') %>% 
  select(1) %>% 
  rename(lsoa_code = `LSOA11CD`)

#plot(sf::st_geometry(bristol))

lsoa = 
  read_csv('lsoa_codes.csv') %>% 
  rename(lsoa_code =`LSOA11 Code`,
         lsoa_name = 3) %>% 
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
   filter(n>=10) %>% 
   arrange(desc(lsoa_code)) %>% 
   st_as_sf(sf_column_name = "geometry") %>% 
  group_by(lsoa_code) %>% 
  summarise(n = n())

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




