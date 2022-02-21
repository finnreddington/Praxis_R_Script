# 3 - mapping by ward

library(stringr)
library(tidyverse)
library(sf)
library(mosaic)
library("ggspatial")
library(RColorBrewer)

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
  slice(-1) %>% 
  select(-c(centrality, LSOA, postcode, deprivation)) %>% 
  rename(ward_name = 2) %>%
  filter(ward_name != "#N/A")

head(data)

# get lsao codes

bris_wards = 
  sf::st_read('spatial/wards/wards.shp') %>% 
  select(-c(1,3)) %>% 
  rename(ward_name = name)
head(bris_wards)

#plot(sf::st_geometry(bristol))

# make same case and convert '&' to 'and'

bris_wards$ward_name = str_to_lower(bris_wards$ward_name, locale = "en")
bris_wards$ward_name = str_replace(bris_wards$ward_name, "&", "and")

# now filter codes based on lsoa names
x = inner_join(data, bris_wards, by=c('ward_name'='ward_name'))
x = anti_join(data, bris_wards, by=c('ward_name'='ward_name'))

head(bris_wards)
head(data)

# summarise
count =
  data %>% 
  group_by(ward_name) %>% 
  mutate(n = n()) %>% 
  arrange(desc(ward_name)) %>% 
  st_as_sf(sf_column_name = "geometry")

head(count)

z = 
  count %>% 
  group_by(ward_name) %>% 
  summarise(n = n()) %>% 
  filter(n >= 10) %>% 
  inner_join(data, bris_wards, by=c('ward_name'='ward_name')) %>% 
  mutate(category = cut(n, breaks = c(0, 10, 20, 30, 40, Inf),
                        labels = c("0-10", "10-20", "20-30", "30-40", ">40")))

head(z)
favstats(z$n)

# plot
my.cols = c("#FFFFFF","#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")

tiff(file='map_wards.tif',height=2000,width=2700,res=300)
ggplot()+
  geom_sf(data=bris_wards, fill="white")+
  geom_sf(data=z, aes(fill=category))+
  scale_fill_manual(values=my.cols)+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_orienteering) +
  ggtitle("No. Respondents by wards")+
  theme(plot.title = element_text(size=12),
  )+
  coord_sf(xlim = c(-2.74, -2.5), 
           ylim = c(51.39, 51.55),
           expand = FALSE)
dev.off()

# hist and stats on geog unit
wards = count %>% 
  group_by(ward_name) %>% 
  summarise(n = n()) %>% 
  filter(n>=10) %>% 
  sf::st_drop_geometry()

favstats(wards$n)


count %>% 
  ggplot()+
  geom_histogram(aes(x = n), binwidth = 0.9, fill="darkblue")+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  scale_y_continuous(breaks = seq(0, 80, by =10))+
  ggtitle("Respondents per Ward")+
  geom_vline(xintercept =10, col="red")+
  ggthemes::theme_igray()

x = 
  count %>% 
  mutate(waste = n>10) %>% 
  select(waste) 

round(sum(x$waste)/length(x$waste)*100,0)





