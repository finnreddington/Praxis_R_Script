# extract LSOA codes

# this script
# 1 - get obs w/postcode
# 2 - link postcodes to lsoa codes
# 3 - extract lsoa geometry
# first get the list of Bristol postcodes

library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(RColorBrewer)

data = read_csv("F:/Praxis/online_data.csv") %>% 
  rename(
    ID = `Respondent ID`,
    post_code = `Postcode cleaned`,
    lsoa_name = LSOA
  ) %>% 
  slice(-1) %>% 
  select(ID, post_code, lsoa_name) %>% 
# select only BS postcodes
  filter(grepl("BS", post_code)) 
# filter postcodes >=6

# load shapefiles
england = 
  sf::st_read('spatial/data/England_lsoa_2011/england_lsoa_2011.shp') %>%
  select(-label) %>% 
  rename(lsoa_code = code,
         lsoa_name = name)

colnames(england)
colnames(x)

head(england,3)
head(data)  

# bristol shp file
bristol = 
  sf::st_read('spatial/data/bristol-imd.shp') %>% 
  select(1) %>% 
  rename(lsoa_code = `LSOA11CD`)

# Task: join my data to spatial geometries
# option 1: use LSOA names
# option 2: use LSOA Codes obtained via postcode filter

# option 1: join with LSOA names
# make both lower case 
data$lsoa_name = str_to_lower(data$lsoa_name, locale = "en")
england$lsoa_name = str_to_lower(england$lsoa_name, locale = "en")

# join together
head(england,3)
head(data)  

x = inner_join(data, england, by=c('lsoa_name'='lsoa_name')) 
# no matches, option 1 failed

# Option 2: link names to codes and THEN link codes to shapefiles

#my_post_codes = 
#  as_tibble(data$postcode) %>% 
#  rename(post_code = value)
# length(unique(my_post_codes$post_code))
# 1140 unique postcodes in the data
# 1408 - 1140 = 268 are repeated

post_codes = read_csv('F:/Praxis/LSOA_lookup/england_output_areas.csv') %>% 
  rename(post_code = PC,
         lsoa_code = lsoa11cd
         ) %>% 
  select(post_code,lsoa_code,lsoa_name)

post_codes$post_code = str_to_lower(post_codes$post_code, locale = "en")
data$post_code = str_to_lower(data$post_code, locale = "en")


colnames(post_codes)
head(post_codes)
head(data)

# now extract the LSOA code and name based on the post code
y = inner_join(data, post_codes, by=c('post_code'='post_code'))

length(x$post_code)-1408
# lost 601 post_codes



# now filter the postcodes I have linked to LSOA codes
y = inner_join(y, england, by=c('lsoa_code'='lsoa_code')) 

length(unique(y$lsoa_code))
 
# now I have an item with LSAO codes, and ID

# make a rate
# summarise
count =
  y %>% 
  group_by(lsoa_name.x) %>% 
  mutate(n = n()) %>% 
  arrange(desc(lsoa_code)) %>% 
  st_as_sf(sf_column_name = "geometry")

class(count)

# plot places >10
# get bristol outline
bris_lsoa = 
  sf::st_read('spatial/data/bristol-imd.shp') %>% 
  select(1) %>% 
  rename(lsoa_code = `LSOA11CD`)

z = 
  count %>% 
  group_by(lsoa_code) %>% 
  summarise(n = n()) %>% 
  filter(n >= 10) %>% 
  inner_join(x, england, by=c('lsoa_code'='lsoa_code')) %>% 
  mutate(category = cut(n, breaks = c(0, 10, 15, 20, 25),
                        labels = c("0-10", "10-15", "15-20", ">20")))

head(z)
favstats(z$n)

# plot

#brewer.pal(5, "Reds")
my.cols = c("#FFFFFF","#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")

tiff(file='map_lsoas.tif',height=2000,width=2700,res=300)
ggplot()+
  geom_sf(data=bris_lsoa, fill="white")+
  geom_sf(data=z, aes(fill=category))+
  scale_fill_manual(values=my.cols)+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_orienteering) +
  ggtitle("No. Respondents by LSOA")+
  theme(plot.title = element_text(size=12),
  )+
  coord_sf(expand=FALSE)
dev.off()

count %>% 
  ggplot()+
  geom_histogram(aes(x = n), binwidth = 0.9, fill="darkblue")+
  scale_x_continuous(breaks = seq(0, 60, by = 2))+
  scale_y_continuous(breaks = seq(0, 150, by = 20))+
  ggtitle("Respondents per LSOA")+
  geom_vline(xintercept =10, col="red")+
  ggthemes::theme_igray()

x = 
  count %>% 
  mutate(waste = n>10) %>% 
  select(waste) 

round(sum(x$waste)/length(x$waste)*100,0)