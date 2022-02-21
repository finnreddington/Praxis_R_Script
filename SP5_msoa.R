# extract MSOA codes

# this sheet
# 1 - get obs w/postcode
# 2 - link postcodes to msoa codes
# 3 - extract msoa geometry
# first get the list of Bristol postcodes

library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(RColorBrewer)
library(stringr)

# get respondents
data = read_csv("F:/Praxis/online_data.csv") %>% 
  rename(
    ID = `Respondent ID`,
    post_code = `Postcode cleaned`
  ) %>% 
  slice(-1) %>% 
  select(ID, post_code)


data$post_code = str_to_lower(data$post_code, locale = "en")


# get all uk postcodes
post_codes = read_csv('F:/Praxis/LSOA_lookup/england_output_areas.csv') %>% 
  rename(post_code = PC,
         msoa_code = msoa11cd
  ) %>% 
  select(post_code,msoa_code,msoa_name)

post_codes$post_code = str_to_lower(post_codes$post_code, locale = "en")

colnames(post_codes)
head(post_codes)
class(post_codes)

# now extract the MSOA code and name based on the post code
x = inner_join(data, post_codes, by=c('post_code'='post_code'))
head(x)

## NOTE: 593 obs get lost at this stage
# perhaps many postcodes from our dataset are not in the postcode lookup
# how many NAs?
#sum(is.na(data$post_code))
# zero
# How many incomplete postcode?
#short_PCs = subset(data, nchar(as.character(post_code)) <= 3)
# 84 are unavailable
#long_PCs = subset(data, nchar(as.character(post_code)) > 3)
# x = inner_join(long_PCs, post_codes, by=c('post_code'='post_code'))
# x still has 815 postcodes, this means the lost ones are not the only problem
# there are still 509 respondents unaccounted for
# one option is to try another list of postcodes but they must be linked
# to LSOA's
# Also many respondents live outside Bristol
# tho this doesn't explain why their postcodes are not being found

# get lsoa geometry data
england = 
  sf::st_read('MSOA/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries.shp') %>%
  rename(msoa_code = msoa11cd) %>% 
  select(msoa_code)

head(england)
colnames(england)
colnames(x)

# now filter the postcodes I have linked to LSOA codes
y = inner_join(x, england, by=c('msoa_code'='msoa_code')) 

head(x)
head(y)

length(unique(y$msoa_code))

# now I have an item with SAO codes, and ID

# make a rate
# summarise
count =
  y %>% 
  group_by(msoa_code) %>% 
  mutate(n = n()) %>% 
  arrange(desc(msoa_code)) %>% 
  st_as_sf(sf_column_name = "geometry")

head(count)
plot(sf::st_geometry(count))

# plot number of observation per lsoa
#world <- ne_countries(scale = "medium", returnclass = "sf")
#ggplot2::theme_set(theme_bw())

# tiff(file='map_LSOA.tif',height=2000,width=2700,res=300)
#ggplot(data=world)+
#  geom_sf()+
#  geom_sf(data=count, aes(fill=n))+
#  scale_fill_viridis_b(option = "plasma", 
#                       name="Responses")+
#  annotation_north_arrow(location = "tr", 
#                         which_north = "true",
#                         height = unit(0.5, "cm"),
#                         width = unit(0.5, "cm"),
#                         style = north_arrow_orienteering) +
# ggtitle("Number of Climate Surveys by MSOA")+
#  theme(plot.title = element_text(size=12),
#  )+
#  coord_sf(xlim = c(-5, 2), ylim = c(50, 52), expand = FALSE)
#dev.off()
# clearly not much use
# subset based on obs >=10

z = 
  count %>% 
  group_by(msoa_code) %>% 
  summarise(n = n()) %>% 
#  filter(n >= 10) %>% 
  inner_join(x, england, by=c('msoa_code'='msoa_code')) %>% 
  mutate(category = cut(n, breaks = c(0, 10, 20, 30, 40, Inf),
                        labels = c("0-10", "10-20", "20-30", "30-40", ">40")))
  
head(z)

# plot msoa's
bris_msoa = 
  sf::st_read('msoa_bristol/msoa01.shp') %>% 
  select(2)

# plot
library(RColorBrewer)
brewer.pal(5, "Reds")
my.cols = c("#FFFFFF","#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")

tiff(file='map_msoas.tif',height=2000,width=2700,res=300)
ggplot()+
  geom_sf(data=bris_msoa, fill="white")+
  geom_sf(data=z, aes(fill=category))+
  scale_fill_manual(values=my.cols)+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_orienteering) +
  ggtitle("No. Respondents by MSOA")+
  theme(plot.title = element_text(size=12),
  )+
  coord_sf(xlim = c(-2.75, -2.5), 
           ylim = c(51.39, 51.55),
           expand = FALSE)
dev.off()

# hist and stats on geog unit
msoa = count %>% 
  group_by(msoa_code) %>% 
  summarise(n = n()) %>% 
  filter(n>=10) %>% 
  sf::st_drop_geometry()

favstats(msoa$n)


count %>% 
ggplot()+
  geom_histogram(aes(x = n), binwidth = 0.9, fill="darkblue")+
  scale_x_continuous(breaks = seq(0, 60, by = 5))+
  scale_y_continuous(breaks = seq(0, 80, by =10))+
  ggtitle("Respondents per MSOA")+
  geom_vline(xintercept =10, col="red")+
  ggthemes::theme_igray()

x = 
  count %>% 
  mutate(waste = n>10) %>% 
  select(waste) 

round(sum(x$waste)/length(x$waste)*100,0)
