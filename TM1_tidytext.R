# Ch1 - Tidy Text

library(tidyverse)
library(tidytext)

# Get data
data <- read_csv("F:/Praxis/online_data.csv")

# drop unwanted columns
data = select(data, 1,11:20)

# rename columns
data =
  data %>% rename(
    ID = `Respondent ID`,
    Q1 = 2,
    Q2 = 3,
    Q3 = 4,
    Q4 = 5,
    Q5 = 6,
    Q6 = 7,
    Q7 = 8,
    Q8 = 9,
    Q9 = 10,
    Q10 = 11
  )
# drop 1st row
data = 
  data %>% slice(-1)

# pivot longer
data = 
data %>% 
  pivot_longer(2:11, 
               names_to = 'question',
               values_to = 'word')

# tokenization
tokens = data %>% 
  unnest_tokens('word', word) 

# remove stop words
data("stop_words")

tokens = tokens %>% 
  anti_join(stop_words, by = c("word" = "word"))

# remove NAs
tokens = tokens %>% 
  drop_na()

# Count most used in whole dataset

count =
tokens %>%
  count(word, sort = TRUE) %>% 
  arrange(desc(n))

# plot most common words
count = tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n))

count %>% 
  ggplot(aes(n, word)) +
  geom_col(fill = 'steel blue') +
  labs(y = NULL)+
  ggtitle("Most Common Words in Climate Survey")+
  scale_x_continuous(name="n", 
                     breaks = c(seq(from = 0, to = 1400, by = 200)),)+

        axis.title.y = element_blank(),
        title = element_text(face="bold", color="#993333", 
                             size=14))+
  ggsave('most_common_words.png')




