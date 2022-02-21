# Ch. 2 - Sentiment Analysis

library(tidytext)
library(textdata)
library(reshape2)
library(wordcloud)
library(quanteda)
library(textplot)
library(tidyverse)

# get data and tidy
data <- read_csv("F:/Praxis/online_data.csv")
data = select(data, 1,11:20)
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
data = 
  data %>% 
  pivot_longer(2:11, 
               names_to = 'question',
               values_to = 'word')
# drop na's and order Q's
data %>% drop_na()

data$question <- 
  factor(data$question,
         levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))


# download sentiments
afinn = get_sentiments("afinn")
nrc = get_sentiments("nrc") 

# filter for 'sadness'
nrc_sadness <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

sadness = tokens %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = TRUE)

# look at net positive/negative of text:
climate_sentiment <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(question, index = ID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  drop_na()

# plot
# factor first
climate_sentiment$question <- 
  factor(climate_sentiment$question,
         levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))


ggplot(climate_sentiment, aes(x=question, y=sentiment, fill = question)) +
  geom_col(show.legend = FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ggtitle("Negativity/Positivity Scores by Question")+
  xlab("Question")+
  scale_y_continuous(name="Bing Sentiment Score", 
                    breaks = c(seq(from = -2000, to = 2000, by = 400)),)+
  theme(axis.text.x = element_text(color="black", 
                                   size=11),
        axis.title.x = element_text(color='black', size = 12))

# See how much particular words contibuted to the sentiment
bing_counts <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# plot this
bing_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)+
  theme(axis.title.x = element_text(size = 11, colour = 'black'))

# Comparison clouuds

x = tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) 
  
comparison.cloud(x, colors = c("red", "green"),
                   max.words = 100)

# Analyze at sentance level
## [This section didn't work very well but seems not too relevent anyway]
sentences = data %>% 
  unnest_tokens(sentence, word, token = "sentences")

# Analyze sentances by question
# get lexicon
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- sentences %>%
  group_by(question) %>%
  summarize(sentences = n())

x = sentences %>%
  semi_join(bingnegative, by=c("sentence" = "word")) %>%
  group_by(question) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = "question") %>%
  mutate(ratio = negativewords/words) %>%
  ungroup()

