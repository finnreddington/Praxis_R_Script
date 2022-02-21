# Ch. 4 - Relationships Between Words

library(tidytext)
library(tidyverse)
library(forcats)
library(knitr)
library(kableExtra)
library(igraph)
library(ggraph)
library(widyr)

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

# bigrams
bigrams =
  data %>% 
  unnest_tokens(bigram, word, token = "ngrams", n=2) %>% 
  drop_na()

# count them

bigrams %>% 
  count(bigram, sort = TRUE)

# remove stop words

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# to rejoin
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Trigrams
trigrams = 
  data %>% 
  unnest_tokens(trigram, word, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>% 
  drop_na()

trigrams

trigrams %>% filter(word2 == 'arsed')

# tf-idf of bigrams
bigram_tf_idf <- bigrams_united %>%
  count(question, bigram) %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf))

# plot bigram tf-idf

bigram_tf_idf %>%
  filter(question == c("Q1", "Q2")) %>% 
  drop_na() %>% 
  group_by(question) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
    axis.text.y = element_text(color="black", 
                               hjust=0))
bigram_tf_idf %>%
  filter(question == c("Q9", "Q10")) %>% 
  drop_na() %>% 
  group_by(question) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
    axis.text.y = element_text(color="black", 
                               hjust=0))

# Trigram

trigrams =
  data %>% 
  unnest_tokens(trigram, word, token = "ngrams", n=3) %>% 
  drop_na()

# count them
# remove stop words
trigrams_separated <- trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

# to rejoin
trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

# tf-idf
trigram_tf_idf <- trigrams_united %>%
  count(question, trigram) %>%
  bind_tf_idf(trigram, question, n) %>%
  arrange(desc(tf_idf))

# plot
trigram_tf_idf %>%
  filter(question == c("Q7")) %>% 
  drop_na() %>% 
  slice_head(n = 10) %>%
  ggplot(aes(tf_idf, fct_reorder(trigram, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
    axis.text.y = element_text(color="black", 
                               hjust=0))

# Bigrams in sentiment analysis

bigrams_sent =
  bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE) 
  
knitr::kable(head(x)) %>% 
  save_kable("table1.png")

# get lexicon
AFINN <- get_sentiments("afinn")

AFINN

# find words associated with not
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

# visualize the 'wrong' contribution
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# negating words
negation_words <- c("not", "no", "never", "without")

negated_words <- 
  bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, ncol = 4, scales = "free")+
  labs(x = "Sentiment value * number of occurrences",
       y = "")
# networks of words

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# plot with ggr
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Correlations with widyr()
# select words by the answer
# which words co-occur in individual answers?

section_words = data %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, word) %>%
  filter(!word %in% stop_words$word)

section_words

# pairwise count
word_pairs <- section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# look at correlations
word_cors <- section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Can filter
change = 
  word_cors %>%
  filter(item1 == "change")

# plot correlations of particular words
word_cors %>%
  filter(item1 %in% c("city", "cars", "bristol", "people", "transport", "change")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()+
  ggtitle("Phi correlations for most common words")

# Visualize correlations
set.seed(2016)

word_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
