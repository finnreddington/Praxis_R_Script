# Ch.3 - Analyzing word and document frequency

library(tidytext)
library(tidyverse)
library(forcats)

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

# Calculate the term frequency for each question

question_words = data %>%
  unnest_tokens('word', word) %>%
  count(question, word, sort = TRUE)

total_words <- question_words %>% 
  group_by(question) %>% 
  summarize(total = sum(n))

question_words <- left_join(question_words, total_words)
question_words

# visualize term frequency
question_words %>% 
  filter(question=="Q1") %>% 
  ggplot(aes(n/total, fill = question)) +
  geom_histogram(show.legend = FALSE, bins = 100) +
  xlim(NA, 0.0051) +
  facet_wrap(~question, ncol = 2, scales = "free_y")+
  ggtitle("Histogram of Term Frequency for Q1")

# Zipf's Law

freq_by_rank <- question_words %>% 
  group_by(question) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = question)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2)+
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+
  ggtitle("Zipf's Law in the Climate Survey")

# tf-idf
question_tf_idf <- question_words %>%
  bind_tf_idf(word, question, n) %>% 
  arrange(desc(tf_idf))

question_tf_idf %>%
  filter(question == c("Q1", "Q2", "Q3", "Q4")) %>% 
  drop_na() %>% 
  group_by(question) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
        axis.text.y = element_text(color="black", 
                                   hjust=0))
question_tf_idf %>%
  filter(question == c("Q5", "Q6", "Q7", "Q8")) %>% 
  drop_na() %>% 
  group_by(question) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
    axis.text.y = element_text(color="black", 
                               hjust=0))

question_tf_idf %>%
  filter(question == c("Q9", "Q10")) %>% 
  drop_na() %>% 
  group_by(question) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~question, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)+
  theme(
    axis.text.y = element_text(color="black", 
                               hjust=0))
# Filter the examples of a word:
marvin =
  data %>% 
  filter(str_detect(word, "marvin")) %>% 
  select(word)

print(marvin$word[])

# Can also filter custom stop words to get a more meaningful selection
      