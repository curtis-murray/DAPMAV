library(tidyverse)
library(wordcloud)
library(ggwordcloud)
library(tidytext)
library(quantreg)
library(ggforce)
library(scales)
library(cowplot)
library(lubridate)
# Loading Data----------------------------------------------------

clean_posts <- read_csv("data/Scrape/clean_posts.csv")

# The number of levels in the topic hierarchy
max_level <- read_lines("data/Topic_Model/max_level.csv") %>% as.integer()

words_all <- read_csv(words_all_path) %>% 
  mutate(word_ID = 1:n()) %>% 
  select(word_ID, word = `0`)

docs_all <- read_csv(docs_all_path) %>% 
  mutate(doc_ID = 1:n()) %>% 
  select(doc_ID, doc = `0`)

for(topic_level in as.character(0:max_level)){
  
  p_w_tw_path <- paste("data/Topic_Model/", "p_w_tw_level_",topic_level, ".csv", sep = "")
  p_tw_d_path <- paste("data/Topic_Model/", "p_tw_d_level_", topic_level, ".csv", sep = "")
  words_all_path <- paste("data/Topic_Model/","words_all.csv", sep = "")
  docs_all_path <- paste("data/Topic_Model/","documents_all.csv", sep = "")
  
  p_w_tw <- read_csv(p_w_tw_path) %>%
    mutate(word_ID = 1:n()) %>% 
    select(word_ID, everything())
  
  p_tw_d <- read_csv(p_tw_d_path) %>%
    mutate(topic = 1:n()) %>% 
    select(topic, everything())
  
  tidy_topics <- p_w_tw %>% 
    gather("topic", "p", -word_ID) %>% 
    mutate(topic = as.numeric(topic) + 1) %>% 
    filter(p > 0) %>% 
    full_join(words_all, by = "word_ID")
  
  n_topics <- tidy_topics %>% pull(topic) %>% unique() %>% length()
  
  tidy_topic_docs <- p_tw_d %>% 
    gather("doc_ID", "p", -1) %>% 
    mutate(doc_ID = as.numeric(doc_ID) + 1) %>% 
    full_join(docs_all, by = "doc_ID") %>% 
    left_join(clean_posts, by = c("doc" = "Post_ID")) %>% 
    select(topic, doc_ID, p, doc, Sub, Date)
  
  tidy_topics_path <- paste("data/Topic_Model/Clean/tidy_topics_", topic_level, ".csv",sep = "")
  write_csv(tidy_topics, tidy_topics_path)
  
  tidy_topic_docs_path <- paste("data/Topic_Model/Clean/tidy_topics_docs", topic_level, ".csv",sep = "")
  write_csv(tidy_topic_docs, tidy_topic_docs_path)
  
}