library(lubridate)
library(tidyverse)
library(tidytext)

remove_comments <- TRUE
remove_stop_words <- TRUE
add_title_to_content <- TRUE
min_words_per_doc <- 10 # Note if stopwords are removed this is the number of non-stopwords.
min_word_count_in_corpus <- 5 # Removes uncommon words.
start_date <- "2019-01-01"
end_date <- "2099-01-01"


paths <- list.files(path = "data/Scrape/Subs", pattern="*.csv")

Posts <- tibble(path = paths) %>% 
  mutate(Sub = str_replace(paths, ".csv", "")) %>% 
  mutate(path = paste("data/Scrape/Subs/", path, sep = "")) %>% 
  group_by(Sub) %>% 
  mutate(posts = map("data", ~read_csv(path))) %>% 
  unnest() %>% 
  select(-path) %>% 
  ungroup()

# Additional stopwords to tidytext::stop_words
more_stop_words <- c("im", "didnt", "shouldnt", "cant", "wont", "amp",
                     "https", "http", "x200b", "www.reddit.com",
                     "utm_name", "ios_app", "utm_medium","ive")

clean_posts <- Posts %>% 
  select(Sub,Post_ID = `Post ID`, Title, Author, Date = `Publish Date`, Flair, Content, parent_id, link_id, Score, n_comments = `Total No. of Comments`, Permalink) %>% 
  group_by_all() %>% 
  summarise() %>%
  ungroup() %>% 
  mutate(Type = ifelse(!is.na(n_comments), "Post", "Reply")) %>% 
  filter(if (remove_comments) Type == "Post" else TRUE) %>% # Remove comments if set
  filter(Date %within% interval(start_date, end_date)) %>% 
  {
    if (add_title_to_content) {
      mutate(., Content = paste0(Title, Content))
    } else {
      .
    }
  } %>% 
  unnest_tokens(word, Content) %>% 
  mutate(word = str_replace(word, '’', "'")) %>% 
  filter(if (remove_stop_words) !word %in% c(stop_words$word,more_stop_words) else TRUE) %>% # Remove stop words if set
  group_by(word) %>% 
  mutate(count = n()) %>% 
  filter(n() >= min_word_count_in_corpus) %>%  # Remove uncommon words
  ungroup() %>% 
  group_by(Post_ID) %>% 
  filter(n() >= min_word_count_in_corpus) %>% # Removes small docs
  group_by(Sub, Post_ID, Title, Author, Date, Flair, parent_id, link_id, n_comments, Type) %>% 
  summarise(Content = paste(word, collapse = " ")) %>% 
  ungroup() %>% 
  group_by(Sub, Post_ID, Title, Author, Date, Flair, parent_id, Content, n_comments, Type) %>% 
  summarise() %>% 
  ungroup()

write_csv(clean_posts, "data/Scrape/clean_posts.csv")
