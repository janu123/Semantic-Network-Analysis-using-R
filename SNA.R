## Load libraries
library(rtweet)
library(igraph)
library(tidyverse)

## Create Twitter token
token <- create_token(
  app = "TwitterSMM_app",
  consumer_key = "27L67KAoPBrjx7qVxZMuicOaX",
  consumer_secret = "oCZJ7lts73WcQbIkRkPzjKYqbzhHFtQIWYis39PmyFfI6l1mVx",
  access_token = "875599623900889088v373COCyhj32SyjGUoek5CSssUR4dAO",
  access_secret = "z89idEWl9osgxS5dGGF2hsaG6ygMd86oir2J28sfBpxjo")

## Download Tweets
tweets.df <- search_tweets("messi", n=250000,token=token,retryonratelimit = TRUE,until="2021-08-13")

## Save R context image
save.image("filename.RData")

auth <- rtweet_app()

df <- search_tweets("#onlinelearning", n=10000,token = auth)
clean_tweets(df)
head(entity(df, "screen_name"))
df_mentions<-entity(df, "user_mentions")

filter(df_mentions, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> net


R_foundation_flw <- get_followers("_R_Foundation", n = 30000,
                                  retryonratelimit = TRUE)

stream_random <- sample_stream(timeout = streamtime, file = filename, parse = FALSE)
df$entities
library(tidytext)
library(tm)
head(df$text)
text_df <- tibble(eng_df$text)
text_df<- gsub("http.*","",  text_df)
text_corpus <- Corpus(VectorSource(text_df))
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords, 
                      c("rodneydavis", "rt", "re", "amp"))
text_corpus <- tm_map(text_corpus, removeWords, 
                      stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_df <- data.frame(text_clean = get("content", text_corpus), 
                      stringsAsFactors = FALSE)
# note the words that are recognized as unique by R
a_list_of_words <- c("Dog", "dog", "dog", "cat", "cat", ",")
unique(a_list_of_words)
## [1] "Dog" "dog" "cat" ","
tweets_clean <- text_df %>%
  dplyr::select(text_clean) %>%
  unnest_tokens(word, text_clean)
tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
data("stop_words")
# view first 6 words
head(stop_words)
## # A tibble: 6 x 2
##   word      lexicon
##   <chr>     <chr>  
## 1 a         SMART  
## 2 a's       SMART  
## 3 able      SMART  
## 4 about     SMART  
## 5 above     SMART  
## 6 according SMART

nrow(tweets_clean)
## [1] 247606

# remove stop words from your list of words
tweet_words <- tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(tweet_words)
## [1] 133584


tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
tweets_paired_words <- df %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
tweets_separated_words <- tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

tweets_filtered <- tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
words_counts <- tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(words_counts)
library(igraph)
library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
words_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Mental Health",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

devtools::install_github("AlexChristensen/SemNeT", dependencies = c("Imports", "Suggests"))

