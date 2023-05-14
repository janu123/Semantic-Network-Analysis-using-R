library(tidytext)
library(tm)
library(rtweet)
library(igraph)
library(tidyverse)
library(tidytext)
library(textmineR)
library(gutenbergr)
library(proxy)
library(network)
library("quanteda")
library(topicmodels)
token <- create_token(
  app = "TwitterSMM_app",
  consumer_key = "WuhzpErtCUk9mo4axsQ8Mz7uI",
  consumer_secret = "B0f2VTXK8NRw1kvasscvvJOqqRMxKB7wly0O0XcSpa2nG6wiOk",
  access_token = "875599623900889088-3E7KKBtX70grMnFs8J7AKsNGLkPmOzQ",
  access_secret = "FV7OfDmXrSi8I7LBxJ164l1oZgUq5n7b9l5KZngK0STiq"
)
auth <- rtweet_app()
df1 <- search_tweets("#onlinelearning", n = 10000, token = token)
df2 <- search_tweets("#eLearning", n = 10000, token = token)
df3 <- search_tweets("#VirtualLearning", n = 10000, token = token)
df <- rbind(df1, df2, df3)
dim(df)
eng_df <- df %>% filter(lang == "en")
dim(eng_df)
eng_df_truc <- eng_df[1:2000, ] # nolint
dim(eng_df_truc)
head(eng_df_truc)
text_df <- eng_df_truc$full_text

text_df <- gsub("http.*", "",  text_df)
text_df
text_corpus <- Corpus(VectorSource(text_df))
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords,
                      c("rodneydavis", "rt", "re", "amp"))
text_corpus <- tm_map(text_corpus, removeWords,
                      stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus,content_transformer(function(x) gsub("\\d+", "", x)))
text_corpus <- tm_map(text_corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
#text_corpus <- tm_map(text_corpus, content_transformer(function(x) gsub("[\\u{1F600}-\\u{1F64F}]|[^\\u{0000}-\\u{007F}]", "", x)))
my_content_transformer <- function(x) {
  # Remove words "abs" and "abcs"
  x <- gsub("\\babs\\b|\\babcs\\b", "", x, ignore.case = TRUE)
  # Remove words containing three or more dots
  x <- gsub("\\b\\w*\\.\\.{2,}\\w*\\b", "", x)
  # Return cleaned text
  x
}
text_corpus <- tm_map(text_corpus, content_transformer(my_content_transformer))
text_corpus <- tm_map(text_corpus, removeWords, "...")
text_corpus <- tm_map(text_corpus, content_transformer(function(x) gsub("\\b[A-Z][a-z]+\\b", "", x)))
text_corpus <- tm_map(text_corpus, content_transformer(function(x) iconv(x, "UTF-8", "ASCII", sub = "")))

text_df <- data.frame(text_clean = get("content", text_corpus),
                      stringsAsFactors = FALSE)
text_df <- na.omit(text_df)
text_df
dim(text_df)
tweets_clean <- text_df %>%
  dplyr::select(text_clean) %>%
  unnest_tokens(word, text_clean, strip_punct = FALSE)
dim(tweets_clean)
tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count", y = "Unique words",
  title = "Count of unique words found in tweets")

data(stop_words)
tweets_clean$word
# Drop stop words as well as useless puntuation
word_counts <- anti_join(tweets_clean, stop_words, by = "word")
word_counts <- anti_join(word_counts,
                         data.frame(word = c("*", ";", ",", ":"),
                                    lexicon = "bad punctuation",
                                    stringsAsFactors = FALSE),
                         by = "word")
eng_tweets <- paste0(word_counts$word, collapse = " ")
skip_gram <- CreateTcm(doc_vec = eng_tweets,
                 skipgram_window = 10,
                 verbose = FALSE,
                 cpus = 1)
skip_gram  <- as.matrix(skip_gram)
skip_gram
diag(skip_gram) <- 0
tweet_network <- graph.adjacency(skip_gram, weighted = TRUE)
simil <- simil(as.matrix(skip_gram), method = "cosine")
tweet_network
edge_weights <- simil
hist(simil, main = "Distribution of similarity values")

threshold <- quantile(simil, 0.9)

edge_weights[edge_weights < threshold] <- 0

# Create a new graph object with edge weights
tweet_network_weighted <- graph.adjacency(as.matrix(edge_weights),
                          weighted = TRUE, mode = "undirected")

# Set edge weights as an attribute of the graph
set_edge_attr(tweet_network_weighted, "weight",
        value = E(tweet_network_weighted)$weight)
edges <- get.data.frame(tweet_network_weighted, what = "edges")

vs <- V(tweet_network_weighted)
nodes_df <- data.frame(name = names(vs))


write.csv(edges, "edges.csv")
write.csv(nodes_df, "nodes.csv")

head(V(tweet_network)$name)
plot(tweet_network)

word_counts %>% count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)



