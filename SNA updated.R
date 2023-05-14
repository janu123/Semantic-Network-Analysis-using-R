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

token <- create_token(
  app = "TwitterSMM_app",
  consumer_key = "WuhzpErtCUk9mo4axsQ8Mz7uI",
  consumer_secret = "B0f2VTXK8NRw1kvasscvvJOqqRMxKB7wly0O0XcSpa2nG6wiOk",
  access_token = "875599623900889088-3E7KKBtX70grMnFs8J7AKsNGLkPmOzQ",
  access_secret = "FV7OfDmXrSi8I7LBxJ164l1oZgUq5n7b9l5KZngK0STiq"
)
df <- search_tweets("#onlinelearning", n = 10000, token = token)

head(df$text)

eng_df<-df %>% filter(lang =="en")
eng_df$full_text<- gsub("http.*","",  eng_df$full_text)
eng_df$full_text<- gsub("https.*","", eng_df$full_text)
emoji_pattern <- "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F1E0-\U0001F1FF]"

text_df <- gsub(emoji_pattern, "", df$text)

text_corpus <- Corpus(VectorSource(eng_df$full_text))
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords, 
                      c("rodneydavis", "rt", "re", "amp","aat"))
text_corpus <- tm_map(text_corpus, removeWords, 
                      stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_df <- data.frame(text_clean = get("content", text_corpus), 
                      stringsAsFactors = FALSE)
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
5
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
# Put it all back together
eng_df_1<- paste0(word_counts$word, collapse = " ")


skip_gram<- CreateTcm(doc_vec = eng_df_1,
                      skipgram_window = 10,
                      verbose = FALSE,
                      cpus = 1)


skip_gram<- as.matrix(skip_gram)
diag(skip_gram) <- 0
tweet_network <- graph.adjacency(skip_gram, weighted = T)
simil<- simil(as.matrix(skip_gram), method = "cosine")
# Plot distribution of similarity values
hist(simil, main = "Distribution of similarity values")

# Set threshold at 90th percentile
threshold <- quantile(simil, 0.9)

# Compute edge weights
edge_weights <- simil
edge_weights[edge_weights < threshold] <- 0

# Create a new graph object with edge weights
tweet_network_weighted <- graph.adjacency(as.matrix(edge_weights), weighted = TRUE, mode = "undirected")

# Set edge weights as an attribute of the graph
set_edge_attr(tweet_network_weighted, "weight", value = E(tweet_network_weighted)$weight)
edges <- get.data.frame(tweet_network_weighted, what="edges")

vs<-V(tweet_network_weighted)
nodes_df <- data.frame(name = names(vs))


write.csv(edges, "edges.csv")
write.csv(nodes_df, "nodes.csv")

