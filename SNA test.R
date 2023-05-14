library(rtweet)
library(igraph)
library(tidyverse)
library(tidytext)
library(textmineR)
library(gutenbergr)
library(proxy)
library(network)
if(!"RCy3" %in% installed.packages()){
  install.packages("BiocManager")
  BiocManager::install("RCy3")
}
library(RCy3)
auth <- rtweet_app()
df <- search_tweets("#onlinelearning", n=10000,token = token)


eng_df<-df %>% filter(lang=="en")

eng_df$text<-clean_tweets(eng_df$text)

eng_df_truncated<-eng_df[1:100,]

eng_df$text= gsub("&amp", "", eng_df$text)
eng_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", eng_df$text)
eng_df$text = gsub("@\\w+", "", eng_df$text)
eng_df$text = gsub("[[:punct:]]", "", eng_df$text)
eng_df$text = gsub("[[:digit:]]", "", eng_df$text)
eng_df$text = gsub("http\\w+", "", eng_df$text)
eng_df$text = gsub("[ \t]{2,}", "", eng_df$text)
eng_df$text = gsub("^\\s+|\\s+$", "", eng_df$text)
address_str <- "aat"
eng_df$text <- gsub(',','',eng_df$text)
tidy_tweets <- eng_df %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c(",", ".", ":", ";", "!", "?"))

# Load in stop words
data(stop_words)

# Drop stop words as well as useless puntuation
word_counts <- anti_join(tweet_words , stop_words, by = "word") 
word_counts <- anti_join(word_counts, 
                         data.frame(word = c("*", ";", ",", ":"), 
                                    lexicon = "bad punctuation",
                                    stringsAsFactors = F), 
                         by = "word")
word_counts<-word_counts %>%
  mutate(word = str_remove(word, "aaraf"))


# Put it all back together
eng_df<- paste0(word_counts$word, collapse = " ")


skip_gram<- CreateTcm(doc_vec = eng_df_truncated,
                                    skipgram_window = 10,
                                    verbose = FALSE,
                                    cpus = 1)
simil<- simil(as.matrix(skip_gram), method = "cosine")

skip_gram<- as.matrix(skip_gram)
diag(skip_gram) <- 0
tweet_network <- graph.adjacency(skip_gram, weighted = T)
tweet_network 
head(V(tweet_network)$name)
plot(tweet_network)
plot(tweet_network, vertex.size = 2, vertex.color = "tomato", vertex.label.cex = .2, vertex.label.color = "black", edge.curved = .1, edge.arrow.size = .3, edge.width = .2)
plot(tweet_network, vertex.size = 2, vertex.color = "tomato", vertex.label = NA)
plot(tweet_network, vertex.size = 2, vertex.color = "tomato", vertex.label.cex = .2, vertex.label.color = "black", edge.curved = .1, edge.arrow.size = .1, edge.width = .2)
class(tweet_network)
igraph.write(tweet_network, "mygraph.gml", format="gml")

mygraph <- igraph::graph_from_data_frame(tweet_network) #create a graph


mylayout <- igraph::layout_as_tree(tweet_network, circular = T)
tweet_network$layout = mylayout
layout <- layout_with_fr(tweet_network) %>% as.data.frame() %>% rename(x=V1, y=V2)
V(tweet_network)$label = V(tweet_network)$name

net = network(tweet_network, directed = FALSE)

