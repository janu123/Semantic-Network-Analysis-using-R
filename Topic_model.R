tweets_byusers_corpus <- iconv(eng_df_truc$full_text)
corpus <- Corpus(VectorSource(tweets_byusers_corpus))
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus,removeWords,stopwords("english")) 
corpus <- tm_map(corpus,removeWords,c("mindhunter", "david", "fincher","netflix")) 
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus,stripWhitespace) 
corpus <- tm_map(corpus, removeWords,
                      c("rodneydavis", "rt", "re", "amp"))
corpus <- tm_map(corpus,content_transformer(function(x) gsub("\\d+", "", x)))
my_content_transformer <- function(x) {
  # Remove words "abs" and "abcs"
  x <- gsub("\\babs\\b|\\babcs\\b", "", x, ignore.case = TRUE)
  # Remove words containing three or more dots
  x <- gsub("\\b\\w*\\.\\.{2,}\\w*\\b", "", x)
  # Return cleaned text
  x
}
corpus <- tm_map(corpus, content_transformer(my_content_transformer))
corpus <- tm_map(corpus, removeWords, "...")
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b[A-Z][a-z]+\\b", "", x)))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, "UTF-8", "ASCII", sub = "")))
dtm <- DocumentTermMatrix(corpus)  
inspect(dtm[1:5, 1:5])  

dtm.mx <- as.matrix(dtm)
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:25] 

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 5 #find 5 topics

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("topic_model",k,"DocsToTopics.csv"))
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("topic_model",k,"TopicsToTerms.csv"))
ldaOut.terms[1:6,]

library(LDAvis)
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(ldaOut))
