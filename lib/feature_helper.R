## Coauthor Features

f.coauthor <- function(name) {
  
  ## Given a name, produce all coauthor and unique coauthor
  coauthor <- c()
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    coauthor[i] <- data_list[[which(query.list==name)]][[i]][3]
  }
  coauthor <- unlist(coauthor)
  unique_coauthor <- unique(unlist(coauthor))
  
  ## Tease out the coauthor that only appears once
  index <- c()
  for (i in 1:length(unique_coauthor)) {
    if (sum(unique_coauthor[i]==coauthor)==1) {
      index<-index
    } else {
      index <- c(index,i)
    }
  }
  unique_coauthor<- unique_coauthor[index]
  
  ## Generate features given unique coauthor
  feature_coauthor <- matrix(nrow = length(data_list[[which(query.list==name)]]),ncol = length(unique_coauthor))
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    for (j in 1:length(unique_coauthor)) {
      feature_coauthor[i,j] <- length(intersect(unlist(data_list[[which(query.list==name)]][[i]][3]),unique_coauthor[j]))
    }
  }
  
  feature_coauthor <- data.frame(feature_coauthor)
  colnames(feature_coauthor) <- unique_coauthor
  return(feature_coauthor)
}



## Journal Features

f.journal <- function(name) {
  
  ## Given a name, produce all journals and unique journals
  journal <- c()
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    journal[i] <- data_list[[which(query.list==name)]][[i]][5]
  }
  journal <- na.omit(unlist(journal))
  unique_journal <- unique(unlist(journal))
  
  ## Tease out the coauthor that only appears once
  index <- c()
  for (i in 1:length(unique_journal)) {
    if (sum(unique_journal[i]==journal)==1) {
      index<-index
    } else {
      index <- c(index,i)
    }
  }
  unique_journal<- unique_journal[index]
  
  ## Generate journal given unique journals
  feature_journal <- matrix(nrow = length(data_list[[which(query.list==name)]]),ncol = length(unique_journal))
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    for (j in 1:length(unique_journal)) {
      feature_journal[i,j] <- length(intersect(unlist(data_list[[which(query.list==name)]][[i]][3]),unique_journal[j]))
    }
  }
  
  feature_journal <- data.frame(feature_journal)
  colnames(feature_journal) <- unique_journal
  return(feature_journal)
}



## Paper Title Feature

f.ptitle <- function(name) {
  
  paper_t <- c()
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    paper_t[i] <- data_list[[which(query.list==name)]][[i]][4]
  }
  paper_id <- c()
  for (i in 1:length(data_list[[which(query.list==name)]])) {
    paper_id[i] <- data_list[[which(query.list==name)]][[i]][2]
  }
  
  it_train <- itoken(unlist(paper_t),
                     preprocessor = tolower,
                     tokenizer = word_tokenizer,
                     ids = paper_id,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under"))
  
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- create_dtm(it_train, vectorizer)
  dim(dtm_train)
  tfidf <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  return(data.frame(as.matrix(dtm_train_tfidf)))
}




  