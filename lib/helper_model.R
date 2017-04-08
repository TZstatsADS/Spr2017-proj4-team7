

run.model <- function(i, attribute, dataset){
  # Change Format - Data Frame
  df <- data.frame(matrix(unlist(dataset[[i]]), 
                          nrow=length(dataset[[i]]), byrow=T), 
                   stringsAsFactors=FALSE)
  
  # Update columns' names
  names(df) <- c("author.id", "paper.id", "coauthors", "paper", "journal")
  
  # Create Hybrid Column
  df$hybrid <- paste(df$paper, df$coauthors, df$journal, sep=" ")
  # Create Vacabulary
  it <- itoken(df[, attribute], 
               preprocessor = tolower, 
               tokenizer = word_tokenizer,
               ids = df$paper.id,
               # turn off progressbar because it won't look nice in rmd
               progressbar = FALSE)
  vocab <- create_vocabulary(it, stopwords = c("a", "an", "the", "in", "on",
                                               "at", "of", "above", "under"))
  
  # Split training & testing set
  df$author.id <- factor(df$author.id)
  set.seed(123) # for reproducibility
  inTrain <- createDataPartition(df$author.id, p=0.8, list=FALSE)
  df.train <- df[inTrain,]
  df.test <- df[-inTrain,]
  
  # Vectorize uniques words
  vectorizer <- vocab_vectorizer(vocab)
  # Train set
  it.train <- itoken(df.train[, attribute], 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = df.train$paper.id,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  dtm.train <- create_dtm(it.train, vectorizer)
  
  # Test set
  it.test <- itoken(df.test[, attribute], 
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer,
                    ids = df.test$paper.id,
                    # turn off progressbar because it won't look nice in rmd
                    progressbar = FALSE)
  dtm.test <- create_dtm(it.test, vectorizer)
  
  # Change Format
  dtm.test <- as.data.frame(as.matrix(dtm.test))
  dtm.train <- as.data.frame(as.matrix(dtm.train))
  # Add labels to the trainnig set
  dtm.train <- cbind(df.train$author.id, dtm.train)
  names(dtm.train)[1] <- "author.id"
  
  # Fit model
  tune.svm <- tune(svm, author.id~., data=dtm.train, kernel="linear", 
                          ranges=list(cost=c(0.1,1,10)))
  
  preds <- predict(tune.svm$best.model, dtm.test)
  acc <- sum(diag(table(preds, df.test$author.id)))/nrow(df.test)
  
  return(acc)
}