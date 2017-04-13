

cv.svm.all <- function(df, K){
  # Run CV with K folds for the SVM function, one-versus-all scheme
  # INPUT: Training set and number of folds
  # OUTPUT: Best Parameters and Performance
  
  costs = c(0.1,1,10)
  
  n <- length(df)
  n.fold <- floor(n/K)
  
  set.seed(123) # for reproducibility
  inTrain <- createFolds(df$author.id, k=K, list = FALSE)
  cv.error <- data.frame(cost=double(),
                         fold=double(), 
                         error=double())
  
  j = 0
  # Comment - the SVM function with the one-versus all implementation doesn't support to take any variable into parameters, as a result we are doing it manually
  cost = costs[1]
  for (i in 1:K){
    j = j+1
    train.data <- df[inTrain != i,]
    test.data <- df[inTrain == i,]
    
    svm.fit <- SVM(author.id~., data=train.data, kernel="linear", C=0.1,
                   class.type="one.versus.all", verbosity=0)
    
    test.label <- test.data$author.id
    test.data$author.id <- NULL
    
    pred <- predict(svm.fit, test.data)
    cv.error[j, "fold"] <- i
    cv.error[j, "cost"] <- cost
    cv.error[j, "error"] <- 1-(sum(diag(table(pred, test.label)))/sum(table(pred, test.label)))
  }
  
  cost = costs[2]
  for (i in 1:K){
    j = j+1
    train.data <- df[inTrain != i,]
    test.data <- df[inTrain == i,]
    
    svm.fit <- SVM(author.id~., data=train.data, kernel="linear", C=1,
                   class.type="one.versus.all", verbosity=0)
    
    test.label <- test.data$author.id
    test.data$author.id <- NULL
    
    pred <- predict(svm.fit, test.data)
    cv.error[j, "fold"] <- i
    cv.error[j, "cost"] <- cost
    cv.error[j, "error"] <- 1-(sum(diag(table(pred, test.label)))/sum(table(pred, test.label)))
  }
  
  cost = costs[3]
  for (i in 1:K){
    j = j+1
    train.data <- df[inTrain != i,]
    test.data <- df[inTrain == i,]
    
    svm.fit <- SVM(author.id~., data=train.data, kernel="linear", C=10,
                   class.type="one.versus.all", verbosity=0)
    
    test.label <- test.data$author.id
    test.data$author.id <- NULL
    
    pred <- predict(svm.fit, test.data)
    cv.error[j, "fold"] <- i
    cv.error[j, "cost"] <- cost
    cv.error[j, "error"] <- 1-(sum(diag(table(pred, test.label)))/sum(table(pred, test.label)))
  }
  
  cost = costs[4]
  for (i in 1:K){
    j = j+1
    train.data <- df[inTrain != i,]
    test.data <- df[inTrain == i,]
    
    svm.fit <- SVM(author.id~., data=train.data, kernel="linear", C=10,
                   class.type="one.versus.all", verbosity=0)
    
    test.label <- test.data$author.id
    test.data$author.id <- NULL
    
    pred <- predict(svm.fit, test.data)
    cv.error[j, "fold"] <- i
    cv.error[j, "cost"] <- cost
    cv.error[j, "error"] <- 1-(sum(diag(table(pred, test.label)))/sum(table(pred, test.label)))
  }
  
  result <- ddply(cv.error, c("cost"), summarise,
                  mean = mean(error),
                  sd   = sd(error)) 
  
  return(list(best.parameter = result$cost[which.min(result$mean)],
              best.performance = min(result$mean)))
}

run.svm <- function(i, attribute, dataset){
  # Run chosen model 
  # INPUT:
  # OUTPUT:
  
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
  result <- cv.svm.all(dtm.train, K=5)
  C <- result$best.parameter
  if(C==0.1){
    svm.fit <- SVM(author.id~., data=dtm.train, kernel="linear", C=0.1,
                   class.type="one.versus.all", verbosity=0)
  }
  else if(C==1){
    svm.fit <- SVM(author.id~., data=dtm.train, kernel="linear", C=1,
                   class.type="one.versus.all", verbosity=0)
  }
  else if(C==10){
    svm.fit <- SVM(author.id~., data=dtm.train, kernel="linear", C=10,
                   class.type="one.versus.all", verbosity=0)
  }
  #tune.svm <- tune(svm, author.id~., data=dtm.train, kernel="linear", 
  #                        ranges=list(cost=c(0.1,1,10)))
  
  preds <- predict(svm.fit, dtm.test)
  acc <- sum(diag(table(preds, df.test$author.id)))/nrow(df.test)
  
  return(acc)
}