

manualtune<-function(author.id,dtm.train,dtm.test,df.test){

er<-vector()

svm.fit_0.01 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.01, verbosity=0)

svm.pred_0.01<-predict(svm.fit_0.01,dtm.test)
er<-c(er,sum(svm.pred_0.01!=df.test$author.id)/length(df.test$author.id))


svm.fit_0.1 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.1, verbosity=0)

svm.pred_0.1<-predict(svm.fit_0.1,dtm.test)
er<-c(er,sum(svm.pred_0.1!=df.test$author.id)/length(df.test$author.id))


svm.fit_0.15 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.15, verbosity=0)

svm.pred_0.15<-predict(svm.fit_0.15,dtm.test)
er<-c(er,sum(svm.pred_0.15!=df.test$author.id)/length(df.test$author.id))


svm.fit_0.2 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.2, verbosity=0)

svm.pred_0.2<-predict(svm.fit_0.2,dtm.test)
er<-c(er,sum(svm.pred_0.2!=df.test$author.id)/length(df.test$author.id))


svm.fit_0.25 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.25, verbosity=0)

svm.pred_0.25<-predict(svm.fit_0.25,dtm.test)
er<-c(er,sum(svm.pred_0.25!=df.test$author.id)/length(df.test$author.id))


svm.fit_0.5 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.5, verbosity=0)

svm.pred_0.5<-predict(svm.fit_0.5,dtm.test)
er<-c(er,sum(svm.pred_0.5!=df.test$author.id)/length(df.test$author.id))


svm.fit_1 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=1, verbosity=0)

svm.pred_1<-predict(svm.fit_1,dtm.test)
er<-c(er,sum(svm.pred!=df.test$author.id)/length(df.test$author.id))


error.mat=as.data.frame(rbind(cost=c(0.01,0.1,0.15,0.2,0.25,0.5,1),er))
cost=error.mat[er==min(er)][1,]

models<-list(svm.fit_0.01,svm.fit_0.1,svm.fit_0.15,svm.fit_0.2,svm.fit_0.25,svm.fit_0.5,svm.fit_1)
predicts<-list(svm.pred_0.01,svm.pred_0.1,svm.pred_0.15,svm.pred_0.2,svm.pred_0.25,svm.pred_0.5,svm.pred_1)

optimodel<-models[[which(er==min(er))]]
optipredict<-predicts[[which(er==min(er))]]

optimodel$cost=cost
optimodel$predict=optipredict
optimodel$error=min(er)

return (optimodel)
}