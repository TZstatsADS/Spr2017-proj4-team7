
library(gmum.r)
manualtune<-function(author.id,dtm.train,dtm.test,test_label){

er<-matrix(0,nrow=7,ncol=2)

svm.fit_0.01 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.01, verbosity=0)

svm.pred_0.01<-predict(svm.fit_0.01,dtm.test)
er[1,1]=0.01
er[1,2]<-sum(svm.pred_0.01!=test_label)/length(test_label)


svm.fit_0.1 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.1, verbosity=0)

svm.pred_0.1<-predict(svm.fit_0.1,dtm.test)
er[2,1]=0.1
er[2,2]<-sum(svm.pred_0.1!=test_label)/length(test_label)


svm.fit_0.15 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.15, verbosity=0)

svm.pred_0.15<-predict(svm.fit_0.15,dtm.test)
er[3,1]=0.15
er[3,2]<-sum(svm.pred_0.15!=test_label)/length(test_label)


svm.fit_0.2 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.2, verbosity=0)

svm.pred_0.2<-predict(svm.fit_0.2,dtm.test)
er[4,1]=0.2
er[4,2]<-sum(svm.pred_0.2!=test_label)/length(test_label)


svm.fit_0.25 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.25, verbosity=0)

svm.pred_0.25<-predict(svm.fit_0.25,dtm.test)
er[5,1]=0.25
er[5,2]<-sum(svm.pred_0.25!=test_label)/length(test_label)


svm.fit_0.5 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=0.5, verbosity=0)

svm.pred_0.5<-predict(svm.fit_0.5,dtm.test)
er[6,1]=0.5
er[6,2]<-sum(svm.pred_0.5!=test_label)/length(test_label)


svm.fit_1 <- SVM(author.id~., data=dtm.train,
               class.type="one.versus.all",C=1, verbosity=0)

svm.pred_1<-predict(svm.fit_1,dtm.test)
er[7,1]=1
er[7,2]<-sum(svm.pred_1!=test_label)/length(test_label)


error.mat=as.data.frame(er)
cost=error.mat[which.min(error.mat[,2]),1]

models<-list(svm.fit_0.01,svm.fit_0.1,svm.fit_0.15,svm.fit_0.2,svm.fit_0.25,svm.fit_0.5,svm.fit_1)
#predicts<-list(svm.pred_0.01,svm.pred_0.1,svm.pred_0.15,svm.pred_0.2,svm.pred_0.25,svm.pred_0.5,svm.pred_1)

optimodel<-models[[which.min(error.mat[,2])]]
#optipredict<-predicts[[which(er==min(er))]]

optimodel$cost=cost
#optimodel$predict=optipredict
optimodel$error=min(er)

return (optimodel)
}