install.packages("caret")
library(caret)

trainingurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

setwd("C:/Users/s.maramreddy/OneDrive - Accenture/Personal/R/Machine Lang/")

download.file(trainingurl, "trngfiledwd.csv")
download.file(testurl, "tstfiledwd.csv")

trngfile <- read.csv("trngfiledwd.csv",na.strings=c("NA","#DIV/0!", ""))
testfile <- read.csv("tstfiledwd.csv",na.strings=c("NA","#DIV/0!", ""))

trngfile<-trngfile[,colSums(is.na(trngfile)) == 0]
testfile <-testfile[,colSums(is.na(testfile)) == 0]

trngfile  <-trngfile[,-c(1:7)]
testfile <-testfile[,-c(1:7)]

cvalind <- createDataPartition(y=trngfile$classe, p=0.3, list=FALSE)
cvaltrngfile <- trngfile[cvalind,]
cvaltestfile <- trngfile[-cvalind,]

ctrl = trainControl(method='cv',number=5,repeats=2,returnResamp='none')
fit1 <- train(classe~., data=cvaltrngfile, method="gbm",trControl=ctrl) 

pred <- predict(fit1, cvaltestfile)
confusionMatrix(pred, cvaltestfile$classe)

fit2 <- train(classe~., data=cvaltrngfile, method="rf",trControl=ctrl) 

pred2 <- predict(fit2, cvaltestfile)
confusionMatrix(pred2, cvaltestfile$classe)

predtest <- predict(fit2, testfile)

finalresult <- as.character(predtest)

predwritefiles = function(z){
  n = length(z)
  for(i in 1:n){
    filename = paste0("problem_id_",z,".txt")
    write.table(z[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

predwritefiles(finalresult)
