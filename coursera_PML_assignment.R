# install.packages("mice")
# install.packages("VIM")
# install.packages("randomForest")
library(caret)
library(ggplot2)
library(randomForest)

raw_data <- read.csv("c://pml-training.csv",header=T)
raw_task <- read.csv("c://pml-testing.csv",header=T)
raw_data <- raw_data[,-1]
raw_task <- raw_task[,-1]
dim(raw_data)
dim(raw_task)
names(raw_data)
NA_col <- as.data.frame(colSums(is.na(raw_data)))
remain_feature<- colnames(raw_data[colSums(is.na(raw_data)) == 0])



raw_data <- raw_data[remain_feature]
raw_task <- raw_task[remain_feature[-92]]

inTrain = createDataPartition(raw_data$classe, p = 6/10)[[1]]
train = raw_data[ inTrain,]
test = raw_data[-inTrain,]



train[train == ' '] <- NA
test[test == ' '] <- NA
final_feature<- colnames(train[colSums(is.na(train)) == 0])

final_train = train[final_feature]
final_test = test[final_feature]

final_train <- final_train[,-(2:5)]
final_test <- final_test[,-(2:5)]




library(randomForest)
set.seed(10000)
fitRF <- randomForest(classe~. ,data=final_train,ntree=50)

pRF_final_train <- predict(fitRF,newdata=final_train[-55])
confusionMatrix(final_train$classe,pRF_final_train)


pRF_final_test <- predict(fitRF,newdata=final_test)
confusionMatrix(final_test$classe,pRF_final_test)




raw_submission <- subset(raw_task,select = names(final_test[-55])) 


pRF_submission_result <- predict(fitRF,newdata=raw_submission)
pRF_submission_result <- as.character(pRF_submission_result)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pRF_submission_result)


