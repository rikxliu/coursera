

# import  the packages needed ,if you dont install them ,you can install.packages("randomForest") in the beginning
library(caret)
library(ggplot2)
library(randomForest)

# import the raw data ,delete column number in the first column
raw_data <- read.csv("c://pml-training.csv",header=T)
raw_task <- read.csv("c://pml-testing.csv",header=T)
raw_data <- raw_data[,-1]
raw_task <- raw_task[,-1]

# calculate the total empty columns, extract the first feature set
NA_col <- as.data.frame(colSums(is.na(raw_data)))
remain_feature<- colnames(raw_data[colSums(is.na(raw_data)) == 0])


# delete the empty features in the raw training and testing data
raw_data <- raw_data[remain_feature]
raw_task <- raw_task[remain_feature[-92]]  ##drop the classe feature in the testing data 

# create the training and testing date with the original training data
inTrain = createDataPartition(raw_data$classe, p = 6/10)[[1]]
train = raw_data[ inTrain,]
test = raw_data[-inTrain,]

# transform other columns with empty content or too many nulls  into NA, then drop them 
train[train == ' '] <- NA
test[test == ' '] <- NA
train[train == ''] <- NA
test[test == ''] <- NA
final_feature<- colnames(train[colSums(is.na(train)) == 0])

final_train = train[final_feature]
final_test = test[final_feature]

# drop the timestamp and window features ,get the final train and test data set
final_train <- final_train[,-(2:5)]
final_test <- final_test[,-(2:5)]







# some basic data explore 
summary(final_train)
table(final_train$user_name,final_train$classe)
ggplot(final_train,aes(classe,fill=user_name))+geom_bar()
ggplot(final_train,aes(roll_belt,fill=classe))+geom_histogram()







# set seed and use randomForest algorithm to fit the predicting model and self CV in randomForest
set.seed(10000)
fitRF <- randomForest(classe~.,data=final_train,ntree=50)
fitRF

#  since randomForest use sampling itself£¬then produce the out of sample data,so we can user OOB stimate of  error rate as an instead of error  in cross-validation
#  OB estimate of  error rate: 0.55%

pRF_final_train <- predict(fitRF,newdata=final_train[-55])
confusionMatrix(final_train$classe,pRF_final_train)  # Accuracy : 1   on the training data set

pRF_final_test <- predict(fitRF,newdata=final_test)
confusionMatrix(final_test$classe,pRF_final_test)    # Accuracy : 0.9934 on the testing data set


# process the raw data of the predicting job, keep the final feature of the training and testing data
raw_submission <- subset(raw_task,select = names(final_test[-55])) 

# apply  the RF model fitted  before on the processed  predicting job raw data and transform it into character
pRF_submission_result <- predict(fitRF,newdata=raw_submission)
pRF_submission_result <- as.character(pRF_submission_result)

# use the official function to produce the final submission result and write down it 
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pRF_submission_result)


