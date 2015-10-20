# install.packages("mice")
# install.packages("VIM")
library(caret)
library(ggplot2)
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
