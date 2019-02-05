# install.packages("sentimentr")
# install.packages("dplyr")
#install.packages("e1071")
#install.packages("stringi")
#install.packages("reticulate")
library(sentimentr)
library(dplyr)
library(e1071)
library(data.table)
library(rpart)
library(neuralnet)
library(reticulate)
#read the data from file
trainData <- read.csv("drugsComTrain_raw/drugsComTrain_raw.csv", header=T,  sep = ",")
#testData <- read.csv("drugsComTest_raw/drugsComTest_raw.csv", header=T,  sep = ",")

#Delete unnecessary columns and missing or wrong values
trainData <- within(trainData, rm("uniqueID","date"))
trainData <- trainData[-which(grepl("</span>",trainData[,2]) == TRUE | trainData[,2] == ""),]

#get the conditions
conditions <- as.data.frame(trainData[,2])
colnames(conditions)<-c("columnName")
#group by ColumnName and sort descending
conditionsDesc <- descOrderByColumn(conditions,"Condition")

#We need at least 2 Drugs that used for one condition so delete single condition ones.
singleCondition <- conditionsDesc[which(conditionsDesc[,2]==1),]
trainData <- trainData[which(trainData[,2] %in%  unlist(singleCondition[,1]) == FALSE),]
conditionsDesc <- conditionsDesc[-which(conditionsDesc[,2]==1),]

trainData2 <- trainData
trainData2<-as.data.frame(trainData2)
conditionsDesc <- as.data.frame(conditionsDesc)

trainData2$condition <- as.character(trainData2$condition)
conditionsDesc$Condition <- as.character(conditionsDesc$Condition)

for (i in 1:length(conditionsDesc$Condition)) {
    cols <- which(trainData2[,2]==conditionsDesc[i,1])
    for (j in 1:length(cols)) {
      trainData2[cols[j],"condition"] <- i 
    }
}

#get the drug names
drugNames <- as.data.frame(trainData[,1])
colnames(drugNames)<-c("columnName")
#group by ColumnName and sort descending
drugNameDesc <- descOrderByColumn(drugNames,"DrugName")

drugNameDesc <- as.data.frame(drugNameDesc)
trainData2$drugName <- as.character(trainData2$drugName)
drugNameDesc$DrugName <- as.character(drugNameDesc$DrugName)

for (i in 1:length(drugNameDesc$DrugName)) {
  cols <- which(trainData2[,1]==drugNameDesc[i,1])
  for (j in 1:length(cols)) {
    trainData2[cols[j],"drugName"] <- i 
    
  }
}

#plot bar chart for first 10 tuples(The most popular conditions)
barplot(conditionsDesc$Count[1:10], names = conditionsDesc$condition[1:10],
        xlab = "Condition", ylab = "Count", col = rainbow(7),
        main = "Bar Chart")
barplot(drugNameDesc$Count[1:10], names = drugNameDesc$DrugName[1:10],
        xlab = "Drug Name", ylab = "Count", col = rainbow(7),
        main = "Bar Chart")

sentimentResultByReview <- sentiment_by(get_sentences(as.character(trainData2$review)))

#trainData$sentimentResult <- sign(sentimentResultByReview[,4])
trainData2[,6] <- sign(sentimentResultByReview[,4])
trainData2 <- within(trainData2, rm("review"))
#delete zerosss
trainData2 <- trainData2[-which(trainData2$ave_sentiment==0),]

trainData2 <- trainData2[which((trainData2$ave_sentiment==1 & trainData2$rating > 5) | (trainData2$ave_sentiment==-1 & trainData2$rating < 6)),]
trainData2 <- within(trainData2, rm("usefulCount","rating"))
write.csv(trainData2,'newTrainData.csv')

source_python('deneme2.py')

dataset <-data('newTrainData.csv')
scores <- calculate_accuracy(dataset[])
cat("Accuracy : ",mean(scores))


##------------------------------------DT-----------------------------------
# fit <- rpart(ave_sentiment~., data = trainData2[1:10000,], method = 'class')
# predict_unseen <-predict(fit, trainData2[1:1000,1:2], type = 'class')
# table_mat <- table(trainData2[1:1000,3], predict_unseen)
# table_mat
##------------------------------------SVM-----------------------------------
# fit <- svm(ave_sentiment~., data = trainData2)
# predict <-predict(fit, trainData2[1:11,1:2])
# table_mat <- table(predict,trainData2[1:11,3])
# table_mat
##--------------------------------------------------------------------------
## nn <- neuralnet(ave_sentiment ~ drugName + condition,data = trainData2,hidden=4,stepmax = 1e8,threshold = 0.001,learningrate = 0.001,linear.output=FALSE, act.fct ="logistic")

##-------------------------------------------------------------------------------------------------------
descOrderByColumn <- function(data,newColumnName){
  orderedData <- data %>%
    group_by(columnName) %>%
    tally(sort = T) %>%
    ungroup() %>%
    arrange(desc(n))
  setnames(orderedData,"n","Count")
  setnames(orderedData,"columnName",newColumnName)
  return(orderedData)
}
##-------------------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------
##--------------------------------------------------------K FOLD CROSS VALIDATION
k_fold_cv <- function(data){
  k=10
  data <- trainData3[sample(nrow(trainData3)),] 
  nr <- nrow(data)
  eachFold <- split(data, rep(1:ceiling(nr/k), each=nr/k, length.out=nr))
  return(eachFold[1:10])
}
##-------------------------------------------------------------------------------------------------------
##--------------------------------------------------------get Accuracy
getAccuracy <- function(data){
  folds <- k_fold_cv(trainData3)
  for (i in 1:length(folds)) {
    eachFold <- folds[i]
    trainingSet <- folds[-i]
    testSet<- folds[i]
    predictedValues <- decisionTree(trainingSet,testSet)
  }
}
##-------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Decision Tree
decisionTree <- function(trainDataSet,testDataSet){
  root <- split
}






