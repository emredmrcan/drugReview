# install.packages("sentimentr")
# install.packages("dplyr")
#install.packages("e1071")
#install.packages("stringi")
#install.packages("reticulate")
#install.packages("rlist")

library(sentimentr)
library(dplyr)
library(e1071)
library(data.table)
library(rpart)
library(neuralnet)
library(reticulate)
library(rlist)

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
write.csv(trainData3,'newTrainData.csv')

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
trainData3 <- trainData2
maxDepth = 10
minSize = 5

getAccuracy(trainData3)
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
    trainingSetDF <- data.frame()
    testSetDF <- data.frame()
    for (j in 1:length(trainingSet)) {
      trainingSetDF <- rbind(trainingSetDF,trainingSet[[j]])
    }
    testSetDF<- rbind(testSetDF,testSet[[1]])
    remove(trainingSet)
    remove(testSet)
    #predictedValues <- decisionTree(trainingSetDF,testSetDF)
  }
}
##-------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Decision Tree
decisionTree <- function(trainDataSet,testDataSet){
  #root <- split(trainDataSet)
  root3 <- split(trainingSetDF)
   root3 <- root
   root3$div[[1]] <- root3$div[[1]][1:200,]
   root3$div[[2]] <- root3$div[[2]][1:100,]
  root2<-rootSplit(root3,1)
  predictions <- list()
  for (i in 1:100) { #length(testDataSet[,1])
    prediction = output_from_tree(root2,testSetDF[i,])    #testDataSet[i,])
    if(is.null(prediction)){
      browser()
      print("asd")
    }
    predictions <- c(predictions,prediction)
  }
  # count <- 0
  # for (i in 1:length(predictions)) {
  #   if(predictions[[1]]==testSetDF[i,3]){
  #     count <- count + 1
  #   }
  # }
  return(predictions)
}
#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Split
split <- function(data){
  targetValues <- unique(data$ave_sentiment)
  node_index <- 999
  node_value <- 999
  node_score <- 999
  node_groups <- list()
  for (index in 1:(length(data)-1)) {
    count <- 0
    count_row <- 0
    for (i in 1:length(data[,1])) {
      count <- count + 1
      count_row <- as.integer(data[i,index]) + count_row
    }
    count_row <- count_row / count
    groups <- testSplit(index, count_row, data)
    gini_value = getGiniIndex(groups, targetValues)
    if(gini_value < node_score){
      node_index<-index
      node_value<- as.integer(data[i,index])
      node_score<-gini_value
      node_groups<-groups
    }
  }
  return(list("i"=node_index,"value"=node_value,"div"=node_groups))
}
#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Test Split
testSplit <- function(index,value, data){
  left<-list()
  right<-list()
  for (i in 1:length(data[,1])) {
    if(as.integer(data[i,index]) < value){
      left <- rbind(left,data[i,])
    }
    else{
      right <- rbind(right,data[i,])
    }
  }
  return(list(left,right))
}

#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Gini
getGiniIndex <- function(groups,targetValues){
  giniIndexValue <- 0
  for (i in 1:length(targetValues)) {
    for (j in 1:length(groups)) {
      number <- 0
      if(length(groups[[j]]) != 0){
        size <- length(groups[[j]][,1])
      }
      else{
        size <- 0
      }
      if(size==0){
        next
      }
      number <- length(groups[[j]][which(groups[[j]][,3]==targetValues[i]),][,1])
      ratio <- number / size
      giniIndexValue <- giniIndexValue + (ratio * (1-ratio))
    }
  }
  return(giniIndexValue)
}

#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Node Split
rootSplit <- function(root,depth){
  left <- root$div[[1]]
  right <- root$div[[2]]
  root <- list.remove(root,'div')
    if(length(left)==0){
      root$left<- -1
      root$right<- 1
      return()
    }
    if(length(right)==0){
      root$left<- 1
      root$right<- -1
      return()
    }
    if(depth>=maxDepth){
      root$left<- terminalNode(left)
      root$right<- terminalNode(right)
      return()
    }
    if(length(left)!=0){
      lengthLeft<- length(left[,1])
    }else{
      lengthLeft<- 0
    }
    if(lengthLeft <= minSize){
      root$left<- terminalNode(left)
    }else{
      root$left <- split(as.data.frame(left))
      root$left <- rootSplit(root$left,depth+1)
    }
    if(length(right) != 0){
      lengthRight<- length(right[,1])
    }else{
      lengthRight<- 0
    }
    if(lengthRight <= minSize){
      root$right<- terminalNode(right)
    }else{
      root$right <- split(as.data.frame(right))
      root$right <- rootSplit(root$right,depth+1)
    }
  return(root)
}
#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------Terminal Node
terminalNode <- function(group){
  outcomes <- group[,3]
  classify <- c(0,0)
  classify[1]<- length(which(group[,3]==-1))
  classify[2]<- length(which(group[,3]==1))
  if(classify[1] > classify[2]){
    return(-1)
  }
  else{
    return(1)
  }
}
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------output_from_tree
output_from_tree <- function(root,row){
  if(is.null(root)){
    return(1)
  }
  else if(is.null(root$left) && is.null(root$right) ){
    return(1)
  }
  else if(as.integer(row[,as.integer(root$i)])<as.integer(root$value)){
    if(is.null(root$left)){
      return(output_from_tree(root$right,row))
    }else{
      if(typeof(root$left) == "list"){
        return(output_from_tree(root$left,row))
      }else{
        return(root$left)
      }
    }
  }else{
    if(typeof(root$right) == "list"){
      return(output_from_tree(root$right,row))
    }else{
      return(root$right)
    }
  }
}
#--------------------------------------------------------------------------------------------------------

unique(trainingSetDF$ave_sentiment)
#rbind(trainingSet[[1]],trainingSet[[2]])
#folds[[1]][1:10,2]
length(groups[[1]][which(groups[[1]][,3]==targetValues[2]),][,1])
