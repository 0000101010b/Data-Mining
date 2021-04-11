wine <- read.csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
  header=F)
names(wine) <- c(
  "Alcohol",
  "MalicAcid",
  "Ash",
  "Height",
  "Alcalinity",
  "Magnesium",
  "TotalPhenols",
  "Flavanoids",
  "NonflavanoidPhenols",
  "Proanthocyanins",
  "ColorIntensity",
  "Hue",
  "OD280OD315",
  "Proline"
)

wine$Alcohol <- factor(wine$Alcohol) #this should be categorical
barplot(table(wine$Alcohol))

boxplot(wine[,c(2:13)])

sapply(wine,FUN=function(x){sum(is.na(x))})

#
#split(wine,f=wine$Alcohol)
#split.default(wine,drop = FALSE,sep = )

wineKNN<-wine
summary(wineKNN)

set.seed(1337)
library(ggplot2)
library(lattice)
library(caret)


index <- createDataPartition(wineKNN$Alcohol,p=.75,list=FALSE)
trainKNN <- wineKNN[index,]
validationKNN <- wineKNN[-index,]

library(randomForest)

training <-wine[index,]

rf <- randomForest(Alcohol ~ . , data=training,importance = TRUE,ntree = 100)
varImpPlot <- varImpPlot(rf)

#mean decrease accuracy
meanDecAcc <- varImpPlot[,1]
#reorder decending
meanDecAcc <-  meanDecAcc[order(-meanDecAcc)]
#make a vector of numbers
a <- c(1:length(meanDecAcc))
#derive those that are odd
a <- a %% 2 == 1
#get the odd colNames
knnFeatures <- names(meanDecAcc[a])
knnFeatures

tuneParams <- trainControl(
  method = "cv",
  number= 10,
  savePredictions = 'final')

knn <- train(trainKNN[,knnFeatures],trainKNN$Alcohol,method = 'knn',trControl=tuneParams,tuneLength = 10)
knn.pred <-predict(knn,newdata= validationKNN[,knnFeatures])
confusionMatrix(knn.pred,validationKNN$Alcohol)


library(C50)
train <- wine[index,]
validation <- wine[-index,]

c50Features <- names(meanDecAcc[!a])
c50Features

c50Tree <- train(train[,c50Features],train$Alcohol,method="C5.0",trControl=tuneParams,tuneLength=3)

c50.pred <- predict(c50Tree,newdata = validation[,c50Features])
confusionMatrix(c50.pred,validation$Alcohol)


meanDecGini <- varImpPlot[,2]
meanDecGini <- meanDecGini[order(-meanDecGini)]
b <- c(1:length(meanDecGini))
b <- b %% 2 == 1
cartFeatures <- names(meanDecGini[b])
cartFeatures

rpartTree <- train(train[,cartFeatures],train$Alcohol,method="rpart",trControl=tuneParams,tuneLength= 10)
rpart.pred <- predict(rpartTree , newdata = validation[,cartFeatures])
confusionMatrix(rpart.pred,validation$Alcohol)


#combination of 3 ML CART,C5.0,KNN

validation$pred_rpart_prob<-predict(object = rpartTree,validation[, cartFeatures],type='prob')
validation$pred_c50_prob<-predict(object = c50Tree,validation[,c50Features],type='prob')
validation$pred_knn_prob<-predict(object = knn,validationKNN[,knnFeatures],type='prob')

validation$pred_avg<-(validation$pred_rpart_prob+validation$pred_knn_prob+validation$pred_c50_prob)/3

validation$preds <- apply(validation$pred_avg, 1, FUN=function(x) {which.max(x)})

validation$preds <- factor(validation$preds, levels=c(1:3), labels=c(1:3))
confusionMatrix(validation$Alcohol, validation$preds)

#ensembled!!!!

dfPred <- cbind(validation$pred_rpart_prob, validation$pred_c50_prob, validation$pred_knn_prob)
validation$maxConfidence <- apply(dfPred, 1, FUN=function(x) {which.max(x)})
summary(validation$maxConfidence)


validation$maxConfidence[validation$maxConfidence > 6] <-
  validation$maxConfidence[validation$maxConfidence > 6] - 6
validation$maxConfidence[validation$maxConfidence > 3] <-
  validation$maxConfidence[validation$maxConfidence > 3] - 3
summary(validation$maxConfidence)

validation$maxConfidence <- factor(validation$maxConfidence, levels=c(1:3), labels=c(1:3))
confusionMatrix(validation$Alcohol, validation$maxConfidence)



