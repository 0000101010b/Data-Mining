setwd("C:/Users/BenTh/Desktop/ADM_/ADM/Lab1")
titanicData <-read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)
titanicData$Survived <- factor(titanicData$Survived,levels=c(0,1),labels=c("No","Yes"))

index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
train <- titanicData[index, ]
test <- titanicData[-index, ]




library(rpart)
library(rpart.plot)
library(RColorBrewer)

regressionTree <- rpart(Survived ~ ., data=train, method="class")
tree <- rpart(Survived ~ Pclass + Fare, data=train, method="class")

plot(regressionTree)
text(regressionTree)

plot(tree)
text(tree)

summary(tree)


library("rattle")
fancyRpartPlot(tree)

rpartPrediction <- predict(tree, test, type = "class")
library(caret)


confusionMatrix(rpartPrediction, test$Survived, positive = "Yes")


newRpart <- rpart(Survived ~ Pclass + Fare + Sex , data=train, method="class", control=rpart.control(minsplit=2, cp=0))

fancyRpartPlot(newRpart)



rpartPrediction <- predict(newRpart, test, type = "class")
confusionMatrix(rpartPrediction, test$Survived, positive = "Yes")


rpartPrediction <- predict(newRpart, train, type = "class")
confusionMatrix(rpartPrediction, train$Survived, positive = "Yes")


#treeToPrune <- rpart(Survived ~ ., data=train, method="class", control=rpart.control( your controls ))
#prunedTree <- prp(treeToPrune,snip=TRUE)$obj
#fancyRpartPlot(prunedTree)


library(randomForest)


forest <- randomForest(Survived ~ Pclass + Fare + Sex, data=train, importance=TRUE, ntree=2000)


varImpPlot(forest)


rf <- predict(forest, test, type = "class")
confusionMatrix(rf, test$Survived, positive = "Yes")

###


library(partykit)


cTree <- ctree(Survived ~ Pclass + Fare + Sex, data=train)
print(cTree)


plot(cTree, type="simple")

rf <- predict(cTree, test, type = "class")
confusionMatrix(rf, test$Survived, positive = "Yes")

set.seed(290875)

# cForest <- cforest(Survived ~ Pclass + Fare + Sex +Cabin,
#                    data=train,
#                    
#                    controls=
#                      cforest_unbiased(ntree=2000,mtry=3))

library(dummies)
## dummies-1.5.6 provided by Decision Patterns
n <- sapply(titanicData, function(x) {is.numeric(x)})
titanicDataRotF <- titanicData[, !n]
titanicDataRotF <- dummy.data.frame(titanicDataRotF[, -1])
titanicDataRotF <- cbind(titanicDataRotF, titanicData[, n])
Survived <- factor(titanicData$Survived, levels=c("Yes", "No"), labels=c(1,0))
str(titanicDataRotF)

index <- sample(1:dim(titanicDataRotF)[1], dim(titanicDataRotF)[1] * .75, replace=FALSE)
trainRF <- titanicDataRotF[index, ]
testRF <- titanicDataRotF[-index, ]

library(rotationForest)
rotForest <- rotationForest(y=Survived[index], x=trainRF)






                   
                   












