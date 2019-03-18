setwd("C:/Users/BenTh/Desktop/ADM_/ADM/Lab1")
titanicData <-read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)
titanicData$Survived <- factor(titanicData$Survived,levels=c(0,1),labels=c("No","Yes"))


library(C50)


vars <- c("Cabin", "Embarked")
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
cFifty <- C5.0(training[,vars],training$Survived)

c <- predict(cFifty,newdata =  testing)

caret::confusionMatrix(c, testing$Survived, positive="Yes")



cFiftyWinnow <- C5.0(training[,vars],training$Survived, control = C5.0Control(winnow = TRUE))

c <- predict(cFiftyWinnow,newdata =  testing)
caret::confusionMatrix(c, testing$Survived, positive="Yes")

library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=5) #5 x 10-fold cv
metric <- "Kappa"
train(training[,vars],training$Survived, method="C5.0", metric=metric, trControl=control)



library(dummies)

training$Name <- NULL
training$Ticket <- NULL
testing$Ticket<-NULL
testing$Name <- NULL
training$Cabin <- NULL
testing$Cabin <- NULL
training$PassengerId <- NULL
testing$PassengerId <- NULL



dummyDF <- dummy.data.frame(training[,-1])
dummyDF$Survived <- training$Survived
dummyDFTesting <- dummy.data.frame(testing[,-1])
dummyDFTesting$Survived <- testing$Survived
dummyDF$EmbarkedNA <= NULL



cFiftyDummy <- C5.0(Survived ~ .,data = dummyDF ,rules=TRUE)
pred <- predict(cFiftyDummy, dummyDFTesting)
caret::confusionMatrix(pred, dummyDFTesting$Survived, positive="Yes")














