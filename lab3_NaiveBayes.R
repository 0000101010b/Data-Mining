setwd("C:/Users/BenTh/Desktop/ADM_/ADM/Lab1")

titanicData <-read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)
str(titanicData)

titanicData <- titanicData[-which(is.na(titanicData$Age)),]

#titanicData <- titanicData[complete.cases(titanicData),]


titanicData$Survived <- factor(titanicData$Survived,levels=c(0,1),labels=c("No","Yes"))

table(titanicData$SibSp)
table(titanicData$Parch)

titanicData$SibSp <- factor(titanicData$SibSp)
titanicData$Parch <- factor(titanicData$Parch)

par(mfrow=c(1,2))
hist(titanicData$Fare, breaks = 30)
hist(titanicData$Age)

titanicData$FareBinned <- cut(titanicData$Fare,
                              breaks = c(0,10,50,max(titanicData$Fare)),
                              labels=c("low", "middle", "high"))

table(titanicData$FareBinned, titanicData$Pclass)

aggregate(Fare ~ Pclass, data=titanicData, FUN=summary)

titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(0,10,20,30,40,50,60,70,Inf),
                             labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)


titanicData[c(3,11)] <- NULL #remove numeric
titanicData$PassengerId <- NULL
#titanicData$Age <- NULL
titanicData$Fare <- NULL

#titanicData$Age <- NULL
#titanicData$Fare <-NULL

library(e1071)
set.seed(24)
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
nb <- naiveBayes(training, training$Survived)
nb.predict <- predict(nb, newdata = testing)
caret::confusionMatrix(testing$Survived, nb.predict, positive = "Yes")


titanicDataNB <- titanicData

titanicData <- read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)


#titanicData <- titanicData[complete.cases(titanicData),]


titanicData[c(3,11)] <- NULL #remove numeric
titanicData$PassengerId <- NULL
titanicData$Fare <- NULL



titanicDataNB$AgeBinned <-cut(titanicDataNB$Age,
                              breaks = c(0,10,20,30,40,50,Inf),
                              labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50+"))

table(titanicDataNB$AgeBinned)

index <- sample(1:dim(titanicDataNB)[1], dim(titanicDataNB)[1] * .75, replace=FALSE)
training <- titanicDataNB[index, ]

testing <- titanicDataNB[-index, ]
nb <- naiveBayes(training, training$Survived)
nb.predict <- predict(nb, newdata = testing)
caret::confusionMatrix(testing$Survived, nb.predict, positive = "Yes")





