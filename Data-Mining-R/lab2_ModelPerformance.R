setwd("C:/Users/BenTh/Desktop/ADM_/ADM/Lab1")
titanicData <-read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)
y <-titanicData$Survived
table(y)
y <- factor(y,levels = c(0,1),labels = c("No","Yes"))
table(y)
prop.table(table(y))
barplot(table(y),main = "Distrib of Titanic Survival",ylab = "Frequency")
barplot(prop.table(table(y)),main = "Distrib of Titanic Survival",ylab = "Frequency")

set.seed(1337)
index <- sample(1:length(y),length(y)*.25,replace=FALSE)
testing <- y[index]

perishModel <-rep("No",length(testing))

coinModel <- round(runif(length(testing),min=0,max=1))
coinModel <- factor(coinModel,levels=c(0,1),labels = c("No","Yes"))

perishModel <- factor(perishModel,levels=c("No","Yes"),labels = c("No","Yes"))
table(testing,perishModel)

coinAccuracy   <- mean(coinModel   == testing)
perishAccuracy <- mean(perishModel == testing)


perish <- c()
coin <- c()

for (i in 1:1000){
  index <-sample(1:length(y),length(y)* .25, replace = FALSE)
  testing <- y[index]
  
  coinModel <- round(runif(length(testing),min=0,max=1))
  coinModel <- factor(coinModel,levels=c(0,1),labels = c("No","Yes"))
  
  coin[i] <- mean(coinModel==testing)
  perish[i]<-mean(perishModel == testing)
}

results <- data.frame(coin,perish)
names(results) <- c("Coin Toss Accuracy", "Everyone Perishes Accuracy")
summary(results)

library(ggplot2)
library(reshape)
ggplot(melt(results), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)


df <- titanicData[, c("Survived", "Sex")]
df$Survived <- factor(df$Survived, levels = c(0,1),labels = c("No","Yes"))

index <- sample(1:dim(df)[1],dim(df)[1]*.75,replace = FALSE)
training <- df[index,]
testing <- df[-index,]

table(training$Survived,training$Sex)

predictSurvival <- function(data){
  model <- rep("No",dim(data)[1])
  model[data$Sex == 'female']<- "Yes"
  return(model)
}




women <- c()

for (i in 1:1000){
  index <-sample(1:dim(df)[1],dim(df)[1]*.75,replace = FALSE)
  testing <- df[-index,]
  
  womenModel <- predictSurvival(testing)
  
  women[i] <- mean(womenModel == testing$Survived)
}

results$'Women Accuracy' <- women
names(results) <- c("Coin","All Perish","Women")

boxplot(results)



df <- titanicData[,c("Survived","Age")]
df$Survived <- factor(df$Survived,levels=c(0,1),labels=c("No","Yes"))
#df <- df[-which(is.na(df$Age)),]
#c(sum(df$Age<18,na.rm = TRUE),sum(df$Age>=18,na.rm = TRUE))
df$Adult=df$Age>=18


index <- sample(1:dim(df)[1] *.75,replace=FALSE)
training <- df[index,]
testing <- df[-index,]

table(training$Survived,training$Adult)



predictSurvivalAge <- function(data){
  model <- rep("No",dim(data)[1])
  model[data$Age>=18] <- "No"
  return(model)
}

age <- c()

for(i in 1:1000)
{
  index <- sample(1:dim(df)[1],dim(df)[1] * .75,replace=FALSE)
  testing <- df[-index,]
  
  ageModel <- predictSurvivalAge(testing)
  
  age[i] <- mean(ageModel==testing$Survived)
  
}

results$'Age Accuracy' <- age
names(results) <- c("Coin", "All Perish", "Women","Age")

boxplot(results)


df <- titanicData[,c("Survived","Pclass")]
df$Survived <- factor(df$Survived,levels=c(0,1),labels=c("No","Yes"))

df$class1<- df$Pclass == 1
df$class2<- df$Pclass == 2
df$class3<- df$Pclass == 3


predictSurvivalClass <- function(data,class){
  model <- rep("No",dim(data)[1])
  model[data$class] <- "No"
  return(model)
}

class1st <- c()

for(i in 1:1000)
{
  index <- sample(1:dim(df)[1],dim(df)[1] * .75,replace=FALSE)
  testing <- df[-index,]
  
  class1stModel <- predictSurvivalAge(testing)
  
  class1st[i] <- mean(ageModel==testing$Survived)
  
}


titanicData$Survived <- factor(titanicData$Survived,levels=c(0,1),labels=c("No","Yes"))

index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
womenModel <- factor(womenModel)


library(gmodels)
CrossTable(testing$Survived,womenModel)


CrossTable(testing$Survived, womenModel, prop.chisq = F, prop.c = F, prop.r = F)


library(lattice)
library(ggplot2)
library(e1071)
library(caret)


confusionMatrix(testing$Survived,womenModel,positive = "Yes")



# MODEL METICS MASK CARET LIBRARY
# 
# library(ModelMetrics)
# auc(testing$Survived, womenModel)
# 
# 
# library(ROCR)
# pWomenModel <- prediction(as.numeric(womenModel), as.numeric(testing$Survived))
# perfWomenModel <- performance(pWomenModel, measure = "tpr", x.measure = "fpr")
# plot(perfWomenModel)
# 
# 
# auc <- performance(pWomenModel, measure = "auc")
# auc <- auc@y.values[[1]]
# auc


library(mice)
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1,11)] #remove feature 1 and 11
titanicData$Embarked[c(62, 830)] <- 'C' #result of Embarked imputation exercise
4
#use a random forest to impute missing age values
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)
#feature engineer a few more things using the passenger name
titanicData$Name <- as.character(titanicData$Name)
titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1
#remove features 3, 7, and 11
titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
# feature engineer a family size categorical variable
titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)

contrasts(titanicData$Sex)

contrasts(titanicData$Pclass)

df <- titanicData
index <- sample(1:dim(df)[1],dim(df)[1]*.75,replace = FALSE)
training <- df[index,]
testing <- df[-index,]



logit <- glm(Survived ~.,family=binomial(link='logit'),data=training)
summary(logit)

anova(logit, test="Chisq")#table of deviance


library(pscl)
pR2(logit)



index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .8, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]

logit <- glm(Survived ~.,family=binomial(link='logit'),data=training)
summary(logit)



logit.prediction <- predict(logit,newdata=testing,type='response')

#results.logit <- ifelse(logit.prediction > 0.5,"Yes","No")



for (i in 1:10) {
  results.logit <- ifelse(logit.prediction > (i/10),"Yes","No")
  results.logit <- as.vector(results.logit)
  results.logit <- factor(results.logit, levels = c("No", "Yes"), labels = c("No", "Yes"))
  cf <- confusionMatrix(results.logit, testing$Survived, positive = "Yes")
  if (i == 1) {
    results <- as.data.frame(t(cf$byClass))
    results <- cbind(results, as.data.frame(t(cf$overall)))
  } else {
    df <- as.data.frame(t(cf$byClass))
    df <- cbind(df, as.data.frame(t(cf$overall)))
    results <- rbind(results, df)
  }
}

par(mfrow=c(2,2))
plot(results$Sensitivity, main = "Sensitivity over i", ylab="Sensitivity", xlab="i")
plot(results$Specificity, main = "Specificity over i", ylab="Specificity", xlab="i")
plot(results$Accuracy, main = "Accuracy over i", ylab="Accuracy", xlab="i")
plot(results$Kappa, main = "Kappa over i", ylab="Kappa", xlab="i")



n <- sapply(titanicData, function(x) {is.numeric(x)})
n

numerics <-titanicData[, n]
summary(numerics)


normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numericsNormal <- normalize(numerics)
summary(numericsNormal)


par(mfrow=c(1,2))
hist(titanicData$Age)
hist(numericsNormal$Age)

titanicDataKNN <- titanicData[, !n]
titanicDataKNN <- cbind(titanicDataKNN, numericsNormal)


library(dummies)
tkNN <- dummy.data.frame(titanicDataKNN[, -1])
summary(tkNN)

Survived <- titanicDataKNN$Survived

index <- sample(1:dim(tkNN)[1], dim(tkNN)[1] * .8, replace=FALSE)
kNNTraining <- tkNN[index, ]
kNNTesting <- tkNN[-index, ]
survivedTrain <- Survived[index]
survivedTest <- Survived[-index]


library(ModelMetrics)

library(class)
folds <- createFolds(Survived, k = 10)
cv_results <- lapply(folds, function(x) {
  knn_train <- tkNN[x, ]
  knn_test <- tkNN[-x, ]
  survivedTrain <- Survived[x]
  survivedTest <- Survived[-x]
  knn_model <- knn(train = knn_train, test = knn_test, cl = survivedTrain, k=3)
  a <- auc(survivedTest, knn_model)
  return(a)
})
auroc <- unlist(cv_results)
summary(auroc)


# define training control parameters
train_control <- trainControl(method="cv", number=10)
model <- train(y=Survived, x=tkNN, trControl=train_control, method="knn")
print(model)




# k1 <- round(sqrt(dim(kNNTraining)[1])) #sqrt of number of instances
# k2 <- round(sqrt(dim(kNNTraining)[2])) #sqrt of number of attributes
# k3 <- 7 #a number between 3 and 10
# 
# knn1 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k1)
# knn2 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k2)
# knn3 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k3)

perf <- c()
folds <- createFolds(Survived, k = 10)
for (i in 1:30) {
  cv_results <- lapply(folds, function(x) {
    knn_train <- tkNN[x, ]
    knn_test <- tkNN[-x, ]
    survivedTrain <- Survived[x]
    survivedTest <- Survived[-x]
    knn_model <- knn(train = knn_train, test = knn_test, cl = survivedTrain, k=i)
    a <- auc(survivedTest, knn_model)
    return(a)
  })
  perf[i] <- mean(unlist(cv_results))
}
plot(perf, xlab="k", ylab="AUROC")




