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




