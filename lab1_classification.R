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






