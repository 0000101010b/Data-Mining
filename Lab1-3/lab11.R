titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T);

y <- titanicData$Survived;

table(y);

perish <- c()
coin <- c()

for(i in 1:1000)
{
  index <- sample(1:length(y),length(y) *.25,replace =FALSE)
  testing <- y[index]
  
  coinModel <- round(runif(length(testing), min=0, max=1))
  coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))
  
  coin[i] <- 1 - mean(coinModel != testing)
  perish[i] <- 1 - mean(perishModel != testing)
}

results <- data.frame(coin,perish)
names(results) <- c("Coin Toss Accuracy", "Everyone Perishes Accuracy")

summary(results)


df <- titanicData[, c("Survived", "Sex")]
df$Survived <- factor(df$Survived, levels = c(0,1), labels = c("No", "Yes"))
index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
training <- df[index, ]
testing <- df[-index, ]
table(training$Survived, training$Sex)


predictSurvival <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Sex == 'female'] <- "Yes"
  return(model)
}
women <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  womenModel <- predictSurvival(testing)
  women[i] <- 1 - mean(womenModel != testing$Survived)
}
results$`Women Accuracy` <- women
names(results) <- c("Coin", "All Perish", "Women")
boxplot(results)



table(titanicData$Parch)


par(mfrow=c(1,2))
hist(titanicData$Fare, breaks = 30)
hist(titanicData$Age)

titanicData$FareBinned <- cut(titanicData$Fare,
        breaks = c(0,10,50,max(titanicData$Fare)),
        labels=c("low", "middle", "high"))

table(titanicData$FareBinned, titanicData$Pclass)

aggregate(Fare ~ Pclass, data=titanicData, FUN=summary)

titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(0,10,20,30,40,50,60,70,80,max(titanicData$Age)),
                             labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)


