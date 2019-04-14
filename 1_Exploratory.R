setwd("C:/Users/BenTh/Desktop/WR/data")
wr<-read.csv("qc14.csv", header = T,na.strings =c(""),stringsAsFactors =T)
set.seed(666)

library(dplyr)
glimpse(wr)

#get rid of index
wr <- wr[,-1]

wr$sd_sentiment<- as.numeric(wr$sd_sentiment)
wr$stars_f <- factor(wr$stars)
wr$date <- as.Date(wr$date)
wr$stn <- as.factor(wr$stn)
wr$year <- as.factor(wr$year)
wr$mo <- as.factor(wr$mo)
wr$da <- as.factor(wr$da)
wr$usaf <- as.factor(wr$usaf)
wr$wban <- as.factor(wr$wban)
wr$fog <- factor(wr$fog, levels = c(0,1))
wr$rain_drizzle <- factor(wr$rain_drizzle, levels = c(0,1))
wr$snow_ice_pellets <- factor(wr$snow_ice_pellets, levels = c(0,1))
wr$hail <-  factor(wr$hail, levels = c(0,1))
wr$thunder <-  factor(wr$thunder, levels = c(0,1))
wr$tornado_funnel_cloud <-  factor(wr$tornado_funnel_cloud, levels = c(0,1))
 

summary(wr)# no hail and no tornadoes




table(wr$distance.km.)
table(wr$sndp) #all missing values

wr$sndp <- NULL #all missing
wr$year <- NULL #all 2014

wr$dewp[wr$dewp == 9999.9] <- NA
wr$slp[wr$slp == 9999.9] <- NA
wr$stp[wr$stp == 9999.9] <- NA
wr$visib[wr$visib == 999.9] <- NA
wr$wdsp[wr$wdsp == 9999.9] <- NA
wr$prcp[wr$prcp == 99.99] <- NA
wr$gust[wr$gust == 999.9] <- NA
wr$min[wr$min == 9999.9] <- NA
wr$max[wr$max == 9999.9] <- NA
wr$mxpsd[wr$mxpsd == 999.9] <- NA


#consider using mice
wr$gust[is.na(wr$gust)] <- mean(wr$gust,na.rm= T)
wr$dewp[is.na(wr$dewp)] <- mean(wr$dewp,na.rm= T)
wr$slp[is.na(wr$slp)] <- mean(wr$slp,na.rm= T)
wr$stp[is.na(wr$stp)] <- mean(wr$stp,na.rm= T)
wr$visib[is.na(wr$visib)] <- mean(wr$visib,na.rm= T)
wr$wdsp[is.na(wr$wdsp)] <- mean(wr$wdsp,na.rm= T)
wr$prcp[is.na(wr$prcp)] <- mean(wr$prcp,na.rm= T)
wr$min[is.na(wr$min)] <- mean(wr$min,na.rm= T)
wr$max[is.na(wr$max)] <- mean(wr$max,na.rm= T)
wr$mxpsd[is.na(wr$mxpsd)] <- mean(wr$mxpsd,na.rm= T)


table(wr$slp)# will need to edit rf method?


#count_***** = number of oberservation in capturing average weather numeric 
boxplot(wr$count_temp,wr$count_dewp,wr$count_slp,wr$count_stp,wr$count_wdsp,wr$count_visib)
#count_slp average is 1 (may be unreliable)


#normalise
boxplot(wr$prcp,wr$dewp,wr$temp,wr$slp,wr$wdsp,wr$stp,wr$visib,wr$gust)
###############################
#Observe numerics
###############################
#wr_n <-  wr[,  -which(names(wr) %in% c(names(Filter(is.factor, wr))))]
n <- sapply(wr, function(x) {is.numeric(x)})
wr_n <- wr[,n]


#observations across months
plot(table(wr$mo))

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
nn <- normalize(wr_n)
summary(nn)


boxplot(wr_n$count_temp,wr$count_dewp,wr$count_slp,wr$count_stp,wr$count_wdsp,wr$count_visib)
#count_slp average is 1 (may be unreliable)

#normalise
boxplot(nn$prcp,nn$dewp,nn$temp,nn$slp,nn$wdsp,nn$stp,nn$visib,nn$gust)

library(corrplot)
corrplot(cor(wr_n), method="number")
corrplot(cor(wr_n), method="color")
corrplot(cor(wr_n), type="upper", order="hclust")


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(wr_n)
head(p.mat[, 1:5])

#library("PerformanceAnalytics")
#chart.Correlation(wr_n, histogram=TRUE, pch=19)

corrplot(cor(wr_n), type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(wr_n[,c(1,2,4,17,10,12,18,25)]), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

hist(wr$temp)
hist(wr$prcp)

boxplot(wr$temp ~ wr$sentiment) #negative with higher temp
boxplot(wr$prcp ~ wr$sentiment)
boxplot(wr$dewp ~ wr$sentiment)

#prcp has to many missing values
table(wr$prcp)# remove na with average or rf
plot(wr$prcp, wr$temp, main = "Scatter plot of Age vs. Monthly Rate")
plot(wr$dewp, wr$temp, main = "Scatter plot of Dew vs. Temperature")


###############################
#Observe categories
###############################
wr_c <- wr[,c(names(Filter(is.factor, wr)))]

#observations across months
plot(table(wr_c$mo))

spineplot(wr_c$sentiment,wr_c$rain_drizzle)
spineplot(wr_c$sentiment,wr_c$fog)
spineplot(wr_c$sentiment,wr_c$hail)#no hail
spineplot(wr_c$sentiment,wr_c$snow_ice_pellets)
spineplot(wr_c$sentiment,wr_c$tornado_funnel_cloud)#thunder
spineplot(wr_c$sentiment,wr_c$thunder)

#not much predictive power among them (rain highest diff)


#####
#Observing the dependent variable
#####

y <- wr$sentiment 

table(y)

prop.table(table(y))

barplot(table(y), main = "Distribution of Sentiment", ylab="Frequency")

#####################

#Benchmarks
#1 Everyone Positive:
str(y) # remain (Y) is 0
b1 <- rep("Postive", dim(wr)[1])
(accuracyB1 <- 1 - mean(b1 != y))


#2 Raining:
str(wr$rain_drizzle)
b2 <- rep("Postive", dim(wr)[1])
b2[wr$rain_drizzle == 1] <- "Negative"
(accuracyB2 <- 1 - mean(b2 != wr$sentiment))

#temperature
str(wr$temp)

hist(wr$temp)

b3 <- rep("Negative", dim(wr)[1])
b3[wr$temp > 41] <- "Postive"# greater than 10 celius
(accuracyB3 <- 1 - mean(b3 != wr$sentiment))


#####################################
#Binning numerics
#####################################
summary(wr_n$temp)
#temp
hist(wr$temp)
wr$tempBinned <- cut(wr$temp,
             breaks = c(-30,0,10,20,32,40,50,60,72,70,90,100,Inf),
             labels = c("Very Frigid", "Frigid", "Very Cold","Cold","Water Freezes","Cool","Mild","Warm","Room Temperature","Hot","Very Hot", "Heat Wave" ))
#dewp
summary(wr_n$dewp)
hist(wr$dewp)
wr$dewpBinned <- cut(wr$dewp,
                     breaks = c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,Inf),
                     labels = c("-40to-30", "-30to-20", "-20to-10","-10to0","0-10","10-20","20-30","30-40","40-50","50-60","60-70", "70-80","80+"))
#prcp


#visib

#wdsp

#gust

#mxwpsd


###
library(caret)
wr_ctest<-wr_c[,-c(1:8,10:13,19:25,27)]
wr_ctest$hail <- NULL
wr_ctest$mo <-NULL
wr_ctest$tempBin <- wr$tempBinned
wr_ctest$dewpBin <- wr$dewpBinned
sample <- createDataPartition(wr_ctest$sentiment, p = .75, list = FALSE)
train <- wr_ctest[sample, ]
test <-  wr_ctest[-sample, ]


#Logistic regression
logit <- glm(sentiment ~.,family=binomial(link='logit'),data=train)
#Start of Question: 11
results.2.logit <- predict(logit,newdata=test,type='response')
results.2.logit <- ifelse(results.2.logit > 0.6,"Postive","Negative")
(logitAcc1 <- 1- mean(results.2.logit != test$sentiment))
results.2.logit <- ifelse(results.2.logit > 0.5, "Postive","Negative")
(logitAcc2 <- 1- mean(results.2.logit != test$sentiment))



wr_svm <- wr

names(wr_svm[,-c(1:12,27:30,32,36,38:52,54)])

wr_svm <- wr_svm[,-c(1:12,27:30,32,36,38:52,54)]

library(caret)
index <- createDataPartition(wr_svm$sentiment, p = 0.75, list = FALSE)
training <- wr_svm[index, ]
testing <- wr_svm[-index, ]


#control parameters
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#linear support vector machine
svm_Linear <- train(sentiment ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(test_pred, testing$sentiment )

