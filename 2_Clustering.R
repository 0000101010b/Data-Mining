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

###########
#imputation
###########

# library(skimr)
# skimmed <- skim_to_wide(wr)
# skimmed
# 
# 
# wr_NoNA <- preProcess(wr, method='knnImpute')
# wr_NoNA
# 
# library(RANN)  # required for knnInpute
# trainData <- predict(wr_NoNA, newdata = wr)
# anyNA(trainData)
#

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




#numerics
n <- sapply(wr, function(x) {is.numeric(x)})
wr_n <- wr[,n]



wr_n$sd_sentiment   <- NULL
wr_n$ave_sentiment <- NULL
wr_n$stars <- NULL

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
wr_nn <- as.data.frame(lapply(wr_n, normalize))
summary(wr_nn)



#categories
wr_c <- wr[,c(names(Filter(is.factor, wr)))]

wr_ctest<-wr_c[,-c(1:8,10:13,19:25,27)]
wr_ctest$hail <- NULL
wr_ctest$mo <-NULL


####################
#K means needs normalized data
####################
library(caret)
library(class)

index <- createDataPartition(wr$sentiment, p = .75, list = FALSE)

train <- wr[index,]
test <- wr[-index,]

train_n <- wr_n[index, ]
test_n <-  wr_n[-index, ]

k1 <- round(sqrt(dim(train_n)[1])) #sqrt of number of instances
k2 <- round(sqrt(dim(train_n)[2])) #sqrt of number of attributes
k3 <- 7 #a number between 3 and 10


knn1 <- knn(train = train_n, test = test_n, cl = train$sentiment, k=k1)
knn2 <- knn(train = train_n, test = test_n, cl = train$sentiment, k=k2)
knn3 <- knn(train = train_n, test = test_n, cl = train$sentiment, k=k3)

(knn1Acc <- 1- mean(knn1 != test$sentiment))
(knn2Acc <- 1- mean(knn2 != test$sentiment))
(knn3Acc <- 1- mean(knn3 != test$sentiment))

caret::confusionMatrix(test$sentiment,knn2, positive = "Postive")


train_nn <- wr_nn[index, ]
test_nn <- wr_nn[-index, ]


wr_k <- cbind(wr_nn, wr_c)

kmeansClusters <- list()
kmeansScores <- c()
for (i in 2:10) {
  clusters <- kmeans(wr_nn, i)
  name <- paste0("kmeans", i)
  kmeansClusters[[name]] <- clusters
  kmeansScores <- rbind(kmeansScores, sum(clusters$withinss))
}


row.names(kmeansScores) <- c(2:10)
colnames(kmeansScores) <- c("k")


plot(kmeansScores, xlab="k", ylab="within groups sum of squares")



library(clusterSim)
kmeansScores <- c()
for (i in 2:10) {
  clusters <- kmeans(wr_nn, i)
  name <- paste0("kmeans", i)
  dbindex <- index.DB(wr_nn, clusters$cluster, centrotypes="centroids")
  kmeansScores <- rbind(kmeansScores, dbindex$DB)
}


row.names(kmeansScores) <- c(2:10)
colnames(kmeansScores) <- c("k")



plot(kmeansScores, xlab="k", ylab="DBIndex")


library(fpc)
plotcluster(wr_nn, kmeansClusters[["kmeans2"]]$cluster)
plotcluster(wr_nn, kmeansClusters[["kmeans4"]]$cluster)
plotcluster(wr_nn, kmeansClusters[["kmeans5"]]$cluster)
plotcluster(wr_nn, kmeansClusters[["kmeans6"]]$cluster)
plotcluster(wr_nn, kmeansClusters[["kmeans10"]]$cluster)



library(cluster)

kmedoidsClusters <- list()
kmedoidsScores <- c()

gower_dist <- daisy(wr_k, metric = "gower", type = list(logratio = 3))

for (i in 2:20) { 
  clusters <- pam(gower_dist, k=i, diss=T) #switched to the full set of attibutes now so need to use medoids
  name <- paste0("kmedoids", i)
  kmedoidsClusters[[name]] <- clusters
  dbindex <- index.DB(gower_dist, clusters$clustering, centrotypes = "medoids", d=gower_dist)
  kmedoidsScores <- rbind(kmedoidsScores, dbindex$DB)
}

row.names(kmedoidsScores) <- c(2:20)
colnames(kmedoidsScores) <- c("k")

plot(kmedoidsScores, xlab="k", ylab="DBIndex")


pcs <- prcomp( ~ ., data=wr_nn) #PCA on categoricals is hard, so let's stick to the numerical attributes
#conveniently, for k-means (I4) and kNN (I2) we normalised the numerical data -- happy days :)
#had we not, prcomp will do this for us using scale=T
plot(pcs, type="l")
#let's get the first 2 PCs
comp <- data.frame(pcs$x[,1:2])

#so we know, from I4 that there are 2 clusters
k <- kmeans(comp, 2)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

