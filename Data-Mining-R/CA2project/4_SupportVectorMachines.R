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


##

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
#####################################################
#Setup Over
#####################################################

wr_svm <- wr
names(wr_svm[,-c(1:12,27:30,32,36,38:52,54)])
wr_svm <- wr_svm[,-c(1:12,27:30,32,36,38:52,54)]

library(caret)
index2 <- createDataPartition(wr_svm$sentiment, p = 0.85, list = FALSE)
wr_svm<-wr_svm[-index2, ]

index <- createDataPartition(wr_svm$sentiment, p = 0.75, list = FALSE)
training <- wr_svm[index, ]
testing <- wr_svm[-index, ]


#control parameters
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#linear
svm_Linear <- train(sentiment ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(test_pred, testing$sentiment )



# SUPPORT VECTOR MACHINE (Grid search due to cost)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

svm_Linear_Grid <- train(Survived ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)


svm_Linear_Grid
plot(svm_Linear_Grid)
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
confusionMatrix(test_pred_grid, testing$Survived)


# SUPPORT VECTOR MACHINE RADIAL
svm_Radial <- train(Survived ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10
)

svm_Radial
plot(svm_Radial)

test_pred_Radial <- predict(svm_Radial, newdata = testing)
confusionMatrix(test_pred_Radial, testing$Survived )


#GRID RADIAL SVM
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))




svm_Radial_Grid <- train(Survived ~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

svm_Radial_Grid



plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)
confusionMatrix(test_pred_Radial_Grid, testing$Survived )





