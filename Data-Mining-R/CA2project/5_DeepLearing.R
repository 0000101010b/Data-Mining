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


h2otrain <- train[, -2]
h2otrain <- cbind(train$Attrition, h2otrain)
colnames(h2otrain)[1] <- "Attrition"

h2otest <- test[, -2]
h2otest <- cbind(test$Attrition, h2otest)
colnames(h2otest)[1] <- "Attrition"

#for simplicity put the dependent first

#this gives us our target to aim for: logitAcc2
write.table(x = h2otrain, file = "training.csv", row.names = F, col.names = T)
write.table(x = h2otest, file = "testing.csv", row.names = F, col.names = T)

library(h2o)
h2o.init(nthreads = -1)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
#convert to the h2o format
h2o_trainset <- h2o.importFile(path = paste0(getwd(), "/training.csv"))
h2o_testset <- h2o.importFile(path = paste0(getwd(), "/testing.csv"))

fit <- h2o.deeplearning(x = 2:29,  # column numbers for predictors
                        y = 1,   # column number for label
                        training_frame = h2o_trainset, # data in H2O format
                        activation = "TanhWithDropout", # or 'Tanh'
                        input_dropout_ratio = 0.2, # % of inputs dropout
                        hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                        hidden = c(150,150,150), # three layers of 50 nodes
                        epochs = 100)

pred <- h2o.predict(fit, h2o_testset)
pred <- as.data.frame(pred)

results.DL <- ifelse(pred$predict > 0.5,1,0)

dlPerformance <- 1 - mean(results.DL != test$Attrition)
(dlPerformance > logitAcc2) #didn't beat it

#let's do SO3 and try a bit harder
hidden_opt <- list(c(200,200), c(100,300,100), c(500,500,500)) 
l1_opt <- c(1e-5,1e-7)
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt)
model_grid <- h2o.grid("deeplearning",
                       hyper_params = hyper_params,
                       x = 2:29,  # column numbers for predictors
                       y = 1,   # column number for label
                       training_frame = h2o_trainset,
                       validation_frame = h2o_testset)

dlPerf <- c()

for (model_id in model_grid@model_ids) {
  model <- h2o.getModel(model_id)
  pred <- h2o.predict(model, h2o_testset)
  pred <- as.data.frame(pred)
  
  results.DL <- ifelse(pred$predict > 0.5,1,0)
  
  dlPerformance <- 1 - mean(results.DL != test$Attrition)
  dlPerf <- rbind(dlPerf, dlPerformance)
}

bestDL <- max(dlPerf)
(bestDL > logitAcc2) #still didn't beat it

#last attempt -- under regular circumstances, we probably wouldn't take a shot in the dark like this
hidden_opt <- list(c(100, 100, 100, 100), c(200, 200, 200, 200), c(300, 300, 300, 300)) 
l1_opt <- c(1e-5,1e-7)
activations <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt, activation=activations)
model_grid <- h2o.grid("deeplearning",
                       hyper_params = hyper_params,
                       x = 2:29,  # column numbers for predictors
                       y = 1,   # column number for label
                       training_frame = h2o_trainset,
                       validation_frame = h2o_testset)

dlPerf <- c()

for (model_id in model_grid@model_ids) {
  model <- h2o.getModel(model_id)
  pred <- h2o.predict(model, h2o_testset)
  pred <- as.data.frame(pred)
  
  results.DL <- ifelse(pred$predict > 0.5,1,0)
  
  dlPerformance <- 1 - mean(results.DL != test$Attrition)
  dlPerf <- rbind(dlPerf, dlPerformance)
}

bestDL <- max(dlPerf)

bestDL

