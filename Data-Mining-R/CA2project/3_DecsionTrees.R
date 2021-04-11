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
                     labels = c("Very_Frigid", "Frigid", "Very_Cold","Cold","Water_Freezes","Cool","Mild","Warm","Room_Temperature","Hot","Very_Hot", "Heat_Wave" ))
#dewp
summary(wr_n$dewp)
hist(wr$dewp)
wr$dewpBinned <- cut(wr$dewp,
                     breaks = c(-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,Inf),
                     labels = c("m40tom30", "m30_m20", "m20tom10","m10to0","0to10","10to20","20to30","30to40","40to50","50to60","60to70", "70to80","80plus"))
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


##################################################
#set up training data
library(caret)

i1 <- createDataPartition(wr_ctest$sentiment, p = .90, list = FALSE)
wr_ctest <- wr_ctest[-i1, ]

index <- createDataPartition(wr_ctest$sentiment, p = .75, list = FALSE)
training <- wr_ctest[index, ]
testing <-  wr_ctest[-index, ]


##################################################

library(C50)

treeModel <- C5.0(sentiment ~ .,data = training)
treeModel
summary(treeModel)
#C5imp(treeModel)

c <- predict(treeModel,testing)
caret::confusionMatrix(testing$sentiment, c , positive = "Postive")

ruleModel <- C5.0(sentiment ~ ., data = training, rules = TRUE)
ruleModel
summary(ruleModel)

c <- predict(ruleModel,testing)#guess all postive
caret::confusionMatrix(testing$sentiment, c , positive = "Postive")

################
#http://jmlr.org/papers/volume15/delgado14a/delgado14a.pdf
################
##
#C50 kappa metric with repeated cross validation
##

control <- trainControl(method="repeatedcv", number=10, repeats=5) #5 x 10-fold cv
metric <- "Kappa"

c50 <- train(sentiment ~.,
             data = training,
             method="C5.0",
             metric=metric,
             trControl=control)
c50

test_pred <- predict(c50, newdata = test)

confusionMatrix(test_pred, test$sentiment)

######################################################################
#Tuned C50 
######################################################################

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")


#set features and classes
x <- training[,-c(5)]
y <- training$sentiment

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

mdl<- train(x=x,
            y=y,
            tuneGrid=grid,
            trControl=fitControl,
            method="C5.0",
            verbose=FALSE)

mdl


xyplot(mdl,type = c("g", "p", "smooth"))
plot(mdl)

#####################
#Custom c50 functions
#####################

library(dplyr)
C5CustomSort <- function(x) {
  
  x$model <- factor(as.character(x$model), levels = c("rules","tree"))
  x[order(x$trials, x$model, x$splits, !x$winnow),]
  
}

C5CustomLoop <- function (grid) 
{
  loop <- plyr::ddply(grid, c("model", "winnow","splits"), function(x) c(trials = max(x$trials)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for (i in seq(along = loop$trials)) {
    index <- which(grid$model == loop$model[i] & grid$winnow == 
                     loop$winnow[i] & grid$splits == loop$splits[i])
    trials <- grid[index, "trials"]
    submodels[[i]] <- data.frame(trials = trials[trials != 
                                                   loop$trials[i]])
  }
  list(loop = loop, submodels = submodels)
}


C5CustomGrid <- function(x, y, len = NULL) {
  c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
  expand.grid(trials = c5seq, splits = c(2,10,20,50), winnow = c(TRUE, FALSE), model = c("tree","rules"))
}


C5CustomFit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  # add the splits parameter to the fit function
  # minCases is a function of splits
  
  theDots <- list(...)
  
  splits   <- param$splits
  minCases <- floor( length(y)/splits ) - 1
  
  if(any(names(theDots) == "control"))
  {
    theDots$control$winnow        <- param$winnow
    theDots$control$minCases      <- minCases
    theDots$control$earlyStopping <- FALSE
  }
  else
    theDots$control <- C5.0Control(winnow = param$winnow, minCases = minCases, earlyStopping=FALSE )
  
  argList <- list(x = x, y = y, weights = wts, trials = param$trials, rules = param$model == "rules")
  
  argList <- c(argList, theDots)
  
  do.call("C5.0.default", argList)
  
}

GetC5Info <- function() {
  
  # get the default C5.0 model functions
  c5ModelInfo <- getModelInfo(model = "C5.0", regex = FALSE)[[1]]
  
  # modify the parameters data frame so that it includes splits
  c5ModelInfo$parameters$parameter <- factor(c5ModelInfo$parameters$parameter,levels=c(levels(c5ModelInfo$parameters$parameter),'splits'))
  c5ModelInfo$parameters$label <- factor(c5ModelInfo$parameters$label,levels=c(levels(c5ModelInfo$parameters$label),'Splits'))
  c5ModelInfo$parameters <- rbind(c5ModelInfo$parameters,c('splits','numeric','Splits'))
  
  # replace the default c5.0 functions with ones that are aware of the splits parameter
  c5ModelInfo$fit  <- C5CustomFit
  c5ModelInfo$loop <- C5CustomLoop
  c5ModelInfo$grid <- C5CustomGrid
  c5ModelInfo$sort <- C5CustomSort
  
  return (c5ModelInfo)
}

c5info <- GetC5Info()

# Define the structure of cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10,  repeats = 10)

# create a custom cross validation grid
grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model=c("tree"), .splits=c(2,5,10,15,20,25,50,100) )

# Choose the features and classes


# Tune and fit model
mdl<- train(x=x,
            y=y,
            tuneGrid=grid,
            trControl=fitControl,
            method=c5info,
            verbose=FALSE)

mdl

plot(mdl)

##################
#regression tree
##################
library(rpart)
library(rpart.plot)
library(RColorBrewer)
regressionTree <- rpart(sentiment ~ ., data=training, method="class")
plot(regressionTree)
text(regressionTree)
summary(regressionTree)


rpartPrediction <- predict(regressionTree, testing, type = "class")
confusionMatrix(rpartPrediction, testing$sentiment, positive = "Postive")

##################
#rpart tree 2
library(rattle)
newRpart <- rpart(sentiment ~ ., data=training, method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(newRpart)#overplot


rpartPrediction <- predict(newRpart, testing, type = "class")
confusionMatrix(rpartPrediction, testing$sentiment, positive = "Postive")

###################
library(randomForest)

forest <- randomForest(sentiment ~ ., data=training, importance=TRUE, ntree=2000)
varImpPlot(forest)
rf <- predict(forest, testing, type = "class")

confusionMatrix(rf, testing$sentiment, positive = "Postive")
####################


library(partykit)


# cTree <- ctree(sentiment ~ ., data=training)
# print(cTree)
# 
# plot(cTree, type="simple")
# ctreePredict <- predict(cTree, testing, type = "node")
# #r<-ifelse(ctreePredict > 0.5, "Postive","Negative")
# r <- factor(r)
# confusionMatrix(r, testing$sentiment, positive = "Postive")

# 
# 
#  cForest <- cforest(sentiment ~ .,
#                     data=training,
#                     
#                     controls=
#                       cforest_unbiased(ntree=2000,mtry=3))



#
# RANDOM FORESTS 
# modified version of https://shiring.github.io/machine_learning/2017/04/02/unbalanced 
#
# SAMPLING 
# 1. ORIGINAL
# 2. UNDER-SAMPLING
# 3. OVER-SAMPING
# 4. ROSE 
# 5. SMOTE
# 
#REQUIRES INSTALL OF ROSE AND SMOTE


#################################
#Mode of Original unbalanced data
#################################
set.seed(666)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE)


model_rf <- caret::train(sentiment ~ .,
                         data = training,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)



final <- data.frame(actual = testing$sentiment,
                    predict(model_rf, newdata = testing, type = "prob"))

final$predict <- ifelse(final$Postive > 0.5, "Postive", "Negative")
final$predict <- factor(final$predict,levels = c("Negative","Postive"))

cm_original <- confusionMatrix(final$predict, factor(testing$sentiment))
confusionMatrix(final$predict, factor(testing$sentiment))



#################################
#Under-sampling
#################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
set.seed(42)
model_rf_under <- caret::train(sentiment ~ .,
                               data = training,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

final_under <- data.frame(actual = testing$sentiment,
                          predict(model_rf_under, newdata = testing, type = "prob"))

final_under$predict <- ifelse(final_under$Postive > 0.5, "Postive", "Negative")
final_under$predict <- factor(final_under$predict,levels = c("Negative","Postive"))

cm_under <- confusionMatrix(final_under$predict, testing$sentiment)
confusionMatrix(final_under$predict, testing$sentiment)

################################
#Oversampling
################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(42)
model_rf_over <- caret::train(sentiment ~ .,
                              data = training,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

final_over <- data.frame(actual = testing$sentiment,
                         predict(model_rf_over, newdata = testing, type = "prob"))

final_over$predict <- ifelse(final_over$Postive > 0.5, "Postive", "Negative")
final_over$predict <- factor(final_over$predict,levels = c("Negative","Postive"))

cm_over <- confusionMatrix(final_over$predict, factor(testing$sentiment))
confusionMatrix(final_over$predict, factor(testing$sentiment))



###########################################
#ROSE
###########################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(sentiment ~ .,
                              data = training,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

final_rose <- data.frame(actual = testing$sentiment,
                         predict(model_rf_rose, newdata = testing, type = "prob"))

final_rose$predict <- ifelse(final_rose$Postive > 0.5, "Postive", "Negative")
final_rose$predict <- factor(final_rose$predict,levels = c("Negative","Postive"))

cm_rose <- confusionMatrix(final_rose$predict,factor(testing$sentiment))
confusionMatrix(final_rose$predict,factor(testing$sentiment))


#######################################
#SMOTE
#######################################
train

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(sentiment ~.,
                               data = training,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_smote <- data.frame(actual = testing$sentiment,
                          predict(model_rf_smote, newdata = testing, type = "prob"))

final_smote$predict <- ifelse(final_smote$Pos > 0.5, "Pos", "Neg")
final_smote$predict <- factor(final_smote$predict,levels = c("Neg","Pos"))

cm_smote <- confusionMatrix(final_smote$predict, testing$sentiment)


#######################

models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)

resampling <- resamples(models)
bwplot(resampling)

##############
library(dplyr)
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("cm_", name))
  
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(Sensitivity = model$byClass["Sensitivity"],
           Specificity = model$byClass["Specificity"],
           Precision = model$byClass["Precision"],
           Recall = model$byClass["Recall"],
           F1 = model$byClass["F1"])
}

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)




library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

#drawing least nodes doesn't work since least nodes creates  
#tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))
#tree_func(final_model = model_rf$finalModel, tree_num)


#Draw most nodes 
tree_num <- which(model_rf_rose$finalModel$forest$ndbigtree == max(model_rf_rose$finalModel$forest$ndbigtree))
tree_func(final_model = model_rf_rose$finalModel, tree_num)

#Will need to prune from here





