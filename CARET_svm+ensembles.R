setwd("C:/Users/BenTh/Desktop/ADM_/ADM/Lab1")
# http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/

t <-read.csv("titanic.csv", header = T,na.strings =c(""),stringsAsFactors =T)

#set factors
mice_mod <- mice(t[, !names(t) %in%
                               c('PassengerId','Name','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
t$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
t$Child[t$Age < 18] <- "Yes"
t$Child[t$Age >= 18] <- "No"
t$Child <- factor(t$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
t$Title <- gsub('(.*, )|(\\..*)', '', t$Name)
t$Title[t$Title == 'Mlle'] <- 'Miss'
t$Title[t$Title == 'Ms'] <- 'Miss'
t$Title[t$Title == 'Mme'] <- 'Mrs'
t$Title[t$Title %in% rare_title] <- 'Rare Title'
t$Title <- as.factor(t$Title)



t$Survived <- factor(t$Survived,levels=c(0,1),labels=c("No","Yes"))
t$Age[is.na(t$Age)] <- mean(t$Age, na.rm=TRUE)
t$Cabin <- NULL
t$Name <- NULL
t$Ticket<- NULL
t$PassengerId <-NULL
t$AgeBinned <- cut(t$Age,
          breaks = c(0,10,20,30,40,50,60,70,Inf),
          labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(t$AgeBinned)



t$Pclass <- factor(t$Pclass)
t$SibSp <- factor(t$SibSp)
t$Parch <- factor(t$Parch)
t$Fare <- NULL
t$Age <- NULL


sum(is.na(t))
t <- na.omit(t)


#create partitions
library(caret)
index <- createDataPartition(t$Survived, p = 0.75, list = FALSE)
training <- t[index, ]
testing <- t[-index, ]


#control parameters
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

#linear
svm_Linear <- train(Survived ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(test_pred, testing$Survived )


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



################################################################
#Carret ensemble: https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
library("mlbench")
library("pROC")


my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Survived, 25),
  summaryFunction=twoClassSummary
)

library("rpart")
library("caretEnsemble")
model_list <- caretList(
 Survived~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)

p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

library("mlbench")
library("randomForest")
library("nnet")
model_list_big <- caretList(
  Survived~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)

xyplot(resamples(model_list))
modelCor(resamples(model_list))


greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)

library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"Survived"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)

varImp(greedy_ensemble)


###############################################
#Caret stack
################################################ 

glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, testing$Survived)

CF/sum(CF)

library("gbm")
gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)


model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
colAUC(model_preds3, testing$Survived)






