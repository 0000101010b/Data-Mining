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


# -Add binning of temperatures




setwd("C:/Users/BenTh/Desktop/WR")
wr<-read.csv("rev_w_sent.csv", header = T,na.strings =c(""),stringsAsFactors =T)
set.seed(1337)


wr$temp

wr$stars <- factor(wr$stars)

library(dplyr)

#######################################
#uncomment to test on smaller set of data
#######################################
wr <- wr %>% filter(usaf!="999999") 

library(caret)

index <- createDataPartition(wr$sentiment, p = 0.75, list = FALSE)
training <- wr[index, ]
testing <- wr[-index, ]


#################################
#Mode of Original unbalanced data
#################################
set.seed(42)

ctrl <- trainControl(method = "repeatedcv", 
                    number = 10, 
                    repeats = 10, 
                    verboseIter = FALSE)


model_rf <- caret::train(sentiment ~ thunder + fog + hail + rain_drizzle + snow_ice_pellets,
                         data = training,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)


final <- data.frame(actual = testing$sentiment,
                          predict(model_rf, newdata = testing, type = "prob"))
final$predict <- ifelse(final$Postive > 0.5, "Postive", "Negative")
final$predict <- factor(final$predict,levels = c("Negative","Postive"))

cm_original <- confusionMatrix(final$predict, testing$sentiment)

#################################
#Under-sampling
#################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")
set.seed(42)
model_rf_under <- caret::train(sentiment ~ thunder + fog + hail + rain_drizzle + snow_ice_pellets,
                               data = training,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

final_under <- data.frame(actual = testing$sentiment,
                    predict(model_rf_under, newdata = testing, type = "prob"))

final_under$predict <- ifelse(final_under$Postive > 0.5, "Postive", "Negative")
final_under$predict <- factor(final_under$predict,levels = c("Negative","Postive"))

cm_under <- confusionMatrix(final_under$predict, testing$sentiment)

################################
#Oversampling
################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(42)
model_rf_over <- caret::train(sentiment ~ thunder + fog + hail + rain_drizzle + snow_ice_pellets,
                              data = training,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

final_over <- data.frame(actual = testing$sentiment,
                          predict(model_rf_over, newdata = testing, type = "prob"))

final_over$predict <- ifelse(final_over$Postive > 0.5, "Postive", "Negative")
final_over$predict <- factor(final_over$predict,levels = c("Negative","Postive"))

cm_over <- confusionMatrix(final_over$predict, testing$sentiment)


###########################################
#ROSE
###########################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(sentiment ~ thunder + fog + hail + rain_drizzle + snow_ice_pellets,
                              data = training,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

final_rose <- data.frame(actual = testing$sentiment,
                         predict(model_rf_rose, newdata = testing, type = "prob"))

final_rose$predict <- ifelse(final_rose$Postive > 0.5, "Postive", "Negative")
final_rose$predict <- factor(final_rose$predict,levels = c("Negative","Postive"))

cm_rose <- confusionMatrix(final_rose$predict, testing$sentiment)

#######################################
#SMOTE
#######################################
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(sentiment ~ thunder + fog + hail + rain_drizzle + snow_ice_pellets,
                               data = training,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_smote <- data.frame(actual = testing$sentiment,
                         predict(model_rf_smote, newdata = testing, type = "prob"))

final_smote$predict <- ifelse(final_smote$Postive > 0.5, "Postive", "Negative")
final_smote$predict <- factor(final_smote$predict,levels = c("Negative","Postive"))

cm_smote <- confusionMatrix(final_smote$predict, testing$sentiment)


#######################

models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               #smote = model_rf_smote,
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


















