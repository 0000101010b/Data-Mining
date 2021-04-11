# rain=re %>% filter(re$rain_drizzle == 1)
# notRain=re %>% filter(re$rain_drizzle == 0)
# 
# p1 <- hist(mean(rain$stars))                     # centered at 4
# p2 <- hist(rnorm(notRain$stars))          # centered at 6
# plot( p1, col=rgb(0,0,1,0.1),ylim=c(0,10))   # first histogram
# plot( p2, col=rgb(1,0,0,0.1),ylim=c(0,10) ,add = T)

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source",
                 repos="http://h2o-release.s3.amazonaws.com/h2o/rel-xu/6/R")


setwd("C:/Users/BenTh/Desktop")
mnist <- read.csv("MNISTTrain.csv", header=T)
mnist$label <- factor(mnist$label)

columnsKeep <- names(which(colSums(mnist[, -1]) > 0))
mnist <- mnist[c("label",columnsKeep)]

t1 <- Sys.time()
pca <- prcomp(mnist[,-1],scale. = F ,center = F)
print(diff.difftime(Sys.time(),t1,units ='sec'))




h2o.mnist <- h2o.importFile(path=paste0(getwd(), "/MNISTTrain.csv"), header=T)
h2o.mnist <- h2o.mnist[c("label", columnsKeep)]
t2 <- Sys.time()
pca.h2o <- h2o.prcomp(h2o.mnist[, -1], k=100)
print( difftime( Sys.time(), t2, units = 'sec'))



library(caret)

set.seed(2018)
#stratified sampling
index <- createDataPartition(mnist$label, p = .25, list = FALSE)
h2o.training <- h2o.mnist[index, -1]
h2o.testing <- h2o.mnist[-index, -1]
pca.h2o.training <- h2o.prcomp(h2o.training, x=names(h2o.training),
                               k=100, ignore_const_cols = FALSE)

pca.train <- as.matrix(mnist[index,-1]) %*%
  as.matrix(pca.h2o.training@model$eigenvectors[, 1:35])


pca.test <- as.matrix(mnist[-index,-1]) %*%
  as.matrix(pca.h2o.training@model$eigenvectors[, 1:35])
h2o.training.pca <- as.h2o(pca.train)
h2o.testing.pca <- as.h2o(pca.test)
mnist$label <- factor(mnist$label, levels = c(0:9),
                      labels = c("zero", "one", "two", "three", "four",
                                 "five", "six", "seven", "eight", "nine"))
h2o.training.pca <- h2o.cbind(h2o.training.pca, as.h2o(factor(mnist[index,1])))
h2o.testing.pca <- h2o.cbind(h2o.testing.pca, as.h2o(factor(mnist[-index,1])))
names(h2o.training.pca)
#rename the dependent (it's current called x)
names(h2o.training.pca)[36] <- "label"
names(h2o.testing.pca)[36] <- "label"



t3 <- Sys.time()
res.dl <- h2o.deeplearning(x = 1:35,
                           y = "label",
                           training_frame = h2o.training.pca,
                           activation = "Tanh",
                           hidden=rep(160,3),
                           epochs = 20)
print( difftime( Sys.time(), t3, units = 'sec'))



(pref<-h2o.performance(model=res.dl, newdata=h2o.testing.pca, xval = TRUE))

pred.dl<-h2o.predict(object=res.dl, newdata=h2o.testing.pca[,-36])
summary(pred.dl)

predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(mnist$label),
                    labels = levels(mnist$label))
confusionMatrix(predicted, mnist[-index, 1])

validation <- createDataPartition(mnist[-index,1], p = 1/3, list = FALSE)
h2o.validation <- h2o.testing.pca[validation, ]


hidden_opt <- list(rep(150,3), c(200, 150, 75, 50), 100)
l1_opt <- c(1e-5,1e-7)
activation <- c("Tanh", "RectifierWithDropout", "Maxout")
hyper_params <- list(hidden = hidden_opt,
                     l1 = l1_opt, activation = activation)
h2o.grid("deeplearning",
         hyper_params = hyper_params,
         x = 1:35,
         y = "label",
         grid_id = "ADM_ann_grid",
         training_frame = h2o.training.pca,
         validation_frame = h2o.validation)


grid <- h2o.getGrid(grid_id = "ADM_ann_grid",
                    sort_by = "accuracy",
                    decreasing = TRUE)


best <- h2o.getModel(grid@model_ids[[1]])
print(best)


h2o.performance(best, newdata=h2o.testing.pca[-validation, ], xval=TRUE)


pred.dl<-h2o.predict(object=best, newdata=h2o.testing.pca[-validation,-36])


predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(mnist$label),
                    labels = levels(mnist$label))
testingLabels <- mnist[-index, 1]
testingLabels <- testingLabels[-validation]
confusionMatrix(predicted, testingLabels)
                

##AutoML

automl <- h2o.automl(x = 1:35, y = "label",
                     training_frame = h2o.training.pca,
                     validation_frame = h2o.validation,
                     project_name = "MNIST_AutoML",
                     max_runtime_secs = 180)



pred.dl<-h2o.predict(object=automl@leader, newdata=h2o.testing.pca[-validation,-36])


predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(mnist$label),
                    labels = levels(mnist$label))
testingLabels <- mnist[-index, 1]
testingLabels <- testingLabels[-validation]
confusionMatrix(predicted, testingLabels)








