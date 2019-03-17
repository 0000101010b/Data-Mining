library(ggfortify) #for plots
df <- iris[c(1:4)] # removes the class label: feature 5
autoplot(prcomp(df, scale. = T, center = T), data = iris, colour = 'Species')

screeplot(prcomp(df, scale. = T, center = T), type="lines")

library(factoextra)
library(FactoMineR)

pca <- PCA(iris[, -5], scale.unit = T, ncp=3)

fviz_screeplot(pca, addlabels = TRUE)
fviz_pca_ind(pca, habillage = iris$Species, label="none", addEllipses = T)


table(iris$Species)


set.seed(1337)
myIris <- iris[, c(1:4)] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=T)})
res.1 <- kmeans(myIris, 3)

str(res.1)
df <- data.frame(cluster = res.1$cluster, species = iris$Species)
table (factor(df$cluster), df$species)


myIris <- iris[, c(1:4)] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=F)})
res.2 <- kmeans(myIris, 3)
df <- data.frame(cluster = res.2$cluster, species = iris$Species)
table (factor(df$cluster), df$species)


pca <- prcomp(iris[, c(1:4)], scale. = T, center = T)
res.3 <- kmeans(pca$x[, 1:2], 3)
df <- data.frame(cluster = res.3$cluster, species = iris$Species)
table (factor(df$cluster), df$species)

pca <- prcomp(iris[, c(1:4)], scale. = T, center = F)
res.4 <- kmeans(pca$x[, 1:2], 3)
df <- data.frame(cluster = res.4$cluster, species = iris$Species)
table (factor(df$cluster), df$species)

totss <- c(res.1[[3]], res.2[[3]], res.3[[3]], res.4[[3]])
tos.withinss <- c(res.1[[5]], res.2[[5]], res.3[[5]], res.4[[5]])
betweenss <- c(res.1[[6]], res.2[[6]], res.3[[6]], res.4[[6]])
performance <- data.frame(totss, tos.withinss, betweenss)
row.names(performance) <- c("w/o pca +c", "w/o pca", "w pca +c", "w pca")
performance

plot(iris[,c(1:4)], col=res.1$cluster)


library(clusterSim) #for the dbindex


library(cluster) #for the silhouette
results <- list()
tot.withinss <- c()
betweenss <- c()
dbindex <- c()
silhouettes <- c()
for (k in 2:10) {
  results[[k]] <- kmeans(myIris, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  dbindex[k] <- index.DB(myIris, results[[k]]$cluster, centrotypes="centroids")$DB
  s <- silhouette(results[[k]]$cluster, daisy(myIris))
  silhouettes[k] <- mean(s[,3])
}

par(mfrow=c(2,2))
plot(tot.withinss, xlab="k")
plot(betweenss, xlab="k")
plot(dbindex, xlab="k")
plot(silhouettes, xlab="k")


plot(silhouette(results[[3]]$cluster, daisy(myIris)))

library(fpc)
plotcluster(myIris, res.2$cluster)


res.2$centers #coords of centroids

centers <- res.2$centers[res.2$cluster, ] #vector of all centers for each point
distances <- sqrt(rowSums((myIris - centers)^2)) #Euclidean pair-wise distances
summary(distances)

sd(distances)



# pick n largest distances: boring!
outliers <- order(distances, decreasing=T)[1:5]
# pick those more than 2 standard deviations from the mean
outliers <- (distances > (mean(distances) + (2 * sd(distances))) |
               distances < (mean(distances) - (2 * sd(distances))))

# display the outliers
print(myIris[outliers,])


# plot clusters (choose 2 dimensions for ease of display)
plot(myIris[,c("Sepal.Length", "Sepal.Width")], pch="o", col=res.2$cluster, cex=0.3)
# plot cluster centers
points(res.2$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
# plot outliers
points(myIris[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)

         


d <- dist(myIris, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red")
            
            
ds <- dbscan(myIris, eps=.25, MinPts=5)


table(ds$cluster, iris$Species)


plot(ds, myIris)

plotcluster(myIris, ds$cluster)


results <- list()
lowerBound <- 2
upperBound <- 10


d <- dist(myIris)

for (i in 2:10) {
  ds <- dbscan(myIris, eps=i/100, MinPts = 5)
  results[[i]] <- cluster.stats(d, ds$cluster)
  results[[i]]$eps <- i/100

}


#prune the NULLS
results[sapply(results, is.null)] <- NULL
#pick what you want to plot, e.g. average silhouette width
avg.silwidth <- lapply(results, FUN = function(x) {
  return (x$avg.silwidth)
})


eps <- lapply(results, FUN = function(x) {
  return (x$eps)
})
plot(y=avg.silwidth, x=eps, xlab="eps")



library(dbscan)
db <- dbscan(myIris, 0.4, 5)
hullplot(myIris, db$cluster)








            
            
                                           





