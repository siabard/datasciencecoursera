
##### WEEK 3

set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each=4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x, y, col="blue", pch=19, cex=2)
text(x + 0.05, y + 0.01, labels = as.character(1:12))

### dist & hclust (Hierachical Clustering)
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)


## Pretty dendrograms
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), hang = 0.1, ...) {
  
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order], 
       col = lab.col[hclust$order], srt=90, adj=c(1, 0.5), xpd = NA, ...)
}
myplclust(hClustering, lab=rep(1:3, each=4), lab.col=rep(1:3, each=4))

### heatmap
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)


### k-means

set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each=4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x, y, col="blue", pch=19, cex=2)
text(x + 0.05, y + 0.01, labels = as.character(1:12))

dataFrame <- data.frame(x, y)
kmeanObj <- kmeans(dataFrame, centers = 3)
names(kmeanObj)

kmeanObj$cluster

par(mar = rep(0.2, 4))
plot(x, y, col=kmeanObj$cluster, pch=19, cex=2)
points(kmeanObj$centers, col=1:3, pch=3, cex=3, lwd=3)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeanObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[,order(kmeanObj$cluster)], yaxt = "n")


### Dimension Reduction
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

heatmap(dataMatrix)

### Add pattern
set.seed(678910)
for ( i in 1:40 ) {
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  if(coinFlip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each = 5)
  }
}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)


### Components of the SVD - mu and vu
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2 / sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)


svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[,1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0, 1))

### Components of the SVD - variance
constantMatrix <- dataMatrixOrdered * 0
for(i in 1:dim(dataMatrixOrdered)[1]) {
  constantMatrix[i,] <- rep(c(0,1), each = 5)
  
}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)


### Add a second pattern
set.seed(678910)
for ( i in 1:40 ) {
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  
  if(coinFlip1) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5), each = 5)
  }
  
  if(coinFlip2) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5), 5)
  }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")


### d
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/ sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", pch = 19)

### Missing Values
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))


## Imputing 
source("https://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)

dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)


### Plotting and Color in R
pal <- colorRamp( c("red", "blue"))
pal(0); pal(1); pal(0.5)

pal(seq (0, 1, len = 10))


pal <- colorRampPalette(c("red", "yellow"))

### RColorBrewer

library(RColorBrewer)
cols <- brewer.pal(5, "BuGn")
pal <- colorRampPalette(cols)
image(volcano, col=pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

#rgb(r, g, b, alpha)
#colorspace package

### Swirl
## Hierarchical Clustering
# define close - distance
dist(dataFrame)
hc <- hclust(distxy)
plot(as.dendrogram(hc))
plot(as.dendrogram(hc))
abline(h=1.5, col="blue")
abline(h=0.4, col="red")
abline(h=0.05, col="red")

dist(dFsm)

# heatmap
heatmap(dataMatrix, col=cm.colors(25))

heatmap(mt)
plot(denmt)

## k means clustering
points(cx, cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd=2)
mdist(x, y, cx, cy)
apply(distTmp, 2, which.min)
points(x, y, pch = 19, cex = 2, col = cols1[newClust])
tapply(x, newClust, mean)
tapply(y, newClust, mean)
points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2)
mdist(x, y, newCx, newCy)
apply(distTmp2, 2, which.min)
points(x, y, pch=19, cex = 2, col = cols1[newClust2])
tapply(x, newClust2, mean)
tapply(y, newClust2, mean)
points(finalCx, finalCy, col=cols1, pch = 9, cex= 2, lwd=2)

kmeans(dataFrame, centers = 3)
plot(x, y, col = kmObj$cluster, pch = 19, cex = 2)
points(kmObj$centers, col=c("black", "red", "green"), pch=3, cex=3, lwd=3)
plot(x, y, col=kmeans(dataFrame, 6)$cluster, pch=19, cex=2)

### Dimension Reduction
heatmap(dataMatrix)

set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}


heatmap(dataMatrix)

svd(mat)
matu %*% diag %*% t(matv)


svd(scale(mat))
prcomp(scale(mat))

svd1$v[,1]
svd1$d


head(constantMatrix)

svd2$v[,1:2]
svd2$d

dim(faceData)
a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1]) 
a1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1] 
myImage(a1)
a2 <- svd1$u[,1:2] %*%   diag(svd1$d[1:2])  %*% t(svd1$v[,1:2])
myImage(a2)

myImage( svd1$u[,1:5] %*%   diag(svd1$d[1:5])  %*% t(svd1$v[,1:5]) )
myImage( svd1$u[,1:10] %*%   diag(svd1$d[1:10])  %*% t(svd1$v[,1:10]) )

### Clustering Example
dim(ssd)
names( ssd[,562:563])
table(ssd$subject)
sum(table(ssd$subject))
table(ssd$activity)

sub1 <- subset(ssd, subject == 1)
names(sub1[, 1:12])


par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)
par(mfrow=c(1,1))


showMe(c(1:6))

mdist <- dist(sub1[,1:3])
hclustering <- hclust(mdist)
myplclust( hclustering, lab.col = unclass(sub1$activity))
mdist <- dist( sub1[,10:12])
hclustering <- hclust(mdist)
myplclust( hclustering, lab.col = unclass(sub1$activity))

svd1 <- svd(scale(sub1[,-c(562,563)]))
dim(svd1$u)
maxCon <- which.max( svd1$v[,2])
mdist <- dist(sub1[, c(10:12, maxCon)])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))
names(sub1[maxCon])

kClust <- kmeans( sub1[, -c(562, 563)], 6)
table(kClust$cluster, sub1$activity)

kClust <- kmeans( sub1[, -c(562, 563)], 6, nstart=100)
table(kClust$cluster, sub1$activity)
dim(kClust$centers)
laying <- which(kClust$size == 29)
plot(kClust$centers[laying, 1:12], pch=19, ylab="Laying Cluster")

names(sub1[, 1:3])
walkdown <- which(kClust$size == 49)
plot(kClust$centers[walkdown, 1:12], pch = 19, ylab = "Walkdown Cluster")