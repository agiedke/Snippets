# Dimension Reduction

# libraries
library(Matrix)
#source("https://bioconductor.org/biocLite.R")
#biocLite("impute")
library(impute)
# creating matrix with random values
set.seed(12345); par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)

# visualizing data
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1]) # creating heatmap (color code: low value -> red, middle -> yellow, high -> white)
heatmap(dataMatrix) # heatmap on sorted matrix(clustered using hierarchical clustering)

# adding pattern to the matrix
set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}

# visualizing data
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

# visualizing clustered (hierarchical) data
hh <- hclust(dist(dataMatrix)) # creating cluster object
dataMatrixOrdered <- dataMatrix[hh$order,] # ordering dataMatrix's rows by cluster
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1]) # visualizing transposed sorted matrix
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)

# SVD illustration
mat <- matrix(
  c(1,2,2,5,3,7),
  nrow=2,
  ncol=3
)
mat # original matrix
svd_mat <- svd(mat) # singular value decomposition function
svd_mat # d = diagonal matrix = singular values of X, u = left singular vectors, v = right singular vectors, u & v are orthogonal; X = UDV^t
diag_mat <- Diagonal(2,svd_mat$d)
svd_mat$u %*% diag_mat %*% t(svd_mat$v) # the product of the three components is the original matrix
# plotting u and v
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19) # LEFT singular vector (u matrix) is associated with the ROW means of the clustered data; clear separation is visible between top 24 and bottom 16 row means; other columns of U dont show these patterns so clearly!
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19) # RIGHT singular vector (v matrix) is associated with the COLUMN means of the clustered data; clear separation is visible between the left and the right 5 column means; other columns of v dont show this pattern so clearly!
svd1$d # these value explain why other columns of U/V dont show the outlined patterns so clearly; the d matrix is an aspect of SVD called variance explained; entries of D are like weights for the U and V columns accounting for the variation in the data = values are decreasing -> weights become smaller
# plotting d
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19) # first entry considerably higher than the rest
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19) # first entry accounts for 40% of variance in the data 
# another extreme example:
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
head(constantMatrix) # very clear pattern
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1]) # very clear pattern visible
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19) # the values of all but the first entry are 0 
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19) # first entry accounts for 100% of the variance
# intuition: this means that the data is one-dimensional. Only 1 piece of information, namely ...
# ...which column an entry is in, determines its value.
# adding 2nd pattern
set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip1 <- rbinom(1,size=1,prob=0.5)
  coinFlip2 <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip1){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),each=5)
  }
  if(coinFlip2){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),5)
  } # 2 coinflips: 1st: add 5 to each entry in the right 5 columns, 2nd: add five to just the even columns
} 
hh <- hclust(dist(dataMatrix)); dataMatrixOrdered <- dataMatrix[hh$order,]
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1]) # matrix ordered by cluster
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 1") # visualization of pattern 1
plot(rep(c(0,1),5),pch=19,xlab="Column",ylab="Pattern 2") # visualization of pattern 2
# plotting diagonal matrix
dev.off()
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
# intuition:
# the first element which showed the difference between the left and right halves of the matrix accounts
# for roughly 50% of the variation in the matrix, and the second element which picked up the alternating
# pattern accounts for 18% of the variance. The remaining elements account for smaller percentages of the
# variation. This indicates that the first pattern is much stronger than the second. Also the two patterns
# confound each other so they're harder to separate and see clearly. This is what often happens with real
# data.

# PCA & SVD
svd(scale(mat)) # singular value decomposition of scaled ((xi-mean(x))/sd(x)) matrix
prcomp(scale(mat)) # principal component analysis of a scaled matrix yields V matrix of that scaled matrix
# plotting pca vs svd
dev.off()
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered,scale=TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1") # rotation matrix from pca = right singular vector from svd -> here values of first rows are plottet for each
abline(c(0,1))

# Data imputation 
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix2)) # error as svd and pca cannot deal with missing data -> imputation necessary
dataMatrix2 <- impute.knn(dataMatrix2)$data # using knn for imputing
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

# Face example
dev.off()
load(file = "C:/Users/arne/DS_Programming_Courses/Coursera/ExploratoryDataAnalysis/week3/data/face.rda")
image(t(faceData)[,nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singluar vector",ylab="Variance explained")
# %*% is matrix multiplication
# Here svd1$d[1] is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5])%*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])%*% t(svd1$v[,1:10])
par(mfrow=c(1,4))
image(t(faceData)[,nrow(faceData):1])
image(t(approx10)[,nrow(approx10):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx1)[,nrow(approx1):1])
# Closing comments:
# First, when reducing dimensions you have to pay attention to the scales... 
# ...on which different variables are measured and make sure that all your...
# ...data is in consistent units. In other words, scales of your data matter. 
# Second, principal components and singular values may mix real...
# ...patterns, as we saw in our simple 2-pattern example, so finding and separating out...
# ...the real patterns require some detective work.