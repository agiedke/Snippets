# K means
# creating example data
x <- runif(10)
x[1:3] <- x[1:3] + 1.5
x[4:7] <- x[4:7] + 5
x[8:10] <- x[8:10] + 3

y <- runif(10)
y[1:3] <- y[1:3] + 1.5
y[4:7] <- y[4:7] + 5
y[8:10] <- y[8:10]
# plotting raw data 
plot(x,y) # 3 clusters visible
df <- data.frame(x, y)

# plotting data color coded by cluster 
plot(x,y,col=kmeans(df,centers=3, nstart=10)$cluster, pch=19,cex=2) # 3 cluster, 10 random starts specified
plot(x,y,col=kmeans(df,centers=3, nstart=1)$cluster, pch=19,cex=2) # 3 cluster, 1 random start specified
plot(x,y,col=kmeans(df,centers=3, nstart=1)$cluster, pch=19,cex=2) # ran again -> clusters may be different
plot(x,y,col=kmeans(df,centers=6, nstart=1)$cluster, pch=19,cex=2) # too many cluster specified
plot(x,y,col=kmeans(df,centers=6, nstart=1)$cluster, pch=19,cex=2) # ran again -> clusters may be different
