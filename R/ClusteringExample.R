# Clustering Example  

# remove all data in memory
rm(list=ls())

# datapaths
main_path <- "C:/Users/arne/DS_Programming_Courses/Coursera/ExploratoryDataAnalysis/week3"

# functions
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  
  ## modifiction of plclust for plotting hclust objects *in colour*!
  
  ## Copyright Eva KF Chan 2009
  
  ## Arguments:
  
  ##    hclust:    hclust object
  
  ##    lab:        a character vector of labels of the leaves of the tree
  
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  
  ##    hang:     as in hclust & plclust
  
  ## Side effect:
  
  ##    A display of hierarchical cluster with coloured leaf labels.
  
  y <- rep(hclust$height,2)
  
  x <- as.numeric(hclust$merge)
  
  y <- y[which(x<0)]
  
  x <- x[which(x<0)]
  
  x <- abs(x)
  
  y <- y[order(x)]
  
  x <- x[order(x)]
  
  plot( hclust, labels=FALSE, hang=hang, ... )
  
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), 
        labels=lab[hclust$order], 
        col=lab.col[hclust$order], 
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}

# loading the data
load(paste(main_path, "/data/samsungData.rda", sep = ""))

# exploring data
names(samsungData)[1:12]
table(samsungData$activity) # number of observation per activity

# plotting avg. acceleration (first 2 variables), grouped (colour coded) by activity for the first subject
par(mfrow=c(1,2))
numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData)[2]) # not much of a pattern except for higher variation in laying group
legend("bottomright",legend=unique(samsungData$activity),col=unique(numericActivity),pch=19, cex = 0.75)

# clustering based on average acceleration (columns 1 to 3)
par(mfrow=c(1,1))
distanceMatrix <- dist(samsungData[samsungData$subject==1,1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity) # not much of a pattern visible

# plotting max acceleration (var 10 &11)
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData)[10]) 
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData)[11]) # clear distinction btw. active (walking) and non-active(standing, sitting, laying) groups

# clustering based on max acceleration (columns 10 to 12)
par(mfrow=c(1,1))
distanceMatrix <- dist(samsungData[samsungData$subject==1,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
# conclusion:
# 1. clear distinction btw. walking and non-walking visible again in clusters
# 2. walking activities are clearly separated: walkdown, walkup, walk form their own cluster
# 3. non-walking activities are in one cluster

# SVD analysis
svd1 = svd(scale(samsungData[samsungData$subject==1,-c(562,563)]))
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numericActivity,pch=19) # first column of left singular vector (observations) -> clear pattern
plot(svd1$u[,2],col=numericActivity,pch=19) # second column of left singular vector (observations) -> more noisy, but: walkup is in a distinct group

# finding and plotting maximum contributor
par(mfrow=c(1,1))
plot(svd1$v[,2],pch=19)
maxContrib <- which.max(svd1$v[,2]) # column 296 is the maximum contributor (v=right singular vector assiciated with columns)
distanceMatrix <- dist(samsungData[samsungData$subject==1,c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
names(samsungData)[maxContrib] # checking which variable is the maximum contributor

# K-means clustering
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6) # only one random start
table(kClust$cluster,samsungData$activity[samsungData$subject==1]) # pattern of assignment of activities to clusters a bit noisy
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=1) # only one random start, second try
table(kClust$cluster,samsungData$activity[samsungData$subject==1]) # second try: different clusters, pattern still noisy
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=100) # now 100 random starts
table(kClust$cluster,samsungData$activity[samsungData$subject==1]) # clearer pattern regarding assignment of activities to clusters
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=100) # now 100 random starts, second try
table(kClust$cluster,samsungData$activity[samsungData$subject==1]) # second try: same clusters

plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="") # coordinates of first cluster for variables 1-10

plot(kClust$center[6,1:10],pch=19,ylab="Cluster Center",xlab="") # coordinates of sixth cluster for variables 1-10
