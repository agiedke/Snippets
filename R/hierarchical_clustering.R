
# creating example data
x <- runif(10)
y <- runif(10)
z <- runif(10)
df <- data.frame(x, y, z)

# hierarchical clustering
distxyz <- dist(df) # computing distance between each point
hc <- hclust(distxyz) # create hierarchical cluster object
plot(hc) # plotting the cluster
plot(as.dendrogram(hc))

# heatmaps
heatmap(as.matrix(df)) # create heatmap, groping points together
heatmap(as.matrix(df), Rowv = as.dendrogram(hc)) # taking into account hierarchical clustering
