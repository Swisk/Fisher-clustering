# Fisher-clustering
Exploration on clustering uses the Fisher Iris data set
This uses the K-means approach to attempt to find clusters


#change kclusters to reveal cluster centers for a particular k
#change maxclust to determine the number of k to which maxclust iterates to
kclusters = 3
maxclust = 15

#import your data here in csv format
fisher <- read.csv("Fisher's_Iris.csv", header = T, stringsAsFactors = F)
x <- fisher[,3:6, drop=FALSE]

Allocation <- cbind(kmeans(x, centers=kclusters, 100)$cluster,x)

#functions from here onwards!

#normalizing x
normalize <- function(x) { 
  x <- as.matrix(x)
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  attr(x, 'normalized:min') = minAttr
  attr(x, 'normalized:max') = maxAttr
  return (x)
} 



clus <- function(k, dataframe){
  return(subset(dataframe[dataframe$K == k,],select=c("Petal Width", "Petal Length","Sepal Length","Sepal Width")))
}

clusterdata <- data.frame(stringsAsFactors = FALSE)
clustno <- vector(length = 0)

#normalize x
x <- normalize(x)
#x <- scale(x)

#WSS(within sum of squares) of a cluster = number of rows * variance of each column
wss <- (nrow(x)-1)*sum(apply(x,2,var))

#Loop function to initialize k-clusters from 2 to 15
for (i in 2:maxclust) {
  y <- kmeans(x, centers=i, 100)
  
    wss[i] <- y$tot.withinss
  
  z <- as.data.frame.matrix(y$centers)
  
  for(j in 0:nrow(z)){
    clusterdata <- rbind(clusterdata, unlist(z[j,]))
  }
  
  for(h in 1:i){
    clustno <- c(clustno, i)
  }
}

clusterdata <- cbind2(clustno, clusterdata)

colnames(clusterdata) <- c("K", "Petal Width", "Petal Length","Sepal Length","Sepal Width")

#output commands from here on!

par(mfrow = c(1,1))

#plot output total WSS of clusters
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#output on console for cluster centers
clusterpoints <- clus(kclusters,clusterdata)
clusterpoints

alpha <- kmeans(x, centers=kclusters, 100)
ct.km <- table(fisher$Species, alpha$cluster)
library(flexclust)
randIndex(ct.km)
ct.km


