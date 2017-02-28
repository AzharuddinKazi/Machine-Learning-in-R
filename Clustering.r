rm(list=ls(all=TRUE))

#Consider mtacrs data of R-datasets
data(mtcars)
mydata <- data.frame(mtcars)
mydata <- na.omit(mydata) # listwise deletion of missing
summary(mydata)
str(mydata)

mydata <- scale(mydata) # standardize variables 
summary(mydata)

###-------------------------    Hierarchical Clustering     ------------------------###

# Ward's method 
d <- dist(mydata, 
          method = "euclidean") # distance matrix
d
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 5 clusters
groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=6, border="red") 

mtcars$cluster <- groups

###-------------------------    K- means Clustering     ------------------------###

# K-Means Cluster Analysis with k = 5
fit <- kmeans(mydata, 5) # 5 cluster solution

#study the model and metrics

#With-in sum of squares in each cluster 
fit$withinss 
sum(fit$withinss) 
fit$tot.withinss

#To check cluster number of each row in data
fit$cluster

#Cluster Centers 
fit$centers 

# get cluster means
aggregate(mydata,by=list(fit$cluster),
          FUN=mean)

# append cluster label to the actual data frame
mydata <- data.frame(mydata,fit$cluster) 
head(mydata)

# Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}


#Scree Plot
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#Now we can perform any classification algorithm on the data to classify a future data point

# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- mtcars[sample(1:nrow(mtcars),1),]

closest.cluster <- function(x) {
        cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
        print(cluster.dist)
        return(which.min(cluster.dist)[1])
}

closest.cluster(test_datapoint)