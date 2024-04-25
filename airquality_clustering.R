
#loading the dataset
data("airquality")
str(airquality)

#calling the libraries
library(cluster)
library(ClusterR)

set.seed(240)

kmeans_result<-kmeans(airquality[,c("Wind","Temp")],centers = 2,nstart = 20) 
kmeans_result


#for clustering 
kmeans_result$cluster


#plotting the cluster for k = 2
plot(airquality[,c("Wind","Temp")],col= kmeans_result$cluster,
     main="kmeans_clustering with k=2",xlab="WIND",ylab="TEMP")   

#for the centers of the clusters

kmeans_result$centers
kmeans_result$centers[,c("Wind","Temp")]

# using pch for plotting the symbol of the clusters and cex is font size

points(airquality[,c("Wind","Temp")],col=kmeans_result$centers,pch=3,cex=3)


# for data visual representation
clusplot(airquality[,c("Wind","Temp")],kmeans_result$cluster,
                    lines = 0,
                    main=paste("cluster airquality"),
                    shade=TRUE,
                    color=TRUE,
                    labels=2,
                    plotchar=FALSE,
                    span=FALSE,
                    xlab="WIND",
                    ylab="TEMP")



# for the elbow plot
# Calculate total within-cluster sum of squares (WCSS) for different values of K
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(airquality[, c("Wind", "Temp")], centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, main = "ELBOW PLOT",xlab = "Number of Clusters (K)", ylab = "Within-cluster Sum of Squares (WCSS)")
lines(1:10, wcss,type = "b", pch = 19, col = "blue")
abline(v = which(diff(wcss) == max(diff(wcss))) + 1, col = "red", lty = 2)
 


