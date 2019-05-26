##Density-Based Spatial Clustering of Applications With Noise

install.packages("dbscan")
install.packages("fpc")
install.packages("factoextra")
library(dbscan)
library(fpc)
library(factoextra)

################# https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

head(data)
data("multishapes", packages="factoextra")
df <- multishapes[,1:2]
head(df)
summary(multishapes)
head(multishapes)
set.seed(123)
db <- fpc::dbscan(df, eps=0.15, MinPts=5)
db
fviz_cluster(db, data=df, stand=FALSE, ellipse=FALSE, 
             show.clust.cent = FALSE, geom="point",
             palette="jco", ggtheme = theme_classic())

db$cluster[sample(1:1089, 20)]
#### cluster 0 corresponds to 'outliers'(black points)
#### seed corresponds to 'core points'

###### eps value Optimization (method : k-nearest neighbor distances)
dbscan:: kNNdistplot(df, k=5)
abline(h=0.15, lty=2)


#####https://www.r-exercises.com/2017/06/10/density-based-clustering-exercises/
###https://www.r-exercises.com/2017/06/10/density-based-clustering-exercises-solutions/

#1. Create a new data frame using all but the last variable 
#   from the iris data set, which is supplied with R.
head(iris)
df <- iris[, -ncol(iris)]
head(df)

#2. Use the scale function to normalize values of all 
#   variables in the new data set (with default settings).
#   Ensure that the resulting object is of class data.frame.

df <- scale(df)
head(df)
df <- as.data.frame(df)
head(df)

#3. Plot the distribution of distances between data points and their fifth nearest neighbors using 
#   the kNNdistplot function from the dbscan package.
#   Examine the plot and find a tentative threshold at which distances start increasing quickly. 
#   On the same plot, draw a horizontal line at the level of the threshold.

require(dbscan)
kNNdistplot(df, k=5)
abline(h=0.8, col='red')

#4. Use the dbscan function from the package of the same name to find density-based clusters in the data. 
#   Set the size of the epsilon neighborhood at the level of the found threshold, and set the number of 
#   minimum points in the epsilon region equal to 5.
#   Assign the value returned by the function to an object, and print that object.

require(dbscan)
db_cluster_iris <- dbscan(df, eps=0.8, MinPts=5)
db_cluster_iris

#5. Plot the clusters with the fviz_cluster function from the factoextra package. Choose the geometry type
#   to draw only points on the graph, and assign the ellipse parameter value such that an outline around points 
#   of each cluster is not drawn.
#   (Note that the fviz_cluster function produces a 2-dimensional plot. If the data set contains two variables 
#   those variables are used for plotting, if the number of variables is bigger the first two principal components 
#   are drawn.)

require(factoextra)
fviz_cluster(db_cluster_iris, df, ellipse = FALSE, geom = "point")

#6. Examine the structure of the cluster object obtained in Exercise 4, and find the vector with cluster assignments. 
#   Make a copy of the data set, add the vector of cluster assignments to the data set, and print its first few lines.

df_copy <- df
df_copy[['cluster']] <- db_cluster_iris[['cluster']]
head(df_copy)

#7. Now look at what happens if you change the epsilon value.
#   Plot again the distribution of distances between data points and their fifth nearest neighbors 
#   (with the kNNdistplot function, as in Exercise 3). On that plot, draw horizontal lines at levels 1.8, 
#   0.5, and 0.4.
#   Use the dbscan function to find clusters in the data with the epsilon set at these values (as in Exercise 4).
#   Plot the results (as in the Exercise 5, but now set the ellipse parameter value such that an outline 
#   around points is drawn).

epsilon_values <- c(1.8, 0.5, 0.4)
kNNdistplot(df, k=5)
for (e in epsilon_values){abline(h=e, col="red")}


for (e in epsilon_values) { 
  db_clusters_iris <- dbscan(df, eps=e, MinPts=4)
  title <- paste("Plot for epsilon = ", e)
  g <- fviz_cluster(db_clusters_iris, df, ellipse = TRUE, geom = "point",
                    main = title)
    print(g)
}

#8. This exercise shows how the DBSCAN algorithm can be used as a way to detect outliers:
#   Load the Wholesale customers data set, and delete all variables with the exception of 
#   Fresh and Milk. Assign the data set to the customers variable.
#   Discover clusters using the steps from Exercises 2-5: scale the data, choose an epsilon value, 
#   find clusters, and plot them. Set the number of minimum points to 5. Use the db_clusters_customers 
#   variable to store the output of the dbscan function.

customers <- read.csv("Wholesale.csv")
customers <- customers[, c("Fresh","Milk")]
customers <- scale(customers)
customers <- as.data.frame(customers)
head(customers)
kNNdistplot(customers, k = 5)
abline(h=0.4, col = "red")
db_clusters_customers <- dbscan(customers, eps=0.4, MinPts=5)
print(db_clusters_customers)
fviz_cluster(db_clusters_customers, customers, ellipse = FALSE, geom = "point")

#9. Compare the results obtained in the previous exercise with the results of the k-means algorithm. 
#   First, find clusters using this algorithm:
#   Use the same data set, but get rid of outliers for both variables (here the outliers may be defined 
#   as values beyond 2.5 standard deviations from the mean; note that the values are already expressed 
#   in unit of standard deviation about the mean). Assign the new data set to the customers_core variable.
#   Use kmeans function to obtain an object with cluster assignments. Set the number of centers equal to 4,
#   and the number of initial random sets (the nstart parameter) equal to 10. Assign the obtained object to 
#   the variable km_clusters_customers variable.
#   Plot clusters using the fviz_cluster function (as in the previous exercise).

fivenum(customers$Milk)
fivenum(customers$Fresh)
boxplot(customers$Milk)
boxplot(customers$Fresh)

customers_core <- customers[customers[['Fresh']] > -2.5 &
                              customers[['Fresh']] < 2.5, ]
customers_core <- customers_core[customers_core[['Milk']] > -2.5 &
                                   customers_core[['Milk']] < 2.5, ]
head(customers_core)

km_clusters_customers <- kmeans(customers_core, centers = 4, nstart = 10)
fviz_cluster(km_clusters_customers,
             customers_core,
             ellipse = FALSE,
             geom = "point")


#10. Now compare the results of DBSCAN and k-means using silhouette analysis:
#    Retrieve a vector of cluster assignments from the db_clusters_customers object.
#    Calculate distances between data points in the customers data set using the dist function
#    (with the default parameters).
#    Use the vector and the distances object as inputs into the silhouette function from the cluster 
#    package to get a silhouette information object.
#    Plot that object with the fviz_silhouette function from the factoextra package.
#    Repeat the steps described above for the km_clusters_customers object and the customers_core data sets.
#    Compare two plots and the average silhouette width values.

require(factoextra)
require(cluster)
db_clusters_vector <- db_clusters_customers[['cluster']]
db_distances <- dist(customers)
db_silhouette <- silhouette(db_clusters_vector, db_distances)
fviz_silhouette(db_silhouette)

km_clusters_vector <- km_clusters_customers[['cluster']]
km_distances <- dist(customers_core)
km_silhouette <- silhouette(km_clusters_vector, km_distances)
fviz_silhouette(km_silhouette)
### the closer to 1, the better Clustering