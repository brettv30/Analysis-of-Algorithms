## K-Means clustering

# First we read in the data file
MyData = read.csv("BehaviorsAndPreferences.csv")
View(MyData)
head(MyData)

# DEFINITION NOTE: 
# We can use the general R-command: 
# Kmeans(x, centers, iter.max = 10, nstart = 1, method = "euclidean")
# x =  Data frame
# centers = Number of clusters
# iter.max = The maximum number of iterations allowed
# nstart   = How many random sets of center should be chosen
# method  = The distance measure to be used

# The capability exists in the package amap which stands for
# Another Multidimensional Analysis Package
# NOTE: Install of amap can take a long time (2-5 min)
install.packages("amap")
library(amap)

# Now we set a seed for random number generator so that we 
# get the same results everytime we run the algorithm
set.seed(456)

# Now we want to grup the 400 people in 4 clusters
# n-start can be anywhere between 25-50
MyClusters = kmeans(MyData, 4, nstart = 30)

# and look at the results
print(MyClusters)

# Now we want to add the cluster identification (1-4) to a new data set:
NewDataSet = cbind(MyData, cluster = MyClusters$cluster)
View(NewDataSet)

# To create new graphs from the clusters we will also 
# install the factoextra package/library
install.packages("factoextra")
library(factoextra)

# fviz_cluster () performs principal component 
# analysis (PCA) and plot data points according 
# to the first 2 principal components that explain 
# the majority of the variance
fviz_cluster(MyClusters, data = MyData)

# Now we can run the models and try 2,3,4,5 different
# clusters and see which worked best
k2 = kmeans(MyData, centers = 2, nstart = 30)
k3 = kmeans(MyData, centers = 3, nstart = 30)
k4 = kmeans(MyData, centers = 4, nstart = 30)
k5 = kmeans(MyData, centers = 5, nstart = 30)

# plots to compare
plot1 = fviz_cluster(k2, geom = "point", data = MyData) + ggtitle("k = 2")
plot2 = fviz_cluster(k3, geom = "point", data = MyData) + ggtitle("k = 3")
plot3 = fviz_cluster(k4, geom = "point", data = MyData) + ggtitle("k = 4")
plot4 = fviz_cluster(k5, geom = "point", data = MyData) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

# we can actually find the optimal number of clusters
# using the fviz_nbclust() command
# run this command before creating a clustering model
set.seed(456)
fviz_nbclust(MyData, kmeans, method = "wss")

### Testing quality of clusters ###
# function to compute average silhouette for k clusters
fviz_nbclust(MyData, kmeans, method = "silhouette")

# GAP Stats for cluster quality and selection
install.packages("cluster")
library(cluster)

# Calculating the gaps for upto 12 clusters with 
# 60 reference sets (B for bootstapping
set.seed(456)
GapStat =  clusGap(MyData, FUN=kmeans,nstart=30, B=60, K.max =12)

# Graphing the data
fviz_gap_stat(GapStat)

# We can now use the information from previous step
# and see if there are any benefits of splitting data 
# in 4 or 12 groups.

k4 = kmeans(MyData, centers = 4, nstart = 30)
k12 = kmeans(MyData, centers = 12, nstart = 30)

plot4 = fviz_cluster(k4, geom = "point", 
                     data = MyData) + ggtitle("k = 4")
plot12 = fviz_cluster(k12, geom = "point", 
                      data = MyData) + ggtitle("k = 12")

grid.arrange(plot4, plot12, nrow = 1)


