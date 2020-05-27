# First we are going to read in the data file
MyData = read.csv("K_Means_Study.csv")

# Now we are going to get a feel for the data set
# Here we view the entire data set and view the top six rows of every variable
View(MyData)
head(MyData)

# here we include the factoextra library
library(factoextra)

# here we set the seed for the random number generator so that we
# get the same results each time we run the algorithm
set.seed(420)

# now we use the wss method to determine the optimal number of clusters
# we know which one is the optimal amount by determining when the elbow 
# flattens out
fviz_nbclust(MyData, kmeans, method = "wss")

# here we include the amap library 
library(amap)

# create four clusters of data then print them out to the console
# use an nstart value of 50 in order to have a more stable result
MyClusters4 = kmeans(MyData, 4, nstart = 50)
print(MyClusters4) 

# Here we are going to print a graph of the four clusters. 
# Something weird is going on, the clusters are greatly overlapping one another.
# This means that we need more clusters to better interpret the data.
fviz_cluster(MyClusters4, data = MyData)

# create a new column in the data set titled cluster 
# this new column will show which cluster every data point is in
NewDataSet = cbind(MyData, cluster = MyClusters4$cluster)

# view the new data set with the cluster column
View(NewDataSet)

# rename the clusters to meaningful names based off of 
# what the data points seem to favor. 
NewDataSet$cluster = factor(NewDataSet$cluster,
                            levels = c(1, 2, 3, 4), labels = c('senior citizens', 
                                                               'middle aged parents w older children',
                                                               'adults w kids',
                                                               'young adults 21+'))

# lets use the silhouette method to better understand how to cluster the data
# this here is telling us that the optimal number to cluster the data is 
# actually only two clusters
fviz_nbclust(MyData, kmeans, method = "silhouette")

# implement the cluster library
library(cluster)

# set the seed to 420 and the Bootstrapping sample to 60.
# set the maximum number of clusters to 12, there should never be more than 12
set.seed(420)
GapStat = clusGap(MyData, FUN=kmeans, nstart=50, B=60, K.max = 12)

# print a graph  of the GapStats to see where the largest gaps lie 
# and to find the optimal number of clusters
# this graph claims that the optimal number of clusters is 3
fviz_gap_stat(GapStat)


# Now we begin to create models with varying clusters in order to find 
# which cluster amount is arguably the best.....


# create new objects containing cluster models ranging from 2 centers, 3 centers, 
# 5 centers, 6 centers
MyClusters2 = kmeans(MyData, centers = 2, nstart = 50)
MyClusters3 = kmeans(MyData, centers = 3, nstart = 50)
MyClusters5 = kmeans(MyData, centers = 5, nstart = 50)
MyClusters6 = kmeans(MyData, centers = 6, nstart = 50)

# create four graphs based on the cluster models we just created and title
# them with the number of clusters
plot1 = fviz_cluster(MyClusters2, geom="point", data = MyData) + ggtitle("k = 2")
plot2 = fviz_cluster(MyClusters3, geom="point", data = MyData) + ggtitle("k = 3")
plot3 = fviz_cluster(MyClusters5, geom="point", data = MyData) + ggtitle("k = 5")
plot4 = fviz_cluster(MyClusters6, geom="point", data = MyData) + ggtitle("k = 6")

# invoke the gridExtra library to effectively display this data
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, nrow=2)

# After looking at the graphs, we can determine that the best 
# distribution among the clusters either lies within three or four because
# of the seemingly equal distributions among the clusters. 
# Everything else becomes too convoluted and messy. 

