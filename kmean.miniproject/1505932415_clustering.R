# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster) 
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

# Before removing Type column, store dataframe with Type column in another variable (will need later)
wine.types <- wine

# Remove first column
wine <- wine[,2:ncol(wine)]

# Scale wine (units of columns are number of standard deviations for the mean)
wine <- scale(wine)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
	              print(wss)
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)
	                }
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine)

# Exercise 2:
#   * How many clusters does this method suggest?

# Answer: It suggests 3 clusters based on the bend in the plot

#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Answer: This method works because the goal of kmeans clustering is to find a number of clusters such that 
# the variability within each cluster is sufficiently low. After k=3 the marginal descrease in variability
# by adding another group decreases, so k=3 is sufficient. The function works by building a vector of 
# within groups sums from 2 to 15 clusters after applying kmeans. For k=1, the total variation is used since
# all the data is treated as 1 cluster.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

table(nc$Best.n[1,])

# Exercise 3: How many clusters does this method suggest?

# Answer: 3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

wine.types.vector <- wine.types$Type
table(actual = as.integer(wine.types.vector), clustered = fit.km$cluster)

# Answer: the table suggest that clusters 1 and 3 were done incorrectly..?

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(pam(wine, 3))

# Answer: the plot would suggest that that clustering did an adequate job of clustering
# the 3 levels of wine as indicated by the differing point geoms and clusters shown.
