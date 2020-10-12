# --------------------------
# Using the Weighted Kappa function, explore how the hierarchical clustering
# compares to K-means for the iris dataset ('iris.csv') given the correct 
# clusters in 'iris_real.csv'.
#
# 1) Calculate the WK for K-means clustering with different values of K.
# 2) Try Hierarchical with different linkage measures.
# 3) Use scatterplots to illustrate the different clusterings.
# 4) Plot the dendrograms where necessary.
# 5) Plot the different Weighted Kappa values on an appropriate graph.
# --------------------------

# Load in the datasets
mydata = read.csv('iris.csv', sep=",")
# Load in the correct clusters we can use as a metric for scoring in the WK 
iris_real = read.csv('iris_real.csv', sep=",")

# Load in an R script - the Weighted Kappa (WK) function
# WK takes in 2 clusterings and returns a score
source("WK_R.R")

# Create empty data-frame to hold WK scores for both clustering techniques
kmeans_df <- data.frame()
hier_df <- data.frame()

# List of linkage measures for Hierarchical clustering
measures <- c("single", "complete", "average")

# Create matrix of Euclidean distances between each data-point
# This is used when we do Hierarchical clustering
d <- dist(mydata, method="euclidean")


# Set number of clusters
K = 10

for (i in 2:K) {
  # Perform K-means clustering
  fit <- kmeans(mydata, i)
  
  # Calculate WK between our clusters and the real clusters
  kappa <- WK_R(fit$cluster, iris_real$X1)
  
  # Put the kappa value in the ith row of the 1st column in our data-frame
  kmeans_df[i, 1] <- kappa
  
  # Loop through the linkage measures, e.g. "average"
  for (m in 1:length(measures)) {
    # Perform Hierarchical clustering
    fit <- hclust(d, method=measures[m])
    
    plot(fit)
    
    # Create the clusters by cutting the dendrogram
    Hgroups <- cutree(fit, k=i)
    rect.hclust(fit, k=i, border="red")
    
    # Draw a scatterplot with the assigned clusters
    # plot(mydata, col=Hgroups)
    
    # Add the kappa scores to the data-frame
    hier_df[i, m] <- WK_R(Hgroups, iris_real$X1)
  }
}

# Plot the first (and only) column of the K-means data-frame
plot(
  kmeans_df$V1,
  main="K-means clustering",
  xlab="Number of clusters (K)",
  ylab="Weighted Kappa value"
)

# Looping through the 3 columns of the Hierarchical data-frame, to make 3 graphs
for (i in 1:3) {
  plot(
    hier_df[, i],
    # Subtract 1 from i, because the measures list is of length 3
    main=paste("Hierarchical clustering: ", measures[i]),
    xlab="Number of clusters (K)",
    ylab="Weighted Kappa value"
  )
}
