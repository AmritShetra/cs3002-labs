# Load in the dataset 
mydata = read.csv('iris.csv', sep=",")

# Load in the correct group of clusters we can use as a metric for scoring 
iris_real = read.csv('iris_real.csv', sep=",")

# Load in an R script - the Weighted Kappa (WK) function
source("WK_R.R")

# Create empty data-frame to hold WK scores for each clustering technique
df <- data.frame()


# List of linkage measures for Hierarchical clustering
measures <- c("single", "complete", "average")

# Create matrix of Euclidean distances between each data-point
d <- dist(mydata, method="euclidean")


# Set number of clusters
K = 10

for (i in 2:K) {
  # Perform K-means clustering
  fit <- kmeans(mydata, i)
  
  # Calculate WK between our clusters and the real clusters
  kappa <- WK_R(fit$cluster, iris_real$X1)
  
  # Put the kappa value in the ith row of the 1st column in our data-frame
  df[i, 1] <- kappa
  
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
    
    # Add the kappa scores, m+1 because Column 1 is for our K-Means scores
    df[i, m+1] <- WK_R(Hgroups, iris_real$X1)
  }
}

# Plot the first column of our data-frame
plot(
  df$V1,
  main="K-means clustering",
  xlab="Number of clusters (K)",
  ylab="Weighted Kappa value"
)

# Looping 2:4 because the 1st column in the data-frame is for the K-means score 
for (i in 2:4) {
  plot(
    df[, i],
    # Subtract 1 from i, because the measures list is of length 3
    main=paste("Hierarchical clustering: ", measures[i-1]),
    xlab="Number of clusters (K)",
    ylab="Weighted Kappa value"
  )
}
