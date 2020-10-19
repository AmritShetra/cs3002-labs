# -------------------------
# Read in "iris.csv" from last week's clustering lab.
# Write a script to do the following:
# 
# 1) Learn a decision tree for the 3 classes of iris. Save some data for testing.
#    Mix up the ordering of the rows - use iris_rand=iris[sample(150,150,)] to do this.
# 2) Test it by scoring the accuracy on test data using different degrees of pruning.
# 3) Scatterplot 2 selected variables of the data (and colour code according 
#    to the decision tree output), display the learnt tree and calculate the accuracy.
# 4) Compare the accuracy of the different pruned trees to KNN with different values of k.
# -------------------------

# Read in datasets and combine them into one
iris <- cbind(
  read.csv("../Lab 2 - Clustering/iris.csv", sep=",", header=FALSE),
  read.csv("../Lab 2 - Clustering/iris_real.csv", sep=",", header=FALSE)
)

# Post-concatenation, col 5 is the same as col 1, so rename it to avoid confusion
names(iris)[5] = "V5"

# Randomise the dataset
iris_rand <- iris[sample(150, 150), ]

# Separate values (X) and class (y)
X <- iris_rand[, 1:4]
y <- iris_rand[, 5]

# Create train and test datasets (60/40 split)
X_train <- X[1:90, ]
y_train <- y[1:90]

X_test <- X[90:150, ]
y_test <- y[90:150]

# Create a decision tree
install.packages("rpart")
library(rpart)
fit <- rpart(y_train~., method="class", data=X_train)

# Plot the decision tree
plot(fit, uniform=TRUE, main="Decision Tree for iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Make predictions on the test set
tree_pred <- predict(fit, X_test, type="class")

# Compare this to the actual test values to get the accuracy
accuracy <- sum(tree_pred == y_test) / length(y_test)
print(accuracy)


# See what effect pruning has (using the cp parameter)
for (i in c(0.1, 0.5, 0.9)) {
  pfit <- prune(fit, cp=i)
  # plot(pfit, uniform=TRUE, main="Pruned decision tree for iris")
  # text(pfit, use.n=TRUE, all=TRUE, cex=.8)
  
  # Calculate the accuracy of the pruned tree
  tree_pred <- predict(pfit, X_test, type='class')
  accuracy <- sum(tree_pred == y_test) / length(y_test)
  message("cp: " , i, ", Accuracy: ", accuracy)
}


# Select 2 variables of the data
vars = c("V1", "V2")

# Plot them on a scatterplot
plot(
  iris_rand$V1, iris_rand$V2,
  main="V1 and V2",
  col=c("blue", "red")
)

# Get a subset of the X datasets, just containing the above columns
X_train_2 = X_train[, vars]
X_test_2 = X_test[, vars]

# Fit the decision tree and create a graph
fit <- rpart(y_train~., method='class', data=X_train_2)
plot(fit, uniform=TRUE, main="Decision Tree for iris (V1 and V2)")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Make predictions on the test set
tree_pred <- predict(fit, X_test_2, type="class")

# Compare this to the actual test values to get the accuracy
accuracy <- sum(tree_pred == y_test) / length(y_test)
print(accuracy)


# Classifying using K-Nearest Neighbour - first, install the class lib
library(class)

# Generate predictions using different values of "k"
for (i in c(3, 6, 9, 12, 15)) {
  knn_pred = knn(X_train, X_test, y_train, k=i)
  accuracy <- sum(knn_pred == y_test) / length(y_test)
  message("k: " , i, ", Accuracy: ", accuracy)
}
