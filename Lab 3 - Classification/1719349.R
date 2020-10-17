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
irispred <- predict(fit, X_test, type="class")

# Compare this to the actual test values to get the accuracy
accuracy = sum(irispred == y_test) / length(y_test)
print(accuracy)
