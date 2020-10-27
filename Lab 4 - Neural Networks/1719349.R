# -----------------
# Try to set up the XOR problem and see what happens when you learn the weights.
# Input (-1,-1); Output: 0
# Input (1, -1); Output: 1
# Input (-1, 1); Output: 1
# Input (1, 1); Output: 0
# Does it learn to correctly classify all the inputs?
# -----------------

# Set up a training set - we'll go with an OR gate
train_in <- rbind(
  c(1, 1),
  c(1, -1),
  c(-1, 1),
  c(-1, -1)
)
train_out <- rbind(1, 1, 1, 0)

# Fit the neural network (with no hidden layers)
set.seed(2)
NN <- neuralnet(train_out~., train_in, hidden=0, threshold=0.001,
                stepmax=1e+05, linear.output=FALSE)

# Visualise the NN
plot(NN)

# Let's see how the network responds to an input of (1, 1)
test_in <- rbind(c(1,1))

# Make predictions using a specified neural network
result <- compute(NN, test_in)
raw_predictions <- result$net.result

# To calculate the discrete class, we threshold it at 0.5
final_predictions <- as.numeric(raw_predictions>0.5)

# This should predict the output as 1 - we now know the NN works
print(final_predictions)

# Set up the XOR test input
test_in <- rbind(
  c(-1, -1),
  c(1, -1),
  c(-1, 1),
  c(1, 1)
)

# This doesn't classify the XOR problem correctly as there is no hidden layer
result <- compute(NN, test_in)
raw_predictions <- result$net.result
# print(result$neurons)
print(raw_predictions)
final_predictions <- as.numeric(raw_predictions>0.5)
print(final_predictions)


# -----------------
# Build a Neural Network classifier of the wine data.
# 1) Read in "winedata2.csv".
# 2) Build the architecture of your neural network. The output must be between one and zero.
# 3) Using any two variables from the test data, create a train and test set.
# 4) Train the neural network on half of the data and test it on the remaining.
# 5) Calculate the accuracy.
# -----------------

wine_data <- read.csv("winedata2.csv", sep=",")

# Convert values in "WineClass" to binary
for (i in 1:length(wine_data$WineClass)) {
  if (wine_data$WineClass[i] == 1) {
    wine_data$WineClass[i] <- 0
  }
  else {
    wine_data$WineClass[i] <- 1
  }
}


X <- wine_data[, 2:3]
y <- wine_data[, 1]

X_train <- X[1:65, ]
y_train <- y[1:65]

X_test <- X[66:130, ]
y_test <- y[66:130]

NN <- neuralnet(y_train~., X_train, hidden=c(3,3), threshold=0.001,
                stepmax=1e+05, linear.output=FALSE)
plot(NN)

result <- compute(NN, X_test)
raw_predictions <- result$net.result
final_predictions <- as.numeric(raw_predictions>0.5)

n_correct = sum(final_predictions == y_test)
print(n_correct / length(y_test))
