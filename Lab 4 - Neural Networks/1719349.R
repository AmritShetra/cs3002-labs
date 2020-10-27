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
