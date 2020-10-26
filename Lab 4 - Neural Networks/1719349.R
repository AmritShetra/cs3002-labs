# -----------------
# Try to set up the XOR problem and see what happens when you learn the weights.
# Input (-1,-1); Output: 0
# Input (1, -1); Output: 1
# Input (-1, 1); Output: 1
# Input (1, 1); Output: 0
# Does it learn to correctly classify all the inputs?
# -----------------

predict <- function(neural_network, test_data) {
  predict_testNN = compute(neural_network, test_data)
  print(predict_testNN$neurons)
  return(predict_testNN$net.result)
}

classify_predictions <- function(net_result) {
  # To calculate the discrete class, we threshold it at 0.5
  return(as.numeric(net_result>0.5))
}


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
NN <- neuralnet(
  train_out~.,
  train_in,
  hidden=0,
  threshold=0.001,
  stepmax=1e+05,
  linear.output=FALSE
)

# Visualise the NN
plot(NN)

# Look at the random weights and biases
print(NN$weights)

# Let's see how the network responds to an input of (1, 1)
test_in <- rbind(c(1,1))

# This should predict the output as 1
net_result <- predict(NN, test_in)
predict_out <- classify_predictions(net_result)
print(predict_out)


# Set up the test output (XOR gate)
test_in <- rbind(
  c(-1, -1),
  c(1, -1),
  c(-1, 1),
  c(1, 1)
)

# This doesn't classify the XOR problem correctly as there is no hidden layer
net_result <- predict(NN, test_in)
predict_out <- classify_predictions(net_result)
print(predict_out)
