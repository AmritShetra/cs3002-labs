# -----------------
# Try to set up the XOR problem and see what happens when you learn the weights.
# Input (-1,-1); Output: 0
# Input (1, -1); Output: 1
# Input (-1, 1); Output: 1
# Input (1, 1); Output: 0
# Does it learn to correctly classify all the inputs?
# -----------------

# Set up a test set - we'll go with an OR gate
train_in <- rbind(
  c(1, 1),
  c(1, -1),
  c(-1, 1),
  c(-1, -1)
)
train_out <- rbind(1, 1, 1, 0)

# Fit the neural network (with no hidden layers)
set.seed(2)
NN = neuralnet(
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

# Let's use `compute` to see if the network responds to an input of (1, 1)
test_in = rbind(c(1,1))
predict_testNN = compute(NN, test_in)

# The activation of the output neuron is here
predict_testNN$net.result

# To calculate the discrete class, we threshold it at 0.5
predict_out = as.numeric(predict_testNN$net.result>0.5)

# This should predict the output as 1
print(predict_out)


# Set up the test output (XOR gate)
test_in = rbind(
  c(-1, -1),
  c(1, -1),
  c(-1, 1),
  c(1, 1)
)
