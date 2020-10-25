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
