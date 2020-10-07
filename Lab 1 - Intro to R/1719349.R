# -----------------
# An introduction to RStudio.
# Using simple commands to explore a dataset and record statistics.
# -----------------

# Load in the CSV file, comma delimited
mydata = read.csv('forestfires.csv', sep=",")

# Plot the data
plot(mydata)

# Produce a table from the data
View(mydata)

# Scatterplots of individual columns ($colname)
plot(mydata$temp, mydata$wind)
plot(mydata[,9], mydata[,11])

# Histogram of a column
hist(mydata$temp)

# Line plot of a column
plot(mydata$temp, type="l")

# Plot X and Y coordinates, colour-coded according to temperature
plot(mydata$X, mydata$Y, col=mydata$temp)

# Calculate mean, median, max and min values for temp column
meantemp = mean(mydata$temp)
mediantemp = median(mydata$temp)
maxtemp = max(mydata$temp)
mintemp = min(mydata$temp)

# Write variables to CSV
write.csv(
  data.frame(meantemp, mediantemp, maxtemp, mintemp),
  file = "Output.csv"
)

# Build a linear model using regression
plot(mydata$temp, mydata$ISI)
lmfire=line(mydata$ISI~mydata$temp)
abline(coef(lmfire))
