
# BDA400 - Assignment 5
# Standard Deviation (stdev)
# Student: Rania Abdelmoaein

stdev <- function(data) {
  
  # Check that the data is numeric and not empty
  if (!is.numeric(data) || length(data) == 0) {
    stop("Data must be a non-empty numeric vector")
  }
  
  # Calculate the mean
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from the mean
  diff_values <- data - mean_value
  
  # Square the differences
  squared_diff <- diff_values^2
  
  # Calculate population variance
  variance <- sum(squared_diff) / length(data)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Test the standard deviation function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

stdev_result <- stdev(data)

print(stdev_result)
