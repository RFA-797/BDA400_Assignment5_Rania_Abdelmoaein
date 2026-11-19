
# BDA400 - Assignment 5
# Simple Moving Average (SMA)
# Student: Rania Abdelmoaein

sma <- function(data, period) {
  
  # Check that the period is valid
  if (length(period) != 1 || is.na(period) ||
      !is.numeric(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive integer")
  }
  
  # Check that enough data is available
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize a vector for SMA values
  sma_values <- numeric(length(data) - period + 1)
  
  # Calculate the average for each window
  for (i in seq_along(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}

# Test the SMA function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

sma_result <- sma(data, period = 3)

print(sma_result)
