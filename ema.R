
# BDA400 - Assignment 5
# Exponential Moving Average (EMA)
# Student: Rania Abdelmoaein

ema <- function(data, period) {
  
  # Check that the period is a positive integer
  if (!is.numeric(period) || length(period) != 1 ||
      is.na(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive integer")
  }
  
  # Check that data is available
  if (length(data) == 0) {
    stop("Data cannot be empty")
  }
  
  # Calculate the EMA multiplier
  multiplier <- 2 / (period + 1)
  
  # Initialize a vector for EMA values
  ema_values <- numeric(length(data))
  
  # Calculate EMA for each data point
  for (i in seq_along(data)) {
    
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <-
        (data[i] - ema_values[i - 1]) * multiplier +
        ema_values[i - 1]
    }
  }
  
  return(ema_values)
}

# Test the EMA function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

ema_result <- ema(data, period = 3)

print(ema_result)

