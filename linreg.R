
# BDA400 - Assignment 5
# Linear Regression (linreg)
# Student: Rania Abdelmoaein

linreg <- function(regressionSource,
                   regressionLength,
                   regressionOffset) {
  
  # Calculate total number of data points
  n <- length(regressionSource)
  
  # Check regression length
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than data length")
  }
  
  # Check offset
  if (regressionOffset < 0 ||
      regressionOffset >= regressionLength) {
    stop("regressionOffset must be between 0 and regressionLength - 1")
  }
  
  # Calculate starting and ending indices
  start_index <- n - regressionOffset - regressionLength + 1
  end_index <- min(n, n - regressionOffset)
  
  # Extract the relevant data
  source_subset <- regressionSource[start_index:end_index]
  
  # Create index values
  index_values <- seq_along(source_subset)
  
  # Calculate means
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Calculate numerator and denominator
  numerator <- sum(
    (index_values - mean_index) *
      (source_subset - mean_source)
  )
  
  denominator <- sum((index_values - mean_index)^2)
  
  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return the results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Test the linear regression function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

linreg_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)
