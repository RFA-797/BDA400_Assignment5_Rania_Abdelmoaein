
# Linear Regression Function
# BDA400 - Assignment 5

linreg <- function(regressionSource, regressionLength, regressionOffset) {

  # Validate the input data
  if (!is.numeric(regressionSource) ||
      anyNA(regressionSource)) {
    stop("regressionSource must contain numeric values without NA")
  }

  n <- length(regressionSource)

  # Validate regression length
  if (!is.numeric(regressionLength) ||
      length(regressionLength) != 1 ||
      is.na(regressionLength) ||
      regressionLength < 2 ||
      regressionLength != floor(regressionLength)) {
    stop("regressionLength must be an integer greater than 1")
  }

  # Validate regression offset
  if (!is.numeric(regressionOffset) ||
      length(regressionOffset) != 1 ||
      is.na(regressionOffset) ||
      regressionOffset < 0 ||
      regressionOffset != floor(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }

  if (regressionLength > n) {
    stop("regressionLength cannot be greater than data length")
  }

  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  if (regressionLength + regressionOffset > n) {
    stop("Not enough data for the selected length and offset")
  }

  # Calculate starting and ending indices
  start_index <- n - regressionOffset - regressionLength + 1
  end_index <- n - regressionOffset

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

  # Return results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}

# Test the Linear Regression function
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

linreg_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)

# Additional test with offset = 1
offset_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 1
)

print(offset_result)
