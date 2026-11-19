
# BDA400 - Assignment 5
# Stochastic RSI (StochRSI)
# Student: Rania Abdelmoaein

# Load the required functions
source("rsi.R", local = TRUE)
source("sma.R", local = TRUE)

stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Validate the smoothing periods
  if (length(k_period) != 1 || !is.numeric(k_period) ||
      is.na(k_period) || k_period < 1 ||
      k_period != floor(k_period) ||
      length(d_period) != 1 || !is.numeric(d_period) ||
      is.na(d_period) || d_period < 1 ||
      d_period != floor(d_period)) {
    stop("k_period and d_period must be positive integers")
  }
  
  # Calculate RSI values
  rsi_values <- rsi(data, period)
  
  # Initialize Stochastic RSI values
  stoch_values <- rep(NA_real_, length(data))
  
  # Normalize RSI over a rolling period
  for (i in seq_along(rsi_values)) {
    
    if (i >= 2 * period) {
      window <- rsi_values[(i - period + 1):i]
      
      if (!anyNA(window)) {
        lowest <- min(window)
        highest <- max(window)
        
        if (highest == lowest) {
          stoch_values[i] <- 0
        } else {
          stoch_values[i] <-
            (rsi_values[i] - lowest) / (highest - lowest)
        }
      }
    }
  }
  
  # Calculate the %K line using SMA
  k_line <- rep(NA_real_, length(data))
  
  for (i in seq_along(stoch_values)) {
    if (i >= k_period) {
      window <- stoch_values[(i - k_period + 1):i]
      
      if (!anyNA(window)) {
        k_line[i] <- sma(window, k_period)[1]
      }
    }
  }
  
  # Calculate the %D line using SMA
  d_line <- rep(NA_real_, length(data))
  
  for (i in seq_along(k_line)) {
    if (i >= d_period) {
      window <- k_line[(i - d_period + 1):i]
      
      if (!anyNA(window)) {
        d_line[i] <- sma(window, d_period)[1]
      }
    }
  }
  
  # Return the two indicator lines
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

# Test with enough data for RSI and smoothing
data <- c(
  45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
  67, 64, 70, 68, 73, 69, 75, 72, 78, 74,
  80, 76, 82, 79, 85, 81, 87, 83, 89, 86
)

stoch_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

print(tail(stoch_result$k_line, 5))
print(tail(stoch_result$d_line, 5))
