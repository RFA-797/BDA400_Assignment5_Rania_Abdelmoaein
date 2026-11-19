
# BDA400 - Assignment 5
# Moving Average Convergence Divergence (MACD)
# Student: Rania Abdelmoaein

# Load the EMA function from the separate script
source("ema.R", local = TRUE)

macd <- function(data, short_period, long_period, signal_period) {
  
  # Calculate short-term and long-term EMA
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate MACD histogram
  histogram <- macd_line - signal_line
  
  # Return all three results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

# Test the MACD function
data <- c(100, 105, 110, 115, 120, 125, 130)

macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

print(macd_result)
