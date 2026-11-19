
# BDA400 - Assignment 5
# Crossover Detection
# Student: Rania Abdelmoaein

crossover <- function(series1, series2) {
  
  # Check that both series are numeric
  if (!is.numeric(series1) || !is.numeric(series2)) {
    stop("Both series must be numeric")
  }
  
  # Check that both series have the same length
  if (length(series1) != length(series2)) {
    stop("Both series must have the same length")
  }
  
  # Initialize crossover signals
  signals <- rep(FALSE, length(series1))
  
  # Detect when series1 crosses above series2
  if (length(series1) >= 2) {
    for (i in 2:length(series1)) {
      if (!anyNA(c(series1[i - 1], series2[i - 1],
                   series1[i], series2[i]))) {
        signals[i] <- (
          series1[i - 1] <= series2[i - 1] &&
            series1[i] > series2[i]
        )
      }
    }
  }
  
  return(signals)
}

# Test the crossover function
series1 <- c(5, 7, 9, 8, 12, 10, 14)
series2 <- c(6, 8, 8, 9, 10, 11, 12)

crossover_result <- crossover(series1, series2)

print(crossover_result)
