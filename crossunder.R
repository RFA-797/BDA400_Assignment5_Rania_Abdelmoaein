
# BDA400 - Assignment 5
# Crossunder Detection
# Student: Rania Abdelmoaein

crossunder <- function(series1, series2) {
  
  # Check that both series are numeric
  if (!is.numeric(series1) || !is.numeric(series2)) {
    stop("Both series must be numeric")
  }
  
  # Check that both series have the same length
  if (length(series1) != length(series2)) {
    stop("Both series must have the same length")
  }
  
  # Initialize crossunder signals
  signals <- rep(FALSE, length(series1))
  
  # Detect when series1 crosses below series2
  if (length(series1) >= 2) {
    for (i in 2:length(series1)) {
      
      if (!anyNA(c(series1[i - 1], series2[i - 1],
                   series1[i], series2[i]))) {
        
        signals[i] <- (
          series1[i - 1] >= series2[i - 1] &&
            series1[i] < series2[i]
        )
      }
    }
  }
  
  return(signals)
}

# Test the crossunder function
series1 <- c(10, 9, 7, 11, 8, 6, 12)
series2 <- c(8, 8, 8, 9, 9, 7, 10)

crossunder_result <- crossunder(series1, series2)

print(crossunder_result)
