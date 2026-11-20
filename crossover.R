# BDA400 - Assignment 5
# Crossover Detection
# Student: Rania Abdelmoaein

crossover <- function(arr1, arr2) {

  # Check that both arrays are numeric
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("Both arrays must be numeric")
  }

  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays must have the same length")
  }

  # Initialize crossover signals
  signals <- rep(FALSE, length(arr1))

  # Detect when arr1 crosses above arr2
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {

      if (!anyNA(c(arr1[i - 1], arr2[i - 1],
                   arr1[i], arr2[i]))) {

        signals[i] <- (
          arr1[i - 1] <= arr2[i - 1] &&
            arr1[i] > arr2[i]
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
