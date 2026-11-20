# BDA400 - Assignment 5
# Crossunder Detection
# Student: Rania Abdelmoaein

crossunder <- function(arr1, arr2) {

  # Check that both arrays are numeric
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("Both arrays must be numeric")
  }

  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays must have the same length")
  }

  # Initialize crossunder signals
  signals <- rep(FALSE, length(arr1))

  # Detect when arr1 crosses below arr2
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {

      if (!anyNA(c(arr1[i - 1], arr2[i - 1],
                   arr1[i], arr2[i]))) {

        signals[i] <- (
          arr1[i - 1] >= arr2[i - 1] &&
            arr1[i] < arr2[i]
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
