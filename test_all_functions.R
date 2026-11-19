
# BDA400 - Assignment 5
# Final Test of All Nine Functions
# Student: Rania Abdelmoaein

# Load all nine functions
source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

# Check that all functions are available
function_names <- c(
  "sma", "ema", "macd",
  "stdev", "linreg", "rsi",
  "stoch_rsi", "crossover", "crossunder"
)

function_status <- sapply(
  function_names,
  function(name) exists(name, mode = "function")
)

print(function_status)

# Confirm that all nine functions loaded
if (all(function_status)) {
  print("SUCCESS: All nine functions loaded!")
} else {
  print("ERROR: One or more functions are missing.")
}

# Final calculation tests and verification log

test_data <- c(
  45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
  67, 64, 70, 68, 73, 69, 75, 72, 78, 74,
  80, 76, 82, 79, 85, 81, 87, 83, 89, 86
)

verification <- capture.output({
  
  cat("BDA400 Assignment 5 - Function Tests\n\n")
  
  cat("1. SMA\n")
  print(sma(test_data, 5))
  
  cat("\n2. EMA\n")
  print(ema(test_data, 5))
  
  cat("\n3. MACD\n")
  print(macd(test_data, 3, 5, 2))
  
  cat("\n4. Standard Deviation\n")
  print(stdev(test_data))
  
  cat("\n5. Linear Regression\n")
  print(linreg(test_data, 5, 0))
  
  cat("\n6. RSI\n")
  print(rsi(test_data, 5))
  
  cat("\n7. Stochastic RSI\n")
  print(stoch_rsi(test_data, 5, 3, 3))
  
  cat("\n8. Crossover\n")
  print(crossover(
    c(5, 7, 9, 8, 12, 10, 14),
    c(6, 8, 8, 9, 10, 11, 12)
  ))
  
  cat("\n9. Crossunder\n")
  print(crossunder(
    c(10, 9, 7, 11, 8, 6, 12),
    c(8, 8, 8, 9, 9, 7, 10)
  ))
})

writeLines(verification, "BDA400_A05_test_results.txt")

cat("Verification log saved successfully!\n")
print(file.exists("BDA400_A05_test_results.txt"))
