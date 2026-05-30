## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Create a sample data frame with log10 data
my_data <- data.frame(
  Time = c(0, 1, 2, 3),
  log10N = c(2, 2.5, 3, 3.5)
)

# Convert the log10N column to lnN
my_data$lnN <- log(10) * my_data$log10N

# Print the updated data frame
print(my_data)

