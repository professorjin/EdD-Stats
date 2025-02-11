# transformation_explorer.R

# 1. Load Libraries (Only those needed for data manipulation)
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
if (!require(tidyr)) { # For pivot_longer (optional, for display)
  install.packages("tidyr")
}
library(tidyr)

# 2. Create Timestamped Output Directory
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path("output", paste0("transformations_", timestamp))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Ask for data source
data_source <- readline(prompt = "Enter 'csv' to use a CSV file, or 'env' to use existing data: ")

# 4. Import Data
if (data_source == "csv") {
  working_file <- readline(prompt = "Enter the name of the CSV file (including .csv): ")
  if (!file.exists(working_file)) {
    stop("File not found.")
  }
  working_data <- read.csv(working_file)
} else if (data_source == "env") {
  working_data_name <- readline(prompt = "Enter the name of the data frame in the environment: ")
  if (!exists(working_data_name) || !is.data.frame(get(working_data_name))) {
    stop("Invalid data frame in environment.")
  }
  working_data <- get(working_data_name)
} else {
  stop("Invalid data source.")
}

# 5. Display available variables (TO CONSOLE)
message("Available variables:")
for (i in 1:ncol(working_data)) {
  message(i, "-", names(working_data)[i])
}

# 6. Select Variable to Transform
var_num_to_transform <- as.integer(readline(prompt = "Enter the NUMBER of the variable you want to transform: "))

if (is.na(var_num_to_transform) || var_num_to_transform < 1 || var_num_to_transform > ncol(working_data)) {
  stop("Invalid variable number.")
}

var_to_transform <- names(working_data)[var_num_to_transform]
cat("You selected variable:", var_to_transform, "\n")

# 7. Transformation Options (Interactive Menu)
message("\nChoose a transformation:")
message("1. Natural Logarithmic (ln(x + c))") # Changed to Natural Log
message("2. Square Root (sqrt(x))")
message("3. Reciprocal (1/x)")
message("4. Square (x^2)")
message("5. Box-Cox Transformation")
message("6. Arcsine Square Root (sin^-1(sqrt(x)))") # Added Arcsine Square Root
message("7. No transformation (Exit and save current data)") # Added exit option

transform_choice <- as.integer(readline(prompt = "Enter the number of your choice: "))

if (is.na(transform_choice) || transform_choice < 1 || transform_choice > 7) {
  stop("Invalid choice number")
}

transformed_data <- working_data # Initialize

if (transform_choice == 1) {
  # Natural Logarithmic Transformation
  c_value <- as.numeric(readline(prompt = "Enter a constant 'c' to add before taking the natural log (or 0 for no constant): "))
  if (is.na(c_value)) {
    stop("You must enter a numeric value")
  }
  new_var_name <- paste0("ln_", var_to_transform)  # Use "ln" for natural log
  transformed_data[[new_var_name]] <- log(working_data[[var_to_transform]] + c_value) # log() is natural log in R
  cat("Created new variable:", new_var_name, "\n")
  
} else if (transform_choice == 2) {
  # Square Root Transformation
  if (any(working_data[[var_to_transform]] < 0)) {
    stop("Cannot perform square root transformation on negative numbers")
  }
  new_var_name <- paste0("sqrt_", var_to_transform)
  transformed_data[[new_var_name]] <- sqrt(working_data[[var_to_transform]])
  cat("Created new variable:", new_var_name, "\n")
  
} else if (transform_choice == 3) {
  # Reciprocal Transformation
  if (any(working_data[[var_to_transform]] == 0)) {
    stop("Cannot perform reciprocal transformation with values of 0")
  }
  new_var_name <- paste0("reciprocal_", var_to_transform)
  transformed_data[[new_var_name]] <- 1 / working_data[[var_to_transform]]
  cat("Created new variable:", new_var_name, "\n")
  
} else if (transform_choice == 4) {
  # Square Transformation
  new_var_name <- paste0("sq_", var_to_transform)
  transformed_data[[new_var_name]] <- working_data[[var_to_transform]]^2
  cat("Created new variable:", new_var_name, "\n")
  
} else if (transform_choice == 5) {
  # Box-Cox Transformation
  if (any(working_data[[var_to_transform]] <= 0)) {
    stop("Cannot perform Box-Cox transformation on non-positive numbers")
  }
  bc <- MASS::boxcox(working_data[[var_to_transform]] ~ 1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  new_var_name <- paste0("boxcox_", var_to_transform)
  transformed_data[[new_var_name]] <- (working_data[[var_to_transform]]^lambda - 1) / lambda
  cat("Created new variable:", new_var_name, "\n")
  cat("Lambda used: ", lambda, "\n")
  
} else if (transform_choice == 6) {
  # Arcsine Square Root Transformation
  if (any(working_data[[var_to_transform]] < 0) || any(working_data[[var_to_transform]] > 1)) {
    stop("Arcsine square root transformation requires values between 0 and 1.")
  }
  new_var_name <- paste0("arcsin_sqrt_", var_to_transform)
  transformed_data[[new_var_name]] <- asin(sqrt(working_data[[var_to_transform]]))
  cat("Created new variable:", new_var_name, "\n")
  
} else if (transform_choice == 7) {
  cat("No Transformation Applied.\n")
}

# 8. Display Summary of Transformed Data (Optional, but good practice)
cat("\nSummary of the transformed data (first 6 rows):\n")
print(head(transformed_data))

# 9. Save Transformed Data
output_file <- file.path(output_dir, "transformed_data.csv")
write.csv(transformed_data, file = output_file, row.names = FALSE)
cat("\nTransformed data saved to:", output_file, "\n")
