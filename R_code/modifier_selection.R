# Simplified Interactive Regression with Type III ANOVA

# Load necessary libraries
packages <- c("car", "here", "lubridate")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("Package", pkg, "could not be installed/loaded."))
    }
  }
  library(pkg, character.only = TRUE)
}

# --- Function Definitions ---

# 1. Create Output Directory
create_output_directory <- function() {
  current_time_pst <- with_tz(Sys.time(), "America/Los_Angeles")
  timestamp <- format(current_time_pst, "%y_%m_%d_%H_%M_%S")
  output_dir <- here::here("output", timestamp)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop("Failed to create output directory.")
  }
  return(output_dir)
}

# 2. Select Data Frame (Simplified)
select_dataframe <- function() {
  available_objects <- ls(envir = .GlobalEnv)
  data_frames <- available_objects[sapply(available_objects, function(x) is.data.frame(get(x)))]
  
  if (length(data_frames) == 0) {
    stop("No data frames found. Please load data first.")
  }
  
  cat("Available data frames:\n")
  for (i in seq_along(data_frames)) {
    cat(i, ": ", data_frames[i], "\n")
  }
  
  while (TRUE) {
    selection <- as.integer(readline(prompt = "Enter the NUMBER of the data frame: "))
    if (!is.na(selection) && selection >= 1 && selection <= length(data_frames)) {
      df_name <- data_frames[selection]
      df <- get(df_name)
      return(list(data = df, name = df_name))  # Return both data and name
    } else {
      message("Invalid selection. Please try again.")
    }
  }
}

# 3. Select Variables (Simplified)
select_variables <- function(df, df_name) {
  cat("\nAvailable variables in", df_name, ":\n")
  variable_names <- names(df)
  for (i in seq_along(variable_names)) {
    cat(i, ": ", variable_names[i], "\n")
  }
  
  # Dependent variable
  while (TRUE) {
    dep_var_num <- as.integer(readline(prompt = "Enter the NUMBER of the dependent variable: "))
    if (!is.na(dep_var_num) && dep_var_num >= 1 && dep_var_num <= length(variable_names)) {
      dep_var <- variable_names[dep_var_num]
      if (!is.numeric(df[[dep_var]])) {  # Check if numeric
        message("Dependent variable must be numeric. Please select again.")
      } else {
        break
      }
    } else {
      message("Invalid selection.")
    }
  }
  
  # Independent variables
  while (TRUE) {
    ind_vars_input <- readline(prompt = "Enter the NUMBER(s) of the independent variables (comma-separated): ")
    ind_vars_nums <- as.integer(strsplit(ind_vars_input, ",")[[1]])
    
    if (all(!is.na(ind_vars_nums)) && all(ind_vars_nums >= 1) && all(ind_vars_nums <= length(variable_names)) && all(ind_vars_nums != dep_var_num)) {
      ind_vars <- variable_names[ind_vars_nums]
      
      #Check if numeric/integer
      non_numeric_ind_vars <- ind_vars[!sapply(df[ind_vars], is.numeric)]
      if (length(non_numeric_ind_vars) > 0) {
        message("Independent variables must be numeric or integer. Please select again.")
      } else {
        break; # Valid selection
      }
    } else {
      message("Invalid selection.  Make sure the numbers are valid and do not include the dependent variable.")
    }
  }
  
  
  return(list(dep_var = dep_var, ind_vars = ind_vars))
}


# 4. Perform Regression and Output ANOVA Table
perform_regression <- function(df, dep_var, ind_vars, output_dir, df_name) { # Added df_name
  # Build formula
  formula_str <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
  model <- lm(as.formula(formula_str), data = df)
  
  # Create output file
  output_file <- file.path(output_dir, "regression_results.txt")
  sink(output_file)
  
  # Output header
  cat("Regression Analysis (Type III ANOVA)\n")
  cat("Data Frame:", df_name, "\n") # Use df_name
  cat("Dependent Variable:", dep_var, "\n")
  cat("Independent Variables:", paste(ind_vars, collapse = ", "), "\n\n")
  
  # Output Type III ANOVA table
  print(car::Anova(model, type = "III"))
  
  sink()  # Close the file connection
  message("Regression results saved to: ", output_file)
  return(model)
}

# --- Main Script Flow ---
main <- function() {
  output_dir <- create_output_directory()
  data_info <- select_dataframe()
  df <- data_info$data
  df_name <- data_info$name  # Get the data frame name
  variable_info <- select_variables(df, df_name)
  dep_var <- variable_info$dep_var
  ind_vars <- variable_info$ind_vars
  model <- perform_regression(df, dep_var, ind_vars, output_dir, df_name) # Pass df_name
}

# Run the main function if interactive
if (interactive()) {
  main()
}