#This is the primary script to run the multiple regression. Here's what it will do:
# Ask you to select a dataframe from a list
# Ask you to select your dependent variable
# Ask you to select your dependent variable


# --- Function Definitions ---

# 1. Select Data Frame
select_dataframe <- function() {
  available_objects <- ls(envir = .GlobalEnv)
  data_frames <- available_objects[sapply(available_objects, function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || "tbl_df" %in% class(obj)
  })]
  
  if (length(data_frames) == 0) {
    stop("No data frames found in the environment.  Please load data first.")
  }
  
  prompt_string <- paste("Select a data frame:\n",
                         paste(seq_along(data_frames), data_frames, sep = ": ", collapse = "\n"),
                         "\nEnter the number: ")
  
  while (TRUE) {
    selection <- readline(prompt = prompt_string)
    selection_num <- suppressWarnings(as.integer(selection))
    
    if (!is.na(selection_num) && selection_num >= 1 && selection_num <= length(data_frames)) {
      working_data <- get(data_frames[selection_num], envir = .GlobalEnv)
      data_file <- data_frames[selection_num]
      break
    } else {
      message("Invalid selection. Please enter a number between 1 and", length(data_frames))
    }
  }
  
  message("Selected data frame:", data_file, "\n")
  return(list(data = working_data, name = data_file))
}

# 2. Select Variables
select_variables <- function(working_data) {
  variable_names <- names(working_data)
  
  # 2.1 Display available variables (TO CONSOLE ONLY)
  message("Available variables:")
  for (i in 1:ncol(working_data)) {
    message(i, "-", names(working_data)[i])
  }
  # 2.2 Get dependent variable.
  prompt_dep_var <- paste("Select the dependent variable:\n",
                          paste(seq_along(variable_names), variable_names, sep = ": ", collapse = "\n"),
                          "\nEnter the number: ")
  dep_var_num <- as.integer(readline(prompt = prompt_dep_var))
  # 2.3 Validate dependent variable selection
  if (is.na(dep_var_num) || dep_var_num < 1 || dep_var_num > length(variable_names)) {
    stop("Invalid dependent variable selection.")
  }
  # 2.4 Get independent variables.
  prompt_ind_vars <- paste("Select the independent variables (comma-separated):\n",
                           paste(seq_along(variable_names), variable_names, sep = ": ", collapse = "\n"),
                           "\nEnter the numbers (comma-separated): ")
  ind_vars_nums_str <- strsplit(readline(prompt = prompt_ind_vars), ",")[[1]]
  ind_vars_nums <- as.integer(ind_vars_nums_str)
  
  # 2.5 Validate independent variable selection
  if (any(is.na(ind_vars_nums)) || any(ind_vars_nums < 1) || any(ind_vars_nums > length(variable_names)) || any(ind_vars_nums == dep_var_num)) {
    stop("Invalid independent variable selection.  Make sure numbers are valid and do not include the dependent variable.")
  }
  
  dep_var <- names(working_data)[dep_var_num]
  ind_vars <- names(working_data)[ind_vars_nums]
  
  return(list(dep_var = dep_var, ind_vars = ind_vars))
}

# 3. Create Output Directory
create_output_directory <- function() {
  current_time_pst <- with_tz(Sys.time(), "America/Los_Angeles")
  timestamp <- format(current_time_pst, "%y_%m_%d_%H_%M_%S")
  output_dir <- here::here("output", timestamp)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(output_dir)) {
    stop(paste("Failed to create output directory:", output_dir))
  }
  return(output_dir)
}

# 4. Perform Listwise Deletion
perform_listwise_deletion <- function(data, dep_var, ind_vars){
  n_rows_initial <- nrow(data)
  cleaned_data <- na.omit(data[, c(dep_var, ind_vars)])
  n_rows_deleted <- n_rows_initial - nrow(cleaned_data)
  message("Number of rows deleted due to missing values:", n_rows_deleted, "\n")
  return (cleaned_data)
}

# 5. Create Scatterplots for Linearity Check
create_scatterplots <- function(working_data, dep_var, ind_vars, output_dir) {
  plot_list <- list()
  
  for (ind_var in ind_vars) {
    p <- ggplot(working_data, aes(x = .data[[ind_var]], y = .data[[dep_var]])) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "blue", formula = 'y~x') +
      labs(title = paste("Scatterplot of", ind_var, "vs.", dep_var), x = ind_var, y = dep_var) +
      theme_bw()
    plot_list[[ind_var]] <- p
  }
  
  scatter_plots_file <- file.path(output_dir, paste0("scatterplots.png"))
  scatter_plot_grid <- do.call(grid.arrange, c(plot_list, ncol = min(3, length(plot_list))))
  ggsave(scatter_plots_file, scatter_plot_grid, width = 12, height = 8, dpi = 300)
  message("Scatterplots saved to: ", scatter_plots_file)
}

# 6. Run All Stepwise Regression Methods
run_all_stepwise <- function(working_data, dep_var, ind_vars, working_formula) {
  
  # Perform listwise deletion *before* model fitting
  working_data <- perform_listwise_deletion(working_data, dep_var, ind_vars)
  initial_model <- lm(working_formula, data = working_data)
  n <- nrow(working_data)
  
  
  # 6.1 P-value Based Stepwise (Detailed Output)
  cat("\n--- P-value Based Stepwise Regression (Both Directions) ---\n")
  p_value_model <- step(initial_model, direction = "both", trace = 1,
                        scope = list(lower = ~1, upper = working_formula))
  cat("\n--- Final Model Formula (P-value Based) ---\n")
  print(formula(p_value_model))
  
  # 6.2 AIC Stepwise (Concise Output)
  aic_model <- stepAIC(initial_model, direction = "both", trace = 0,
                       scope = list(lower = ~1, upper = working_formula))
  cat("\n--- Final Model Formula (AIC Based) ---\n")
  print(formula(aic_model))
  
  # 6.3 BIC Stepwise (Concise Output)
  bic_model <- step(initial_model, direction = "both", trace = 0,
                    scope = list(lower = ~1, upper = working_formula), k = log(n))
  cat("\n--- Final Model Formula (BIC Based) ---\n")
  print(formula(bic_model))
  cat("\n--- End Stepwise Regression Results ---\n\n")
  
  return(p_value_model) # Return the p-value based model
}



# 7. Run Regression Analysis (Now uses p-value stepwise model and cleaned data)
run_regression_analysis <- function(working_data, dep_var, ind_vars, output_dir) {
  
  # 7.1 Open Output File
  output_file <- file.path(output_dir, paste0("regression_results.txt"))
  sink(output_file)
  
  # 7.2 Output header
  readable_timestamp <- format(with_tz(Sys.time(), "America/Los_Angeles"), "%b %d, %H:%M:%S")
  cat("Regression Analysis Run:", readable_timestamp, "\n\n")
  
  # 7.3 Construct *initial* formula (for stepwise scope)
  formula_string <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
  working_formula <- as.formula(formula_string)
  #cat("Initial Model Formula:", formula_string, "\n\n")  # No longer needed
  
  # 7.4 Run *all* stepwise regressions, get the p-value based final model
  working_model <- run_all_stepwise(working_data, dep_var, ind_vars, working_formula)
  
  
  # 7.5 Output Results (using the final model from p-value stepwise)
  cat("Model Summary:\n")
  print(summary(working_model))
  
  cat("\nANOVA Table (Type III):\n")
  print(car::Anova(working_model, type = 3))
  
  # Durbin-Watson Test
  cat("\nDurbin-Watson Test for Autocorrelation:\n")
  print(lmtest::dwtest(working_model))
  
  if (length(working_model$coefficients) > 2) {
    vif_values <- vif(working_model)
    tolerance_values <- 1 / vif_values
    cat("\nVIF and Tolerance Values:\n")
    vif_tolerance_df <- data.frame(
      VIF = ifelse(vif_values > 10, paste0(round(vif_values, 3), "*"), round(vif_values,3)),
      Tolerance = ifelse(tolerance_values < 0.1, paste0(round(tolerance_values,3), "*"), round(tolerance_values,3))
    )
    print(vif_tolerance_df)
    cat("(*) VIF > 10 or Tolerance < 0.1\n")
  } else {
    cat("\nVIF and Tolerance cannot be calculated with only an intercept.\n")
  }
  
  
  cat("\n95% Confidence Intervals:\n")
  print(confint(working_model, level = 0.95))
  
  cat("\nData Summary (First 6 rows):\n")
  print(head(working_data))
  
  # 7.6 Create ggplot2 Diagnostic Plots (using the final model)
  # 7.6.1 Residuals vs Fitted
  model_data <- data.frame(
    Fitted = fitted(working_model),
    Residuals = resid(working_model),
    Row = 1:nrow(working_data)
  )
  
  cooks_d <- cooks.distance(working_model)
  influential_points <- order(cooks_d, decreasing = TRUE)[1:3]
  
  p1 <- ggplot(model_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red", formula = 'y ~ x') +
    geom_text(data = model_data[influential_points, ], aes(label = Row), vjust = -0.5, hjust = 0.5, color = "blue") +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_bw()
  
  # 7.6.2 P-P Plot
  n <- length(resid(working_model))
  observed_p <- ppoints(n)[rank(resid(working_model))]
  theoretical_p <- pnorm(scale(resid(working_model)))
  
  conf_level <- 0.95
  alpha <- 1 - conf_level
  z_alpha <- qnorm(1 - alpha / 2)
  lower_bound <- observed_p - z_alpha * sqrt((observed_p * (1 - observed_p)) / n)
  upper_bound <- observed_p + z_alpha * sqrt((observed_p * (1 - observed_p)) / n)
  
  lower_bound <- pmax(0, lower_bound)
  upper_bound <- pmin(1, upper_bound)
  
  pp_df <- data.frame(
    Theoretical = theoretical_p,
    Observed = observed_p,
    Lower = lower_bound,
    Upper = upper_bound
  )
  
  p3 <- ggplot(pp_df, aes(x = Theoretical, y = Observed)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
    labs(title = "P-P Plot", x = "Theoretical Probabilities", y = "Observed Probabilities") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_bw()
  
  # 7.6.3 Histogram of Residuals
  p2 <- ggplot(data.frame(Residuals = resid(working_model)), aes(x = Residuals)) +
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),  # Freedman-Diaconis rule
                   fill = "lightblue", color = "black") +
    labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
    theme_bw()
  
  
  # 7.6.4 Arrange and Save Diagnostic Plots
  combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)
  diagnostic_plots_file <- file.path(output_dir, paste0("diagnostic_plots.png"))
  ggsave(diagnostic_plots_file, combined_plot, width = 15, height = 5, dpi = 300)
  
  # 7.7 Save the model object
  model_object_file <- file.path(output_dir, paste0("model_object.RData"))
  save(working_model, file = model_object_file)
  
  message("Regression analysis complete. See '", output_file, "' for details.")
  sink() # 7.8 Close output file
  return(working_model)
}

# 8. Detect and Display Outliers (Modified from find_high_leverage)
detect_outliers <- function(data, dep_var, ind_vars, n = 10) {
  # 8.1 Fit the Linear Model (after listwise deletion)
  data_deleted <- perform_listwise_deletion(data, dep_var, ind_vars)
  formula_string <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))
  formula <- as.formula(formula_string)
  model <- lm(formula, data = data_deleted)
  
  # 8.2 Calculate Outlier Statistics
  leverage_values <- hatvalues(model)
  standardized_residuals <- rstandard(model)
  cooks_d <- cooks.distance(model)

  # 8.3 Identify Outliers based on multiple criteria
  k <- length(model$coefficients) - 1  # Number of predictors
  n <- nrow(data_deleted)             # Number of observations after listwise deletion
  f_threshold <- qf(0.50, df1 = k + 1, df2 = n - k - 1)
  
  outlier_indices_resid <- which(abs(standardized_residuals) > 3)
  outlier_indices_leverage <- which(leverage_values > 2 * (k + 1) / n)
  outlier_indices_cooks <- which(cooks_d > f_threshold)
  
  all_outlier_indices <- unique(c(outlier_indices_resid, outlier_indices_leverage, outlier_indices_cooks))
  
  
  # 8.4 Create Data Frame for Output
  if (length(all_outlier_indices) > 0) {
    outlier_data <- data.frame(
      Row = all_outlier_indices
    )
    
    # Add standardized residuals if present
    if (any(all_outlier_indices %in% outlier_indices_resid)) {
      outlier_data$Standardized_Residual = round(standardized_residuals[all_outlier_indices], 3)
    } else {
      outlier_data$Standardized_Residual = NA
    }
    
    # Add leverage if present
    if (any(all_outlier_indices %in% outlier_indices_leverage)) {
      outlier_data$Leverage = round(leverage_values[all_outlier_indices], 3)
    } else {
      outlier_data$Leverage = NA
    }
    
    # Add Cook's distance if present
    if (any(all_outlier_indices %in% outlier_indices_cooks)) {
      outlier_data$Cooks_Distance = round(cooks_d[all_outlier_indices], 3)
      F_values = pf(cooks_d[all_outlier_indices], df1 = k + 1, df2 = n - k - 1)
      outlier_data$F_Value = round(F_values,3)
      
    } else {
      outlier_data$Cooks_Distance = NA
      outlier_data$F_Value = NA # Add F_Value even if no Cook's outliers
    }
    
    # Add the actual data values
    outlier_vars <- data_deleted[all_outlier_indices, c(dep_var, ind_vars), drop = FALSE]
    outlier_data <- cbind(outlier_data, outlier_vars)
  } else {
    outlier_data <- data.frame() # Return an empty data frame if no outliers
  }
  return(outlier_data)
  
}


# 9. Run Outlier Analysis (Combines detection and removal)
run_outlier_analysis <- function(working_data, dep_var, ind_vars, output_dir) {
  
  # 9.1 Open Output File
  output_file <- file.path(output_dir, "outlier_report.txt")
  sink(output_file)
  
  readable_timestamp <- format(with_tz(Sys.time(), "America/Los_Angeles"), "%b %d, %H:%M:%S")
  cat("Outlier Analysis Run:", readable_timestamp, "\n\n")
  
  # 9.2 Detect Outliers
  outlier_data <- detect_outliers(working_data, dep_var, ind_vars)
  
  # 9.3 Display and Handle Outliers (Interactive)
  if (nrow(outlier_data) > 0) { # Check if the data frame is not empty
    cat("\nPotential outliers found based on multiple criteria:\n\n")
    print(outlier_data)
    cat("\n")
    
    # 9.3.1 Get user input for rows to remove
    rows_to_remove_str <- readline(prompt = "Enter the row numbers of outliers to remove (comma-separated, or 'none'): ")
    
    if (tolower(rows_to_remove_str) != "none") {
      rows_to_remove <- as.integer(strsplit(rows_to_remove_str, ",")[[1]])
      
      # 9.3.2 Validate user input
      if (any(is.na(rows_to_remove)) || any(!(rows_to_remove %in% outlier_data$Row)) ) {
        stop("Invalid row numbers entered.  Must be in the list of potential outliers.")
      }
      # 9.3.3 Remove specified outliers
      # Perform listwise deletion first, and then remove outliers.
      working_data_deleted <- perform_listwise_deletion(working_data, dep_var, ind_vars)
      working_data_cleaned <- working_data_deleted[-rows_to_remove, ] # Corrected outlier removal
      
      cat("\nRemoved rows:", paste(rows_to_remove, collapse = ", "), "\n")
    } else {
      working_data_cleaned <- perform_listwise_deletion(working_data, dep_var, ind_vars) # Keep all data (after listwise deletion)
      cat("\nNo outliers removed.\n")
    }
    
  } else {
    # No outliers found by *any* criteria
    cat("No outliers detected based on specified criteria.\n")
    working_data_cleaned <-  perform_listwise_deletion(working_data, dep_var, ind_vars) # No outliers, so cleaned data is after listwise deletion
  }
  
  # 9.4 Save Cleaned Data
  cleaned_data_file <- file.path(output_dir, "cleaned_data.csv")
  write.csv(working_data_cleaned, file = cleaned_data_file, row.names = FALSE)
  cat("\nCleaned data saved to:", cleaned_data_file, "\n")
  
  sink() # 9.5 Close the outlier report file
  message("Outlier analysis complete.  See '", output_file, "' for details and '", cleaned_data_file, "' for the cleaned data.")
  return(working_data_cleaned)
}



# --- Main Script Flow ---

# 10. Load Packages
packages <- c("car", "MASS", "ggplot2", "gridExtra", "here", "lubridate", "lmtest")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 11. Select Data Frame
data_info <- select_dataframe()
working_data <- data_info$data
data_file_name <- data_info$name

# 12. Select Variables
variable_info <- select_variables(working_data)
dep_var <- variable_info$dep_var
ind_vars <- variable_info$ind_vars

# 13. Create Output Directory
output_dir <- create_output_directory()

# 14. Run Outlier Analysis and Get Cleaned Data *FIRST*
cleaned_data <- run_outlier_analysis(working_data, dep_var, ind_vars, output_dir)

# 15. Create Scatterplots (using original data)
create_scatterplots(working_data, dep_var, ind_vars, output_dir)

# 16. Run Regression Analysis (using *CLEANED* data)
working_model <- run_regression_analysis(cleaned_data, dep_var, ind_vars, output_dir) # Use cleaned_data

# Script ends here.  No re-run option.
