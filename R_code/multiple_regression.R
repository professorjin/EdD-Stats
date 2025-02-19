library(ggplot2)
library(gridExtra)
library(tidyverse)
library(haven)

# creates output path and file
current_time <- Sys.time()
folder_name <- format(current_time, "%y%m%d_%H%M_%S_multiple_regression", tz = "America/Los_Angeles")
output_path <- file.path("output", folder_name)
if(!dir.exists("output")){
  dir.create("output")
}

dir.create(output_path)

cat("", file = file.path(output_path, "multiple_regression_results.txt")) # Creates file for text results

# This will run the multiple regression

# --- Function Definitions ---

# 1. Select Pre-built Model
# 1a: Get all object names in the global environment
mr_all_object_names <- ls()

# 1b: Get the objects corresponding to those names
mr_all_objects <- mget(mr_all_object_names)

# 1c: Check which objects are linear models
mr_is_lm_vector <- vapply(mr_all_objects, is, logical(1), "lm")

# 1d: Use the logical vector to subset the original list of names
mr_all_lms <- mr_all_object_names[mr_is_lm_vector]

# 1d: Print the list of data frame names
# Check if there are any data frames
if (length(mr_all_lms) == 0) {
  cat("No linear models found.\nTo create a model, use this format: model <- lm(DV ~ IV * MOD, data = DF_name)")
} else {
  cat("Available linear models:\n") # Print a header
  for (i in seq_along(mr_all_lms)) {
    cat(paste(i, ": ", mr_all_lms[i], "\n", sep = ""))
  }
}

# 1e. Prompt the user for input
mr_lm_selection_num <- readline(prompt = "Enter the number of the linear model you want to use: ")
mr_lm_selection_num <- suppressWarnings(as.integer(mr_lm_selection_num))
mr_selected_lm_name <- mr_all_lms[mr_lm_selection_num]
mr_selected_lm <- get(mr_selected_lm_name)
mr_data <- eval(mr_selected_lm$call$data) #extract data frame
mr_data_name <- deparse(mr_selected_lm$call$data) # extract data frame name

mr_lm_formula <- formula(mr_selected_lm) # Extract formula
mr_lm_dv <- all.vars(mr_lm_formula)[1]   # Extract dependent variable
mr_lm_ivs <- all.vars(mr_lm_formula)[-1]  # Extract independent variables

capture.output(
  {
    cat("Model: ", mr_lm_dv, "~", paste(mr_lm_ivs, collapse = " + "), "\n")
    cat("Data frame: ", mr_data_name, "\n")
  },
  file = file.path(output_path, "multiple_regression_results.txt"),
  append = TRUE
)

# 2. Perform Listwise Deletion for Blank Values
mr_formula_vars <- all.vars(formula(mr_selected_lm))  # Extract variables from the model formula
mr_cleaned_data <- na.omit(mr_data[, mr_formula_vars]) # Delete any values that have "NA"
mr_n_rows_deleted <- nrow(mr_data) - nrow(mr_cleaned_data) # Calculate rows deleted
message("Number of rows deleted due to missing values: ", mr_n_rows_deleted, "\n")
mr_selected_lm <- lm(formula(mr_selected_lm), data = mr_cleaned_data)

# 3. Create Scatterplots for Linearity Check (Modified to use model formula)
mr_plot_list <- list()
mr_cleaned_data <- mutate(mr_cleaned_data, across(where(is.labelled), as.numeric))

for (i in mr_lm_ivs) {
  p <- ggplot(mr_cleaned_data, aes(x = .data[[i]], y = (.data[[mr_lm_dv]]))) +
    geom_point() +
    geom_smooth(method = "lm", formula = 'y ~ x', se = TRUE) +
    labs(title = paste("Scatterplot of", i, "vs.", mr_lm_dv),
         x = i, y = mr_lm_dv) +
    theme_bw()
  mr_plot_list[[i]] <- p
}

mr_scatter_plot_grid <- do.call(grid.arrange, c(mr_plot_list, ncol = min(3, length(mr_plot_list))))
ggsave(file.path(output_path, "scatterplots.png"), mr_scatter_plot_grid, width = 12, height = 8, dpi = 300)

# 4. Run Stepwise Regression

cat("\n--- AIC Stepwise Regression (Both Directions) ---\n")
mr_aic_model <- step(mr_selected_lm, direction = "both", trace = 1,
                      scope = list(lower = ~1, upper = mr_lm_formula))
cat("\n--- Final Model Formula (AIC Based) ---\n")
print(formula(mr_aic_model))

# 4.3 BIC Stepwise (Concise Output)
mr_bic_model <- step(mr_selected_lm, direction = "both", trace = 0,
                  scope = list(lower = ~1, upper = mr_lm_formula), k = log(nrow(mr_cleaned_data)))
cat("\n--- Final Model Formula (BIC Based) ---\n")
print(formula(mr_bic_model))