# 1 Choose a data frame

# 1a: Get all object names in the global environment
bm_all_object_names <- ls()

# 1b: Get the objects corresponding to those names
bm_all_objects <- mget(bm_all_object_names)

# 1c: Check which objects are data frames (apply is.data.frame to each)
bm_is_dataframe_vector <- sapply(bm_all_objects, is.data.frame)

# 1d: Use the logical vector to subset the original list of names
bm_all_dataframes <- bm_all_object_names[bm_is_dataframe_vector]

# 1d: Print the list of data frame names
# Check if there are any data frames
if (length(bm_all_dataframes) == 0) {
  cat("No data frames found.\n")
} else {
  cat("Available data frames:\n") # Print a header
  for (i in seq_along(bm_all_dataframes)) {
    cat(paste(i, ": ", bm_all_dataframes[i], "\n", sep = ""))
  }
}

# 1e. Prompt the user for input
bm_selection <- readline(prompt = "Enter the number of the data frame you want to use: ")
bm_selection_num <- suppressWarnings(as.integer(bm_selection))
bm_selected_df_name <- bm_all_dataframes[bm_selection_num]
bm_selected_df <- get(bm_selected_df_name)

# 2 Choose variables

# 2a: Get a list of variables
bm_variable_names <- names(bm_selected_df)

# 2b: Print the list of variables
if (length(bm_variable_names) == 0) {
  cat("No variables found")
} else {
  cat("\nList of Variables\n")
  for (i in seq_along(bm_variable_names)) {
    var_name <- bm_variable_names[i]
    var_label <- attr(bm_selected_df[[var_name]], "label")
    if (is.null(var_label)){
      var_label <- ""
    }
    cat(paste(i, ": ", var_name, " (", var_label, ")\n", sep = ""))
  }
}

# 2c: Choose a dependent variable
cat("Choose a dependent variable")
bm_DVselection <- readline(prompt = "Enter the number of the dependent variable: ")
bm_DVselection_num <- suppressWarnings(as.integer(bm_DVselection))
bm_selected_DV_name <- bm_variable_names[bm_DVselection_num]

# 2d: Choose an independent variable
bm_IVselection <- readline(prompt = "Enter the number of the independent variable(s), comma separated (e.g., 1,3,4): ")
bm_IVselection_nums <- suppressWarnings(as.integer(strsplit(bm_IVselection, ",")[[1]]))
bm_selected_IV_names <- bm_variable_names[bm_IVselection_nums]

# 3 Build the model
bm_formula <- as.formula(paste(bm_selected_DV_name, "~", paste(bm_selected_IV_names, collapse = " + ")))
bm_model <- lm(formula = bm_formula, data = eval(parse(text = bm_selected_df_name)))
cat("Formula:", format(bm_formula), "\n")

rm(bm_all_dataframes)
rm(bm_all_object_names)
rm(bm_all_objects)
rm(bm_is_dataframe_vector)
rm(bm_selection)
rm(bm_selection_num)
rm(bm_selected_df_name)
rm(bm_DVselection)
rm(bm_DVselection_num)
rm(bm_IVselection)
rm(bm_IVselection_nums)
rm(bm_variable_names)
rm(i)
rm(bm_selected_df)
rm(var_label)
rm(var_name)