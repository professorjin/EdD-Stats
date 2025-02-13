list_dataframes <- function() {
  # Get names of all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Filter to keep only data frames (and tibbles)
  data_frame_names <- all_objects[sapply(all_objects, function(obj_name) {
    obj <- get(obj_name, envir = .GlobalEnv)
    is.data.frame(obj) || "tbl_df" %in% class(obj)
  })]
  
  # Print the names, one per line
  if (length(data_frame_names) > 0) {
    cat("Available data frames:\n")
    for (name in data_frame_names) {
      cat(name, "\n")
    }
  } else {
    cat("No data frames found in the global environment.\n")
  }
}

# Example usage (optional, but good for testing)
# Create some dummy data frames:
df1 <- data.frame(x = 1:3)
df2 <- data.frame(a = letters[1:3])

# Call the function to list the data frames:
list_dataframes()

# Clean up the example data frames (optional).
rm(df1, df2)