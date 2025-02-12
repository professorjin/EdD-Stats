#This reports the variable code for a SPSS .sav file; e.g., it will tell you that
# for a gender variable, it will tell you that 1=male and 2=female

if (!require(haven)) {
  install.packages("haven")
}
library(haven)
if (!require(here)) {
  install.packages("here")
}
library(here) #For easier file paths

# --- Get file path from user (using file.choose() directly) ---

file_path <- file.choose() # Open file chooser immediately


# --- Check if a file was selected ---
if (is.na(file_path) || length(file_path) == 0) { # User cancelled, or didn't choose.
  stop("No file selected.  Script terminated.")
}


# --- Check if the file exists and has the correct extension ---
if (!file.exists(file_path) || tolower(tools::file_ext(file_path)) != "sav") {
  stop("Invalid file selected. Please select a valid .sav file.")
}


# --- Read the .sav file ---
tryCatch({
  data <- haven::read_sav(file_path)
  
  # --- Get variable labels ---
  variable_labels <- sapply(data, function(x) attr(x, "label"))
  cat("\nVariable Labels:\n")
  print(variable_labels)
  
  # --- Get value labels ---
  cat("\nValue Labels:\n")
  value_labels <- sapply(data, function(x) attr(x, "labels"))
  
  # Print value labels, handling NULLs gracefully
  for (i in seq_along(value_labels)) {
    if (!is.null(value_labels[[i]])) {  # Check if labels exist for this variable
      cat(names(value_labels)[i], ":\n") # Print variable name
      print(value_labels[[i]])
    } else {
      cat(names(value_labels)[i], ": No value labels\n") # Indicate if no labels
    }
  }
  
  
}, error = function(e) {
  message("Error reading the .sav file: ", e$message)
})
