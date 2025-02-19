library(kableExtra)
library(tidyverse)
library(interactions)
library(broom)
library(ggfortify)
library(effects)

current_time <- Sys.time()
folder_name <- format(current_time, "%y%m%d_%H%M_%S_interactions")
output_path <- file.path("output", folder_name)
if(!dir.exists("output")){
  dir.create("output")
}

dir.create(output_path)

cat("", file = file.path(output_path, "interaction_analysis_results.txt")) # Creates file for text results

# 1 Choose a linear model

# 1a: Get all object names in the global environment
ss_all_object_names <- ls()

# 1b: Get the objects corresponding to those names
ss_all_objects <- mget(ss_all_object_names)

# 1c: Check which objects are linear models
ss_is_lm_vector <- vapply(ss_all_objects, is, logical(1), "lm")

# 1d: Use the logical vector to subset the original list of names
ss_all_lms <- ss_all_object_names[ss_is_lm_vector]

# 1d: Print the list of data frame names
# Check if there are any data frames
if (length(ss_all_lms) == 0) {
  cat("No linear models found.\nTo create a model, use this format: model <- lm(DV ~ IV * MOD, data = DF_name)")
} else {
  cat("Available linear models:\n") # Print a header
  for (i in seq_along(ss_all_lms)) {
    cat(paste(i, ": ", ss_all_lms[i], "\n", sep = ""))
  }
}

# 1e. Prompt the user for input
ss_selection <- readline(prompt = "Enter the number of the linear model you want to use: ")
ss_selection_num <- suppressWarnings(as.integer(ss_selection))
ss_selected_lm_name <- ss_all_lms[ss_selection_num]
ss_selected_lm <- get(ss_selected_lm_name)

# 2 Obtain variable names

# 2a: Make a list of the variables names
ss_selected_lm_formula <- formula(ss_selected_lm)  # Get the model formula
ss_selected_lm_vars <- all.vars(ss_selected_lm_formula) # Extract all variable names

# 2b: Choose the independent variable and the moderator
ss_dv <- ss_selected_lm_vars[1] # The first variable is the DV
ss_iv_and_mod <- ss_selected_lm_vars[-1]  # Remaining variables are IV and moderator

cat("Available variables and moderators:\n")
for (i in seq_along(ss_iv_and_mod)) {
  cat(paste(i, ": ", ss_iv_and_mod[i], "\n", sep = ""))
}

ss_iv_selection <- readline(prompt = "Enter the number of the independent variable: ")
ss_iv_selection_num <- suppressWarnings(as.integer(ss_iv_selection))
ss_selected_iv_name <- ss_iv_and_mod[ss_iv_selection_num] # get the name of the IV
ss_selected_iv_name <- rlang::sym(ss_selected_iv_name) #prepare the variable to be used for sim_slopes

ss_mod_selection <- readline(prompt = "Enter the number of the moderator variable: ")
ss_mod_selection_num <- suppressWarnings(as.integer(ss_mod_selection))
ss_selected_mod_name <- ss_iv_and_mod[ss_mod_selection_num] #get the name of the modifying variable
ss_selected_mod <- model.frame(ss_selected_lm)[[ss_selected_mod_name]] #get the actual modifying variable
ss_selected_mod_name <- rlang::sym(ss_selected_mod_name) # prepare the variable to be used for sim_slopes

# 3 run a simple slopes

# 3a: Choose the number of levels
ss_modlevels_num_selection <- readline(prompt = "Calculate simple slope at this many levels: ")
ss_modlevels_num <- suppressWarnings(as.integer(ss_modlevels_num_selection))

# 3b: Determine the difference between minimum and maximum moderator values
ss_mod_min <- min(ss_selected_mod, na.rm = TRUE) #na.rm to avoid errors
ss_mod_max <- max(ss_selected_mod, na.rm = TRUE) #na.rm to avoid errors
ss_mod_values <- seq(from = ss_mod_min, to = ss_mod_max, length.out = ss_modlevels_num)

# 4a Run the sim_slopes and add it to the .txt
ss_slopes <- sim_slopes(model = ss_selected_lm, pred = !!ss_selected_iv_name, modx = !!ss_selected_mod_name, modx.values = ss_mod_values)
capture.output(
  {
    print(ss_slopes)
  },
  file = file.path(output_path, "interaction_analysis_results.txt"),
  append = TRUE
)
cat("\n", file = file.path(output_path, "interaction_analysis_results.txt"), append = TRUE)

ss_interact_plot <- interact_plot(model = ss_selected_lm, pred = !!ss_selected_iv_name, modx = !!ss_selected_mod_name, modx.values = ss_mod_values)
ggsave(
  filename = file.path(output_path, "interaction_plot.png"),
  plot = ss_interact_plot,
  width = 6,
  height = 4,
  dpi = 300
  )

ss_johnson_neyman <- johnson_neyman(model = ss_selected_lm, pred = !!ss_selected_iv_name, modx = !!ss_selected_mod_name)
ggsave(
  filename = file.path(output_path, "Johnson-Neyman_plot.png"),
  plot = ss_johnson_neyman$plot,
  width = 6,
  height = 4,
  dpi = 300
)

ss_slopes_table <- ss_slopes$slopes #create a table

# Format the table
ss_slopes_table_formatted <- ss_slopes_table %>%
  kable(format = "markdown", digits = 3, caption = "Simple Slopes Analysis")

#capture the table
capture.output(
  {
    print(ss_slopes_table_formatted)
  },
  file = file.path(output_path, "interaction_analysis_results.txt"),
  append = TRUE
)
cat("\n", file = file.path(output_path, "interaction_analysis_results.txt"), append = TRUE)