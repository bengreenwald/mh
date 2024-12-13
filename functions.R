###################
#### FUNCTIONS ####
###################

# get group means
group_means <- function(df = mh_data, var, outcome = change_phq9_total) {
  
  var_sym <- rlang::ensym(var)
  outcome_sym <- rlang::ensym(outcome)
  
  df %>% 
    group_by(!!var_sym) %>% 
    summarise(mean = mean(!!outcome_sym), .groups = "drop")
  
}

# clean up statistical test results, add sig stars
tidy_results <- function(df) {
  
  df %>% 
    broom::tidy() %>% 
    mutate(
      sig = case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ as.character(NA))
    )

}

# save results csv file
results_csv <- function(df) {
  
  # create a dynamic filename based on the test data
  file_name <- glue::glue("{deparse(substitute(df))}.csv")
  
  # generate the csv file, round numbers more nicely and remove scientific notation
  df %>% 
    mutate(across(where(is.numeric), ~ prettyNum(., scientific = FALSE, digits = 6))) %>%
    write_csv(., here("output", file_name))
  
}