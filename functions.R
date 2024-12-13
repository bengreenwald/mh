###################
#### FUNCTIONS ####
###################

# get group means
group_means <- function(df = mh_data, var, outcome = change_phq9_total) {
  
  # for dynamic variables 
  var_sym <- rlang::ensym(var)
  outcome_sym <- rlang::ensym(outcome)
  
  # simple aggregation by selected variable
  df %>% 
    group_by(!!var_sym) %>% 
    summarise(mean = mean(!!outcome_sym), .groups = "drop")
  
}

# clean up statistical test results, add significant stars
tidy_results <- function(df) {
  
  df %>% 
    # tidy up values into tables
    broom::tidy() %>% 
    # add significant stars
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
  
  # rounded numbers + remove scientific notation, generate the csv file
  df %>% 
    mutate(across(where(is.numeric), ~ prettyNum(., scientific = FALSE, digits = 6))) %>%
    write_csv(., here("output", file_name))
  
}