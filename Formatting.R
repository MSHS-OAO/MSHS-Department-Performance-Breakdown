
# Creating Custom Functions ------------------------------------------------
percent_formatting <- function(df, col_name, matches = F){
  if(matches == F){
    df <- df %>%
      mutate_at(vars(contains(col_name)), ~ round(.x, digits = 2)) %>%
      mutate_at(vars(contains(col_name)), ~ paste0(.x, "%")) 
  }else if (matches == T){
    df <- df %>%
      mutate_at(vars(ends_with(col_name)), ~ round(.x, digits = 2)) %>%
      mutate_at(vars(ends_with(col_name)), ~ paste0(.x, "%"))   
  }
}

# format percentages in roll list elements
roll_format <- lapply(roll, 
                      function(x) percent_formatting(x, c("%", "Index")))

# format percentages in all variance list elements to prepare appendix
variance_format <- lapply(variance, 
                          function(x) percent_formatting(x, c("%", "Index")))

# format percentages in the comparison calculations df
comparison_calculations_format <- percent_formatting(comparison_calculations, 
                                                     c("%", "Index"))