
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

test_df <- percent_formatting(comparison_calculations, c("%", "Index"), matches = F)

test_list <- lapply(roll, function(x) percent_formatting(x, c("%", "Index")))
