
# Creating Custom Functions ------------------------------------------------
percent_formatting <- function(df, col_name){
  df <- df %>%
    mutate_at(vars(contains(col_name)), ~ round(.x, digits = 2)) %>%
    mutate_at(vars(contains(col_name)), ~ paste0(.x, "%"))
}

test_df <- percent_formatting(comparison_calculations, c("%", "Index"))

test_list <- lapply(roll, function(x) percent_formatting(x, c("%", "Index")))
