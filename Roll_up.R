library(purrr)

# join definitions, previous distribution and current distribution -------
roll <- definitions %>%
  select(Hospital, VP, `Corporate Service Line`, Code, `Key Volume`) %>%
  left_join(variance[[previous_distribution_i]]) %>%
  left_join(variance[[distribution_i]]) %>%
  select(
    -contains("Volume"), # this removes "Key Volume". Only needed for join
    -contains("Productivity Index"),
    -contains("Overtime %"),
    -contains("Labor Expense Index"),
    -contains("WHpU"),
    -contains("Education & Orientation %"),
    -contains("Below Target/On Target/Above Target")
  )

# create list made of both roll ups --------------------------------------
roll <- list(
  vp = roll %>%
    group_by(Hospital, VP) %>%
    summarise(across(where(is.numeric), sum, na.rm = T)),
  corporate = roll %>%
    group_by(Hospital, `Corporate Service Line`) %>%
    summarise(across(where(is.numeric), sum, na.rm = T))
)

dates <- c(previous_distribution, distribution)

# summing education and orientation hours together to be used for
# a future calculation
roll <- roll %>%
  lapply(function(x)
    reduce(  
      lapply(seq_len(length(dates)), function(y) {
        
        x[, paste(dates[y], "Edu Orientation Hrs")] <-
          (x[, paste(dates[y], "Education Hours")] +
             x[, paste(dates[y], "Orientation Hours")])
        
        return(x)}
      ),
      left_join
    )
  )

# Percentage Calc Function -----------------------------------------------
gen_pct <- function(x, result, numer, denom, date) {
  
  # This function creates a column that is a percentage calculation
  # and appends it to the existing dataframe
  
  x[, paste(date, result)] <-
    (100 * (x[, paste(date, numer)] /
             x[, paste(date, denom)]))
  
  return(x)
}

# Percentage Calculation Nested Apply-------------------------------------------

# entries for the gen_pct() to be used within the apply() function
roll_calc_inputs <- cbind(
  Prod_index = c("Productivity Index", "Target FTE", "FTE"),
  LE_Index = c("Labor Expense Index", "Target Labor Expense", "Labor Expense"),
  OT_pct = c("Overtime %", "Overtime Hours", "Paid Hours"),
  EDU_Ort_pct = c("Education & Orientation %", "Edu Orientation Hrs", "Paid Hours")
)

roll <-
  lapply(roll, function(x)
    reduce(
      lapply(seq_len(ncol(roll_calc_inputs)), function(y) 
        reduce(
          lapply(seq_len(length(dates)), function(z)
            gen_pct(x,
                     result = roll_calc_inputs[1, y],
                     numer = roll_calc_inputs[2, y],
                     denom = roll_calc_inputs[3, y],
                     date = dates[z]
            )
          ),
          left_join
        )
      ),
      left_join
    )
  )

# Difference Calc Functions -----------------------------------------------
gen_diff <- function(x, metric, pre, post) {
  # This function creates columns that are difference calculations

  x[[paste0(metric, ": Difference from Previous Distribution Period")]] <-
    (x[[paste(post, metric)]] - x[[paste(pre, metric)]])

  return(x)
}

# Difference Calculation --------------------------------------------------

# entries for the gen_pct() to be used within the apply() function
roll_diff_inputs <- cbind(
  c("Target FTE", previous_distribution, distribution),
  c("FTE", previous_distribution, distribution),
  c("FTE Variance", previous_distribution, distribution),
  c("Productivity Index", previous_distribution, distribution),
  c("Overtime %", previous_distribution, distribution),
  c("Labor Expense Index", previous_distribution, distribution)
)

roll <- lapply(roll, function(x)
  reduce(
    lapply(seq_len(ncol(roll_diff_inputs)), function(y) 
      gen_diff(x,
               metric = roll_diff_inputs[1, y],
               pre = roll_diff_inputs[2, y],
               post = roll_diff_inputs[3, y]
      )
    ),
    left_join
  )
)

# Finalizing Column Order -------------------------------------------------
roll <- roll %>% lapply(function(x) {
  col_field_order <- c(
    "Target FTE", "FTE", "FTE Variance", "Productivity Index", "Overtime %",
    "Labor Expense Index", "Total Worked Hours", "Regular Hours", "Overtime Hours",
    "Education Hours", "Orientation Hours", "Agency Hours",
    "Other Worked Hours", "Education & Orientation %"
  )

  diff_text_fields <- c(
    "Target FTE", "FTE", "FTE Variance",
    "Productivity Index", "Overtime %", "Labor Expense Index"
  )

  diff_text <- ": Difference from Previous Distribution Period"

  col_order <- c(
    # the column index is dependent on the column order in the group_by()
    # and summarise() functions at the top of this script
    "Hospital", colnames(x)[2],
    paste(previous_distribution, col_field_order),
    paste(distribution, col_field_order),
    paste0(diff_text_fields, diff_text)

  )

  # sequencing the columns in the desired order.
  # This also removes unneeded columns.
  x <- x[, col_order]
  
  x <- x %>% mutate(Notes = "")
})


# Do column titles need to be adjusted slightly?
