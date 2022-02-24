# join definitions, previous distribution and current distribution -------
roll <- definitions %>%
  select(Hospital, VP, `Corporate Service Line`, Code, `Key Volume`) %>%
  left_join(variance[[previous_distribution_i]]) %>%
  left_join(variance[[distribution_i]]) %>%
  select(
    -contains("Volume"), # this removes "Key Volume". Only needed for join so is ok
    -contains("Productivity Index"),
    -contains("Overtime %"),
    -contains("LE Index"),
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


# Percentage Calc Function -----------------------------------------------
gen_pct <- function(x, result, numer1, numer2 = NA, denom, dates) {
  if (is.na(numer2)){
    numer_i <- x[, paste(dates[1], numer1)]
    numer_ii <- x[, paste(dates[2], numer1)]
  } else {
    numer_i <- x[, paste(dates[1], numer1)] + x[, paste(dates[1], numer2)]
    numer_ii <- x[, paste(dates[2], numer1)] + x[, paste(dates[2], numer2)]}
  

  x[, paste(dates[1], result)] <-
    ((numer_i / x[, paste(dates[1], denom)]) * 100)
  
  x[, paste(dates[2], result)] <-
    ((numer_i / x[, paste(dates[2], denom)]) * 100)

  return(x)
}


# Percentage Calculation --------------------------------------------------
# for (date in c(previous_distribution, distribution)) {
#   roll_o <- lapply(roll, function(x) {
# 
# 
#     x <- gen_pct(
#       x,
#       result = "Productivity Index",
#       numer1 = "Target FTE",
#       denom = "FTE"
#     )
# 
#     x <- gen_pct(
#       x,
#       result = "Overtime %",
#       numer1 = "Overtime Hours",
#       denom = "Paid Hours"
#     )
# 
#     # Need to add calculation for Labor Expense Index
#     # gen_pct(
#     #   x,
#     #   result = "LE Index",
#     #   numer1 = "Target LE",
#     #   denom = "LE"
#     # )
# 
#     x <- gen_pct(
#       x,
#       result = "Education & Orientation %",
#       numer1 = "Education Hours",
#       numer2 = "Orientation Hours",
#       denom = "Paid Hours"
#     )
#     return(x)
#   })
# }


# Percentage Calculation Nested Apply-------------------------------------------

roll_calc_inputs <- cbind(
  c("Productivity Index", "Target FTE", "FTE", NA),
  # c("Overtime %", ),
  # c(),
  c("Overtime %", "Overtime Hours", "Paid Hours", NA)
)

  roll2 <- lapply(roll, function(x)
    do.call(
      left_join,
      lapply(1:ncol(roll_calc_inputs), function(y)
        gen_pct(x,
                result = roll_calc_inputs[1, y],
                numer1 = roll_calc_inputs[2, y],
                denom = roll_calc_inputs[3, y],
                numer2 = roll_calc_inputs[4, y],
                dates = c(previous_distribution, distribution))
      ))
    )
    
    
    # Need to add calculation for Labor Expense Index
    # gen_pct(
    #   x,
    #   result = "LE Index",
    #   numer1 = "Target LE",
    #   denom = "LE"
    # )
    

#     )
#     return(x)
#   })
# }

# Difference Calc Functions -----------------------------------------------
gen_diff <- function(x, metric, pre, post) {
  x[[paste(metric, "Difference from Previous Distribution Period")]] <-
    (x[[paste(post, metric)]] - x[[paste(pre, metric)]])

  return(x)
}

# gen_diff_pct <- function(x, metric, pre, post) {
#   x[[paste(metric, "% Change from Previous Distribution Period")]] <-
#     (100 * (x[[paste(post, metric)]] - x[[paste(pre, metric)]]) /
#       x[[paste(pre, metric)]])
# 
#   return(x)
# }

# Difference Calculation --------------------------------------------------
roll <- lapply(roll, function(x) {
  prev_d <- previous_distribution
  post_d <- distribution

  # Create a for loop over the metrics?

  x <- gen_diff(
    x,
    metric = "Target FTE",
    pre = prev_d,
    post = post_d
  )

  # Add a % Change for Target FTE?
  # x <- gen_diff_pct(
  #   x,
  #   metric = "Target FTE",
  #   pre = prev_d,
  #   post = post_d
  # )

  x <- gen_diff(
    x,
    metric = "FTE",
    pre = prev_d,
    post = post_d
  )

  # Add a % Change for FTE?
  # x <- gen_diff_pct(
  #   x,
  #   metric = "FTE",
  #   pre = prev_d,
  #   post = post_d
  # )

  x <- gen_diff(
    x,
    metric = "FTE Variance",
    pre = prev_d,
    post = post_d
  )

  x <- gen_diff(
    x,
    metric = "Productivity Index",
    pre = prev_d,
    post = post_d
  )

  x <- gen_diff(
    x,
    metric = "Overtime %",
    pre = prev_d,
    post = post_d
  )

  # Need to add calculation for Labor Expense Index
  # x <- gen_diff(
  #   x,
  #   metric = "Labor Expense Index",
  #   pre = prev_d,
  #   post = post_d
  # )

  return(x)
})

# Finalizing Column Order -------------------------------------------------
roll <- roll %>% lapply(function(x) {
  
  col_field_order <- c(
    "Target FTE", "FTE", "FTE Variance", "Productivity Index", "Overtime %",
    # "LE Index",
    "Total Worked Hours", "Regular Hours", "Overtime Hours", "Education Hours",
    "Orientation Hours", "Agency Hours", "Other Worked Hours",
    "Education & Orientation %"
  )

  col_order <- c(
    "Hospital", colnames(x[2]),
    paste(previous_distribution, col_field_order),
    paste(distribution, col_field_order),
    "Target FTE Difference from Previous Distribution Period",
    # "Target FTE % Change from Previous Distribution Period",
    "FTE Difference from Previous Distribution Period",
    # "FTE % Change from Previous Distribution Period",
    "FTE Variance Difference from Previous Distribution Period",
    "Productivity Index Difference from Previous Distribution Period",
    "Overtime % Difference from Previous Distribution Period"
    # "Overtime % Productivity Index Difference from Previous Distribution Period",
    # "Labor Expense Index % Difference from Previous Distribution Period"
  )

  # sequencing the columns in the desired order.  This also removes
  # unneeded columns.
  x <- x[, col_order]
})

# Creating Notes Column ---------------------------------------------------
roll <- lapply(roll, function(x) {
  x <- x %>% mutate(Notes = "")
  return(x)
})
