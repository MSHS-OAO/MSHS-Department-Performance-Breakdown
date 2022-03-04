# join definitions, previous distribution and current distribution -------
roll <- definitions %>%
  select(Hospital, VP, `Corporate Service Line`, Code, `Key Volume`) %>%
  left_join(variance[[previous_distribution_i]]) %>%
  left_join(variance[[distribution_i]]) %>%
  select(
    -contains("Volume"), # this removes "Key Volume". Only needed for join
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

  # This function creates columns that are percentage calculations

  # Alternative is to return the input dataframe with the new columns already
  # appended.

  # error handling for input limitations?

  # make more flexible to handle unlimited numerators?  Not necessary for our
  # application, but could be a useful practice

  # make more flexible to handle unlimited dates within the data frame?j
  # or make it more flexible by only handling a single date, and then
  # another lapply could be added to the calculation to run over any number
  # of dates that would be entered

  if (is.na(numer2)) {
    # numerator for the first date
    numer_i <- x[, paste(dates[1], numer1)]
    # numerator for the 2nd date
    numer_ii <- x[, paste(dates[2], numer1)]
  } else {
    # if there are multiple fields to sum together as the numerator then we
    # add the fields together.  This happens for both dates

    # we could sum these columns together by creating a dummy column first,
    # then delete the dummy column later,
    # so this function doesn't need to be so flexible
    numer_i <- x[, paste(dates[1], numer1)] + x[, paste(dates[1], numer2)]
    numer_ii <- x[, paste(dates[2], numer1)] + x[, paste(dates[2], numer2)]
  }

  x[, paste(dates[1], result)] <-
    ((numer_i / x[, paste(dates[1], denom)]) * 100)

  x[, paste(dates[2], result)] <-
    ((numer_ii / x[, paste(dates[2], denom)]) * 100)

  return(x[, c(paste(dates[1], result), paste(dates[2], result))])
}

# Percentage Calculation Nested Apply-------------------------------------------

# entries for the gen_pct() to be used within the apply() function
roll_calc_inputs <- cbind(
  c("Productivity Index", "Target FTE", "FTE", NA),
  c("LE Index", "Target Labor Expense", "Labor Expense", NA),
  c("Overtime %", "Overtime Hours", "Paid Hours", NA),
  c(
    "Education & Orientation %", "Education Hours", "Paid Hours",
    "Orientation Hours"
  )
)
# would be better to reorder the entry items
# so numerator1 is next to numerator2?


# if using left_join instead of cbind,
# as soon as ncol(roll_calc_inputs) > 2, the function doesn't work

roll <- lapply(roll, function(x) {
  x <- cbind(
    x,
    do.call(
      cbind,
      lapply(seq_len(ncol(roll_calc_inputs)), function(y) {
        gen_pct(x,
          result = roll_calc_inputs[1, y],
          numer1 = roll_calc_inputs[2, y],
          denom = roll_calc_inputs[3, y],
          numer2 = roll_calc_inputs[4, y],
          dates = c(previous_distribution, distribution)
        )
      })
    )
  )
})

# Difference Calc Functions -----------------------------------------------
gen_diff <- function(x, metric, pre, post) {
  # This function creates columns that are difference calculations

  x[[paste(metric, "Difference from Previous Distribution Period")]] <-
    (x[[paste(post, metric)]] - x[[paste(pre, metric)]])

  z <- as.data.frame(
    x[[paste(metric, "Difference from Previous Distribution Period")]]
  )
  colnames(z) <- paste(metric, "Difference from Previous Distribution Period")

  return(z)
}

# gen_diff_pct <- function(x, metric, pre, post) {
#   # This function creates columns that are percent change calculations
#
#   x[[paste(metric, "% Change from Previous Distribution Period")]] <-
#     (100 * (x[[paste(post, metric)]] - x[[paste(pre, metric)]]) /
#       x[[paste(pre, metric)]])
#
#   return(x[[paste(metric, "% Change from Previous Distribution Period")]])
# }

# Difference Calculation --------------------------------------------------

# entries for the gen_pct() to be used within the apply() function
roll_diff_inputs <- cbind(
  c("Target FTE", previous_distribution, distribution),
  c("FTE", previous_distribution, distribution),
  c("FTE Variance", previous_distribution, distribution),
  c("Productivity Index", previous_distribution, distribution),
  c("Overtime %", previous_distribution, distribution),
  c("LE Index", previous_distribution, distribution)
)

roll <- lapply(roll, function(x) {
  x <- cbind(
    x,
    do.call(
      cbind,
      lapply(seq_len(ncol(roll_diff_inputs)), function(y) {
        gen_diff(x,
          metric = roll_diff_inputs[1, y],
          pre = roll_diff_inputs[2, y],
          post = roll_diff_inputs[3, y]
        )
      })
    )
  )
})

# Finalizing Column Order -------------------------------------------------
roll <- roll %>% lapply(function(x) {
  col_field_order <- c(
    "Target FTE", "FTE", "FTE Variance", "Productivity Index", "Overtime %",
    "LE Index", "Total Worked Hours", "Regular Hours", "Overtime Hours",
    "Education Hours", "Orientation Hours", "Agency Hours",
    "Other Worked Hours", "Education & Orientation %"
  )

  diff_text_fields <- c(
    "Target FTE", "FTE", "FTE Variance",
    "Productivity Index", "Overtime %", "LE Index"
  )

  diff_text <- "Difference from Previous Distribution Period"

  col_order <- c(
    "Hospital", colnames(x[2]),
    paste(previous_distribution, col_field_order),
    paste(distribution, col_field_order),
    paste(diff_text_fields, diff_text)

    # if we want to add % Change for some metrics we need to make adjustments.
    # Examples:
    # "Target FTE % Change from Previous Distribution Period"
    # "FTE % Change from Previous Distribution Period"
  )

  # sequencing the columns in the desired order.
  # This also removes unneeded columns.
  x <- x[, col_order]
})

# % columns need to be formatted.  Does this include rounding?
# is this completed in the Formatting.R script?

# Do column titles need to be adjusted slightly?

# Creating Notes Column ---------------------------------------------------
roll <- lapply(roll, function(x) {
  x <- x %>% mutate(Notes = "")
  return(x)
})
