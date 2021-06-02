#bind output with metric averages from time period portion
VP_roll <- cbind(output_index[,1:5],
                 time_period[[1]][,27], time_period[[2]][,27], 
                 time_period[[4]][,27], time_period[[5]][,27], 
                 time_period[[6]][,27], time_period[[7]][,27])
#set column names for the metric averages
colnames(VP_roll)[c(6:11)] <- c("Target Worked FTE","Worked FTE","Paid Hours", 
                                "OT Hours", "Target LE", "LE")
#join with department performance and remove unneccessary columns
VP_roll_performance <- VP_roll %>%
  left_join(reportBuilder$department_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key.Volume" = "Key.Volume")) %>%
  select(c(-12:-18))
#establish VP metric index
VP_numbers <- seq(from = 12, to = ncol(VP_roll_performance), by = 7)
#select current and previous distribution metric columns
VP_roll_distribution <- VP_roll_performance %>%
  select(
    c(1:11,
      VP_numbers[previous_distribution_i]:
        (VP_numbers[previous_distribution_i]+6),
      VP_numbers[distribution_i]:
        (VP_numbers[distribution_i]+6)))
#assign column names to selected distribution periods
colnames(VP_roll_distribution)[12:ncol(VP_roll_distribution)] <- 
  c(paste(dates[previous_distribution_i,1],dataElements[1]),
    paste(dates[previous_distribution_i,1],dataElements[2]),
    paste(dates[previous_distribution_i,1],dataElements[3]),
    paste(dates[previous_distribution_i,1],dataElements[4]),
    paste(dates[previous_distribution_i,1],dataElements[5]),
    paste(dates[previous_distribution_i,1],dataElements[6]),
    paste(dates[previous_distribution_i,1],dataElements[7]),
    
    paste(dates[distribution_i,1],dataElements[1]),
    paste(dates[distribution_i,1],dataElements[2]),
    paste(dates[distribution_i,1],dataElements[3]),
    paste(dates[distribution_i,1],dataElements[4]),
    paste(dates[distribution_i,1],dataElements[5]),
    paste(dates[distribution_i,1],dataElements[6]),
    paste(dates[distribution_i,1],dataElements[7]))

VP_roll_comparison <- VP_roll_distribution %>%
  left_join(reportBuilder$FYTD_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key.Volume" = "Key.Volume")) %>%
  select(c(1:25,33:42))