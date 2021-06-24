library(tibble)

#bind output with metric averages from time period portion
VP_roll <- cbind(breakdown_text[,1:5],
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
                   "Key Volume" = "Key.Volume")) %>%
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
  c(paste0("1_",dataElements[1]),
    paste0("1_",dataElements[2]),
    paste0("1_",dataElements[3]),
    paste0("1_",dataElements[4]),
    paste0("1_",dataElements[5]),
    paste0("1_",dataElements[6]),
    paste0("1_",dataElements[7]),
    
    paste0("2_",dataElements[1]),
    paste0("2_",dataElements[2]),
    paste0("2_",dataElements[3]),
    paste0("2_",dataElements[4]),
    paste0("2_",dataElements[5]),
    paste0("2_",dataElements[6]),
    paste0("2_",dataElements[7]))

#join with FYTD averages
VP_roll_comparison <- VP_roll_distribution %>%
  left_join(reportBuilder$FYTD_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key Volume" = "Key.Volume")) %>%
  select(c(1:25,33:43))

#summarize all metrics by Hospital and VP
VP_roll_summarize <- VP_roll_comparison %>%
  group_by(Hospital, VP) %>%
  summarise(across(`Target Worked FTE`:`Actual Labor Expense - FYTD Avg`,
                   ~ sum(., is.na(.), 0)))

VP_roll_comparison_calc <- VP_roll_summarize %>%
  #time period average---------------------------------------------------------
  mutate(`FTE Variance` = `Worked FTE` - `Target Worked FTE`,
         .after = `Worked FTE`) %>%
  mutate(`Productivity Index` = `Target Worked FTE`/`Worked FTE`,
         .after = `FTE Variance`) %>%
  mutate(`OT%` = `OT Hours`/`Paid Hours`,
         .after = `Productivity Index`) %>%
  mutate(`LE Index` = `Target LE`/LE,
         .after = `OT%`) %>%
  select(-`Paid Hours`:-LE) %>%
  #previous distribution-------------------------------------------------------
  # mutate(Target_FTE_diff_1 = `1_Target FTE` - `Target Worked FTE`, 
  #        .after = `1_Target FTE`) %>%
  # mutate(FTE_diff_1 = `1_FTE` - `Worked FTE`, 
  #        .after = `1_FTE`) %>%
  # mutate(FTE_percent_1 = (`1_FTE` - `Worked FTE`)/`Worked FTE`, 
  #        .after = FTE_diff_1) %>%
  mutate(FTE_Var_1 = `1_FTE` - `1_Target FTE`,
         .after = `1_FTE`) %>%
  mutate(PI_1 = `1_Target FTE`/`1_FTE`,
         .after = FTE_Var_1) %>%
  # mutate(PI_diff_1 = PI_1 - `Productivity Index`,
  #        .after = PI_1) %>%
  mutate(OT_percent_1 = `1_Overtime Hours`/`1_Paid Hours`,
         .after = PI_1) %>%
  # mutate(OT_diff_1 = OT_percent_1 - `OT%`,
  #        .after = OT_percent_1) %>%
  mutate(LE_index_1 = `1_Target Labor Expense`/`1_Labor Expense`,
         .after = OT_percent_1) %>%
  # mutate(LE_diff_1 = LE_index_1 - `LE Index`,
  #        .after = LE_index_1) %>%
  select(-`1_Vol`:-`1_Labor Expense`) %>%
  #current distribution--------------------------------------------------------
  # mutate(Target_FTE_diff_2 = `2_Target FTE` - `Target Worked FTE`, 
  #        .after = `2_Target FTE`) %>%
  # mutate(FTE_diff_2 = `2_FTE` - `Worked FTE`, 
  #        .after = `2_FTE`) %>%
  # mutate(FTE_percent_2 = (`2_FTE` - `Worked FTE`)/`Worked FTE`, 
  #        .after = FTE_diff_2) %>%
  mutate(FTE_Var_2 = `2_FTE` - `2_Target FTE`,
         .after = `2_FTE`) %>%
  mutate(PI_2 = `2_Target FTE`/`2_FTE`,
         .after = FTE_Var_2) %>%
  # mutate(PI_diff_2 = PI_2 - `Productivity Index`,
  #        .after = PI_2) %>%
  mutate(OT_percent_2 = `2_Overtime Hours`/`2_Paid Hours`,
         .after = PI_2) %>%
  # mutate(OT_diff_2 = OT_percent_2 - `OT%`,
  #        .after = OT_percent_2) %>%
  mutate(LE_index_2 = `2_Target Labor Expense`/`2_Labor Expense`,
         .after = OT_percent_2) %>%
  # mutate(LE_diff_2 = LE_index_2 - `LE Index`,
  #        .after = LE_index_2) %>%
  select(-`2_Vol`:-`2_Labor Expense`) %>%
  #FYTD Calcs
  mutate(FTE_Var_FYTD = `Actual Worked FTE - FYTD Avg` - 
           `Total Target Worked FTE - FYTD Avg`,
         PI_FYTD = `Total Target Worked FTE - FYTD Avg`/
           `Actual Worked FTE - FYTD Avg`,
         OT_FYTD = `Overtime Hours - FYTD Avg`/
           `Total Paid Hours - FYTD Avg`,
         LEI_FYTD = `Target Labor Expense - FYTD Avg`/
           `Actual Labor Expense - FYTD Avg`) %>%
  select(-`Volume - FYTD Avg`:-`Actual Labor Expense - FYTD Avg`) %>%
  #comparison calcs
  mutate(Target_diff_FYTD = 
           `2_Target FTE` - `Total Target Worked FTE - FYTD Avg`,
         Target_diff_RP = `2_Target FTE` - `1_Target FTE`,
         FTE_diff_FYTD = `2_FTE` - `Actual Worked FTE - FYTD Avg`,
         FTE_diff_RP = `2_FTE` - `1_FTE`,
         FTE_Variance_FYTD = FTE_Var_2 - FTE_Var_FYTD,
         FTE_Variance_RP = FTE_Var_2 - FTE_Var_1,
         PI_diff_FYTD = PI_2 - PI_FYTD,
         PI_diff_RP = PI_2 - PI_1,
         OT_diff_FYTD = OT_percent_2 - OT_FYTD,
         OT_diff_RP = OT_percent_2 - OT_percent_1,
         LE_diff_FYTD = LE_index_2 - LEI_FYTD,
         LE_diff_RP = LE_index_2 - LE_index_1) %>%
  select(-`Total Target Worked FTE - FYTD Avg`:-LEI_FYTD)

#assign an empty vector to notes and bind it to the df
Notes <- vector(mode="character", length = nrow(VP_roll_comparison_calc))
VP_roll_comparison_calc <- cbind(VP_roll_comparison_calc, Notes)

#fix column names
colnames(VP_roll_comparison_calc)[9:33] <- 
  c(#previous distribution-----------------------------------------------------
    paste(dates[previous_distribution_i,1],dataElements[1]),
    # paste0("*",dates[previous_distribution_i,1], " Target FTE Difference"),
    paste(dates[previous_distribution_i,1],dataElements[2]),
    # paste0("*",dates[previous_distribution_i,1], " FTE Difference"),
    # paste0("*",dates[previous_distribution_i,1], " FTE % Change"),
    paste(dates[previous_distribution_i,1], " FTE Variance"),
    paste(dates[previous_distribution_i,1], " Productivity Index"),
    # paste("*",dates[previous_distribution_i,1], " PI Difference"),
    paste(dates[previous_distribution_i,1], " Overtime %"),
    # paste("*",dates[previous_distribution_i,1], " OT Difference"),
    paste(dates[previous_distribution_i,1], " LE Index"),
    # paste("*",dates[previous_distribution_i,1], " LE Index Difference"),
    #current distribution------------------------------------------------------
    paste(dates[distribution_i,1],dataElements[1]),
    # paste0("*",dates[distribution_i,1], " Target FTE Difference"),
    paste(dates[distribution_i,1],dataElements[2]),
    # paste0("*",dates[distribution_i,1], " FTE Difference"),
    # paste0("*",dates[distribution_i,1], " FTE % Change"),
    paste(dates[distribution_i,1], " FTE Variance"),
    paste(dates[distribution_i,1], " Productivity Index"),
    # paste("*",dates[distribution_i,1], " PI Difference"),
    paste(dates[distribution_i,1], " Overtime %"),
    # paste("*",dates[distribution_i,1], " OT Difference"),
    paste(dates[distribution_i,1], " LE Index"),
    # paste("*",dates[distribution_i,1], " LE Index Difference"),
    #Comparison calcs----------------------------------------------------------
    "Target FTE Difference from FYTD",
    "Target FTE Difference from Previous Distribution Period",
    "FTE Difference from FYTD",
    "FTE Difference from Previous Distribution Period",
    "FTE Variance Difference from FYTD",
    "FTE Variance Difference from Previous Distribution Period",
    "Productivity Index % Difference From FYTD",
    "Productivity Index % Difference From Previous Distribution Period",
    "Overtime % Difference From FYTD",
    "Overtime % Difference From Previous Distribution Period",
    "Labor Expense Index % Difference From FYTD",
    "Labor Expense Index % Difference From Previous Distribution Period",
    "Notes")

