#rename columns for roll ups
for(i in 1:length(colnames(variance_roll[[previous_distribution_i]]))){
  colnames(variance_roll[[previous_distribution_i]])[i] <- 
    paste0(colnames(variance_roll[[previous_distribution_i]])[i], "_1")
  colnames(variance_roll[[distribution_i]])[i] <- 
    paste0(colnames(variance_roll[[distribution_i]])[i], "_2")
}

#create base data frame for roll ups
roll_up <- cbind(breakdown_change[,1:3], 
                 variance_roll[[previous_distribution_i]],
                 variance_roll[[distribution_i]]) %>%
  select(-contains("Vol"),
         -contains("Productivity Index"),
         -contains("WHpU"),
         -contains("Education_Orientation"),
         -contains("FTE_Variance"),
         -contains("PI_"),
         -contains("OT_"),
         -contains("LE_Index"),
         -target_1, -target_2)

#create list for both roll ups
roll_up_list <- list(
  vp = roll_up %>% 
    group_by(Hospital, VP) %>%
    summarise(across(Target_FTE_1:Other_Worked_Hours_2,
              ~ sum(., na.rm = T))),
  corporate = roll_up %>% 
    group_by(Hospital, `Corporate Service Line`) %>%
    summarise(across(Target_FTE_1:Other_Worked_Hours_2,
                     ~ sum(., na.rm = T)))
)

#-------------------------Perform necessary calculations
#FTE Variance
roll_up_list <- lapply(roll_up_list, transform,
                   FTE_Var_1 = FTE_1 - Target_FTE_1)
roll_up_list <- lapply(roll_up_list, transform,
                       FTE_Var_2 = FTE_2 - Target_FTE_2)
#Productivity Index
roll_up_list <- lapply(roll_up_list, transform,
                       PI_1 = Target_FTE_1 / FTE_1 * 100)
roll_up_list <- lapply(roll_up_list, transform,
                       PI_2 = Target_FTE_2 / FTE_2 * 100)
#Overtime %
roll_up_list <- lapply(roll_up_list, transform,
                       OT_1 = Overtime_Hours_1 / Paid_Hours_1 * 100)
roll_up_list <- lapply(roll_up_list, transform,
                       OT_2 = Overtime_Hours_2 / Paid_Hours_2 * 100)
#Labor Expense Indes
roll_up_list <- lapply(roll_up_list, transform,
                       LE_Index_1 = Target_LE_1 / LE_1 * 100)
roll_up_list <- lapply(roll_up_list, transform,
                       LE_Index_2 = Target_LE_2 / LE_2 * 100)
#Education Orientation %
roll_up_list <- lapply(roll_up_list, transform,
                       ED_Orientation_1 = 
                         (Education_Hours_1 + Orientation_Hours_1) 
                       / Paid_Hours_1 *100)
roll_up_list <- lapply(roll_up_list, transform,
                       ED_Orientation_2 = 
                         (Education_Hours_2 + Orientation_Hours_2)
                       / Paid_Hours_2 * 100)

##Comparison Calculations
#Target FTE
roll_up_list <- lapply(roll_up_list, transform,
                       target_diff = Target_FTE_2 - Target_FTE_1)
#FTE
roll_up_list <- lapply(roll_up_list, transform,
                       fte_diff = FTE_2 - FTE_1)
#FTE variance
roll_up_list <- lapply(roll_up_list, transform,
                       fte_var_diff = FTE_Var_2 - FTE_Var_1)
#Productivity Index
roll_up_list <- lapply(roll_up_list, transform,
                       PI_diff = PI_2 - PI_1)
#Productivity Index
roll_up_list <- lapply(roll_up_list, transform,
                       OT_diff = OT_2 - OT_1)
#Productivity Index
roll_up_list <- lapply(roll_up_list, transform,
                       LE_diff = LE_Index_2 - LE_Index_1)
#Notes
roll_up_list <- lapply(roll_up_list, transform,
                       Notes = "")


#VP roll_up column selection
roll_up_list$vp <- roll_up_list$vp %>%
  select(Hospital, VP, 
         
         Target_FTE_1, FTE_1, FTE_Var_1, PI_1, OT_1, LE_Index_1,
         Worked_Hours_1, Regular_Hours_1, Overtime_Hours_1, Education_Hours_1,
         Orientation_Hours_1, Agency_Hours_1, Other_Worked_Hours_1, 
         ED_Orientation_1,
         
         Target_FTE_2, FTE_2, FTE_Var_2, PI_2, OT_2, LE_Index_2,
         Worked_Hours_2, Regular_Hours_2, Overtime_Hours_2, Education_Hours_2,
         Orientation_Hours_2, Agency_Hours_2, Other_Worked_Hours_2, 
         ED_Orientation_2,
         
         target_diff, fte_diff, fte_var_diff, PI_diff, OT_diff, LE_diff, Notes)

#Corporate roll_up column selection
roll_up_list$corporate <- roll_up_list$corporate %>%
  select(Hospital, Corporate.Service.Line, 
         
         Target_FTE_1, FTE_1, FTE_Var_1, PI_1, OT_1, LE_Index_1,
         Worked_Hours_1, Regular_Hours_1, Overtime_Hours_1, Education_Hours_1,
         Orientation_Hours_1, Agency_Hours_1, Other_Worked_Hours_1, 
         ED_Orientation_1,
         
         Target_FTE_2, FTE_2, FTE_Var_2, PI_2, OT_2, LE_Index_2,
         Worked_Hours_2, Regular_Hours_2, Overtime_Hours_2, Education_Hours_2,
         Orientation_Hours_2, Agency_Hours_2, Other_Worked_Hours_2, 
         ED_Orientation_2,
         
         target_diff, fte_diff, fte_var_diff, PI_diff, OT_diff, LE_diff, Notes)

column_names <- c(
  "Hospital",
  "place_holder",
  
  paste(dates[previous_distribution_i,1], dataElements[1]),
  paste(dates[previous_distribution_i,1], dataElements[2]),
  paste(dates[previous_distribution_i,1], "FTE Variance"),
  paste(dates[previous_distribution_i,1], "Productivity Index"),
  paste(dates[previous_distribution_i,1], "Overtime %"),
  paste(dates[previous_distribution_i,1], "LE Index"),
  paste(dates[previous_distribution_i,1], dataElements[8]),
  paste(dates[previous_distribution_i,1], dataElements[9]),
  paste(dates[previous_distribution_i,1], dataElements[10]),
  paste(dates[previous_distribution_i,1], dataElements[11]),
  paste(dates[previous_distribution_i,1], dataElements[12]),
  paste(dates[previous_distribution_i,1], dataElements[13]),
  paste(dates[previous_distribution_i,1], dataElements[14]),
  paste(dates[previous_distribution_i,1], dataElements[15]),
  
  paste(dates[distribution_i,1], dataElements[1]),
  paste(dates[distribution_i,1], dataElements[2]),
  paste(dates[distribution_i,1], "FTE Variance"),
  paste(dates[distribution_i,1], "Productivity Index"),
  paste(dates[distribution_i,1], "Overtime %"),
  paste(dates[distribution_i,1], "LE Index"),
  paste(dates[distribution_i,1], dataElements[8]),
  paste(dates[distribution_i,1], dataElements[9]),
  paste(dates[distribution_i,1], dataElements[10]),
  paste(dates[distribution_i,1], dataElements[11]),
  paste(dates[distribution_i,1], dataElements[12]),
  paste(dates[distribution_i,1], dataElements[13]),
  paste(dates[distribution_i,1], dataElements[14]),
  paste(dates[distribution_i,1], dataElements[15]),
  "Target FTE Difference from Previous Distribution Period",
  "FTE Difference from Previous Distribution Period",
  "FTE Variance Difference from Previous Distribution Period",
  "Productivity Index % Difference From Previous Distribution Period",
  "Overtime % Difference From Previous Distribution Period",
  "Labor Expense Index % Difference From Previous Distribution Period",
  "Notes")

#apply column names to both data framse
roll_up_list <- lapply(roll_up_list, setNames, column_names)
colnames(roll_up_list$vp)[2] <- "VP"
colnames(roll_up_list$corporate)[2] <- "Corporate Service Line"
