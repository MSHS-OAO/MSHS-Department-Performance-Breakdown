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

#VP roll_up column selection
VP_roll <- roll_up_list$vp %>%
  select(Hospital, VP, 
         
         Target_FTE_1, FTE_1, FTE_Var_1, PI_1, OT_1, LE_Index_1,
         Worked_Hours_1, Regular_Hours_1, Overtime_Hours_1, Education_Hours_1,
         Orientation_Hours_1, Agency_Hours_1, Other_Worked_Hours_1, 
         ED_Orientation_1,
         
         Target_FTE_2, FTE_2, FTE_Var_2, PI_2, OT_2, LE_Index_2,
         Worked_Hours_2, Regular_Hours_2, Overtime_Hours_2, Education_Hours_2,
         Orientation_Hours_2, Agency_Hours_2, Other_Worked_Hours_2, 
         ED_Orientation_2)

#Corporate roll_up column selection
Corporate_roll <- roll_up_list$corporate %>%
  select(Hospital, Corporate.Service.Line, 
         
         Target_FTE_1, FTE_1, FTE_Var_1, PI_1, OT_1, LE_Index_1,
         Worked_Hours_1, Regular_Hours_1, Overtime_Hours_1, Education_Hours_1,
         Orientation_Hours_1, Agency_Hours_1, Other_Worked_Hours_1, 
         ED_Orientation_1,
         
         Target_FTE_2, FTE_2, FTE_Var_2, PI_2, OT_2, LE_Index_2,
         Worked_Hours_2, Regular_Hours_2, Overtime_Hours_2, Education_Hours_2,
         Orientation_Hours_2, Agency_Hours_2, Other_Worked_Hours_2, 
         ED_Orientation_2)

