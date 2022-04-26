
# Creating Custom Functions ------------------------------------------------
percent_formatting <- function(df, col_name){
  df <- df %>%
    mutate_at(vars(contains(col_name)), ~ round(.x, digits = 2)) %>%
    mutate_at(vars(contains(col_name)), ~ paste0(.x, "%"))
}
test1 <- percent_formatting(comparison_calculations,c("%", "Index"))


#Main Department Breakdown-----------------------------------------------------
#format target WHpU
breakdown_change[,9] <- round(breakdown_change[,9], digits = 4)
# #format time period averages
# breakdown_change[,c(10:13,42,43)] <- round(breakdown_change[,c(10:13,42,43)], digits = 2)
#format percentages
format_breakdown <- c(14:16,25,31:33,42)
for(i in 1:length(format_breakdown)){
  for(j in 1:nrow(breakdown_change)){
    breakdown_change[j,format_breakdown[i]] <- 
      round(breakdown_change[j,format_breakdown[i]], digits = 2)
  }
}
for(i in 1:length(format_breakdown)){
  for(j in 1:nrow(breakdown_change)){
    breakdown_change[j,format_breakdown[i]] <- 
      paste0(breakdown_change[j,format_breakdown[i]], "%")
  }
}

#Appendix----------------------------------------------------------------------
#format target WHpU
breakdown_performance_appendix[,9] <- 
  round(breakdown_performance_appendix[,9], digits = 4)
# #format time period averages
# breakdown_performance_appendix[,c(10:13)] <-
#   round(breakdown_performance_appendix[,c(10:13)], digits = 2)
format_appendix <- seq(from = 14, to = ncol(breakdown_performance_appendix)-2,
                       by = 17)
for(i in 1:length(format_appendix)){
  for(j in 1:nrow(breakdown_performance_appendix)){
    breakdown_performance_appendix[j, format_appendix[i]] <- 
      round(breakdown_performance_appendix[j, format_appendix[i]], digits = 2)
    breakdown_performance_appendix[j, format_appendix[i]+1] <- 
      round(breakdown_performance_appendix[j, format_appendix[i]+1], digits = 2)
    breakdown_performance_appendix[j, format_appendix[i]+2] <- 
      round(breakdown_performance_appendix[j, format_appendix[i]+2], digits = 2)
  }
}
for(i in 1:length(format_appendix)){
  for(j in 1:nrow(breakdown_performance_appendix)){
    breakdown_performance_appendix[j, format_appendix[i]] <- 
     paste0(breakdown_performance_appendix[j, format_appendix[i]], "%")
    breakdown_performance_appendix[j, format_appendix[i]+1] <- 
      paste0(breakdown_performance_appendix[j, format_appendix[i]+1], "%")
    breakdown_performance_appendix[j, format_appendix[i]+2] <- 
      paste0(breakdown_performance_appendix[j, format_appendix[i]+2], "%")
  }
}

#Rollup---------------------------------------------------------------------
# #format time period averages
# VP_roll_comparison_calc[,c(3:5,9:11,15:17,21:26)] <- 
#   round(VP_roll_comparison_calc[,c(3:5,9:11,15:17,21:26)], digits = 2)
#format VP roll up
format_roll_up <- c(6:8,16,20:22,30,34:36)
for(i in 1:length(format_roll_up)){
  for(j in 1:nrow(roll_up_list[[1]])){
    roll_up_list[[1]][j,format_roll_up[i]] <- 
      round(roll_up_list[[1]][j,format_roll_up[i]], digits = 2)
  }
}
roll_up_list[[1]] <- as.data.frame(roll_up_list[[1]])
for(i in 1:length(format_roll_up)){
  for(j in 1:nrow(roll_up_list[[1]])){
    roll_up_list[[1]][j,format_roll_up[i]] <- 
      paste0(roll_up_list[[1]][j,format_roll_up[i]], "%")
  }
}
#format corporate roll up
for(i in 1:length(format_roll_up)){
  for(j in 1:nrow(roll_up_list[[2]])){
    roll_up_list[[2]][j,format_roll_up[i]] <- 
      round(roll_up_list[[2]][j,format_roll_up[i]], digits = 2)
  }
}
roll_up_list[[2]] <- as.data.frame(roll_up_list[[2]])
for(i in 1:length(format_roll_up)){
  for(j in 1:nrow(roll_up_list[[2]])){
    roll_up_list[[2]][j,format_roll_up[i]] <- 
      paste0(roll_up_list[[2]][j,format_roll_up[i]], "%")
  }
}
