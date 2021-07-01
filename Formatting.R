#Main Department Breakdown-----------------------------------------------------
#format target WHpU
breakdown_text[,8] <- round(breakdown_text[,8], digits = 4)
#format time period averages
breakdown_text[,c(9:12,42)] <- round(breakdown_text[,c(9:12,42)], digits = 2)
#format percentages
format_breakdown <- c(13:15,20:22,27:29,45:50)
for(i in 1:length(format_breakdown)){
  for(j in 1:nrow(breakdown_text)){
    breakdown_text[j,format_breakdown[i]] <- 
      round(breakdown_text[j,format_breakdown[i]], digits = 2)
  }
}
for(i in 1:length(format_breakdown)){
  for(j in 1:nrow(breakdown_text)){
    breakdown_text[j,format_breakdown[i]] <- 
      paste0(breakdown_text[j,format_breakdown[i]], "%")
  }
}

#Appendix----------------------------------------------------------------------
#format target WHpU
breakdown_performance_appendix[,8] <- 
  round(breakdown_performance_appendix[,8], digits = 4)
#format time period averages
breakdown_performance_appendix[,c(9:12)] <- 
  round(breakdown_performance_appendix[,c(9:12)], digits = 2)
format_appendix <- seq(from = 13, to = ncol(breakdown_performance_appendix)-2,
                       by = 7)
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

#VP Rollup---------------------------------------------------------------------
#format time period averages
VP_roll_comparison_calc[,c(3:5,9:11,15:17,21:26)] <- 
  round(VP_roll_comparison_calc[,c(3:5,9:11,15:17,21:26)], digits = 2)
#format percentages
format_VP <- c(6:8,12:14,18:20,27:32)
for(i in 1:length(format_VP)){
  for(j in 1:nrow(VP_roll_comparison_calc)){
    VP_roll_comparison_calc[j,format_VP[i]] <- 
      round(VP_roll_comparison_calc[j,format_VP[i]], digits = 2)
  }
}
VP_roll_comparison_calc <- as.data.frame(VP_roll_comparison_calc)
for(i in 1:length(format_VP)){
  for(j in 1:nrow(VP_roll_comparison_calc)){
    VP_roll_comparison_calc[j,format_VP[i]] <- 
      paste0(VP_roll_comparison_calc[j,format_VP[i]], "%")
  }
}
