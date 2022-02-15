#left join definitions, previous distribution and current distribution
roll <- definitions %>%
  select(Hospital, VP, `Corporate Service Line`, Code, `Key Volume`) %>%
  left_join(variance[[previous_distribution_i]]) %>%
  left_join(variance[[distribution_i]]) %>%
  select(-contains("Volume"), #this removes "Key Volume". Only needed for join so is ok
         -contains("Productivity Index"),
         -contains("Overtime %"),
         -contains("LE Index"),
         -contains("WHpU"),
         -contains("Education & Orientation %"),
         -contains("Below Target/On Target/Above Target"))
