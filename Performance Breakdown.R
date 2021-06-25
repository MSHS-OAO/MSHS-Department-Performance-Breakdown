library(dplyr)
library(tidyr)
library(readxl)
library(rlist)
library(stringr)
library(here)

#Establish Constants-----------------------------------------------------------
#Site(s) user would like to produce department breakdown for
# enter "MSHS" for all sites
output_site <- c("MSHS")

#define current and previous distribution "mm/dd/yyyy"
distribution <- "04/24/2021"
previous_distribution <- "03/27/2021"

#define percentage threshold for what is considered upward/downward change
threshold <- 1.5

#Read in Files-----------------------------------------------------------------
dir_breakdown <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Analysis/MSHS Department Breakdown/")

#Read in end dates file for column headers
dates <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Universal Data/Mapping/",
                          "MSHS_Pay_Cycle.xlsx")) %>%
  select(END.DATE) %>%
  mutate(END.DATE = as.Date(END.DATE)) %>%
  filter(END.DATE >= as.Date("05/23/2020", format = "%m/%d/%Y")) %>%
  mutate(END.DATE = paste0(
    substr(END.DATE, 6, 7), "/",
    substr(END.DATE, 9, 10), "/",
    substr(END.DATE, 1, 4))) %>%
  distinct()

#Reporting definitions included in all hospital admin rollup reports
definitions <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/",
                                "MSHS Productivity/Productivity/",
                                "Universal Data/Mapping/",
                                "MSHS_Reporting_Definition_Mapping.xlsx")) %>%
  filter(!is.na(KEY.VOLUME), DEPARTMENT.BREAKDOWN == 1) %>%
  select(SITE, VP, DEFINITION.CODE, DEFINITION.NAME, KEY.VOLUME) %>%
  distinct()
colnames(definitions) <- c("Hospital", "VP", "Code", "Name", "Key Volume")

#labor standards for target information
laborStandards <- read.csv(paste0(dir_breakdown,
                                  "Labor Standards/", "LaborStandards.csv"),
                           header = F,colClasses = c(rep("character", 7),
                                                     rep("numeric", 7)))
#change column headers for labor standards
colnames(laborStandards) <- c("Partner", "Hospital", "Code", "EffDate", "VolID",
                              "DepID", "Standard Type", "Target WHpU", "LEpU", 
                              "WHpU2", "LEpU2", "PHpU", "MinStaff", "FixStaff", 
                              "KeyVol")
#filter on key volume and turn effective date into date format
laborStandards <- laborStandards %>% 
  select(-16:-21) %>%
  filter(KeyVol == "Y") %>%
  mutate(EffDate = 
           paste0(
             substr(EffDate,1,2), "/",
             substr(EffDate,3,4), "/",
             substr(EffDate,5,8)),
         EffDate = as.Date(EffDate, format = "%m/%d/%Y")) 

#list for baseline, productivity performance and productivity index reports
reportBuilder <- list()
#read baseline performance report (Time Period Performance)
reportBuilder[[1]] <- read.csv(paste0(dir_breakdown,
                                      "Report Builder/Time Period Performance/",
                                      "Time Period Performance.csv"),
                               as.is = T)
#read productivity performance report (Department Performance Breakdown)
reportBuilder[[2]] <- read.csv(paste0(dir_breakdown,
                                      "Report Builder/",
                                      "Department Performance Breakdown/",
                                      "Report Builder.csv"),
                               as.is = T)
#Read productivity index report (Productivity Index Report)
reportBuilder[[3]] <- read.csv(paste0(dir_breakdown,
                                      "Report Builder/",
                                      "Productivity Index Performance/",
                                      "Productivity.csv"),
                               as.is = T)
#Read FYTD report (FYTD Performance Report)
reportBuilder[[4]] <- read.csv(paste0(dir_breakdown, 
                                      "Report Builder/FYTD Performance/",
                                      "FYTD Performance.csv"),
                               as.is = T)

#Format Files------------------------------------------------------------------
#renaming columns
rb_fytd_colnames <- c(colnames(reportBuilder[[4]])[1:9],
                      paste(reportBuilder[[4]][1, 10:ncol(reportBuilder[[4]])]))
colnames(reportBuilder[[4]]) <- rb_fytd_colnames
#function to format numeric column by column name "col_name" to remove
#text characters in the data table "df"
format_numbers <- function(df, col_name){
  df[2:nrow(df), col_name] <- df[2:nrow(df), col_name] %>%
    str_replace_all(c("," = "", " " = "", "\\$" = "", "%" = ""))
}
#function to apply numeric formatting function to each "x" data table 
#in the list "lt"
format_list <- function(lt, x){
  lt[[x]][2:nrow(lt[[x]]), 10:ncol(lt[[x]])] <- 
    sapply(colnames(lt[[x]])[10:ncol(lt[[x]])],
           function(y) format_numbers(lt[[x]], y))
  lt[[x]] <- lt[[x]] %>% slice(-1)
  lt[[x]][, 10:ncol(lt[[x]])] <- sapply(lt[[x]][, 10:ncol(lt[[x]])],
                                        function(x) as.numeric(x))
  return(lt[[x]])
}
#using functions to format numeric columns in all data tables in the list
reportBuilder <- sapply(1:length(reportBuilder), 
                        function(x) format_list(reportBuilder, x))
#apply names to each list element
names(reportBuilder) <- c("time_period_performance", "department_performance",
                          "productivity_index", "FYTD_performance")

#Calculations------------------------------------------------------------------
#calculate date index for distribution and previous distribution
for(i in 1:nrow(dates)){
  if(dates[i,1] == distribution){
    distribution_i <- i
  } else if(dates[i,1] == previous_distribution){
    previous_distribution_i <- i
  }
}

#Labor Standards---------------------------------------------------------------
#join labor standards and baseline performance to definitions table
breakdown_targets <- 
  left_join(definitions, laborStandards, by = c("Code" = "Code")) %>%
  select(Hospital.x, VP, Code, Name, `Key Volume`,
         EffDate, `Standard Type`, `Target WHpU`) %>%
#Baseline Time Period Performance----------------------------------------------  
  left_join(reportBuilder$time_period_performance, 
            by = c("Code" = "Department.Reporting.Definition.ID", 
                   "Key Volume" = "Key.Volume")) 
#take necessary columns
breakdown_targets <- 
  as.data.frame(breakdown_targets[,c(1:8,16:ncol(breakdown_targets))])
#create list for time period averages
time_period <- list()
#place 26 time periods of each metric into each list object. 7 list objects
cadence <- seq(from = 9, to = ncol(breakdown_targets), by = 7)
for(i in 0:6){
  time_period[[i+1]] <- breakdown_targets[,cadence + i]
}
#calculate 26 time period avg for each metric
time_period <- lapply(time_period, function(x) {
  data.frame(x) %>%
    mutate(Avg = round(rowMeans(x, na.rm = T), digits = 4))})
#cbind metric averages
breakdown_time_period <- cbind(
  breakdown_targets[,1:8], time_period[[1]][,27], time_period[[2]][,27],
  time_period[[3]][,27], time_period[[4]][,27], time_period[[5]][,27], 
  time_period[[6]][,27], time_period[[7]][,27])
#Assign column names
colnames(breakdown_time_period)[c(1,5,9:15)] <- c("Hospital", "Key Volume",
                                                  "Target Worked FTE", 
                                                  "Worked FTE", "Volume",
                                                  "Paid Hours", "OT Hours", 
                                                  "Target LE", "LE")
#calculate PI, OT% and LE Index
breakdown_time_period <- breakdown_time_period %>%
  mutate(`Productivity Index` = (`Target Worked FTE`/`Worked FTE`) * 100,
         `OT%` = (`OT Hours`/`Paid Hours`) * 100,
         `LE Index` = (`Target LE`/LE) * 100) %>%
  mutate(`FTE Variance` = `Worked FTE` - `Target Worked FTE`,
         .after = `Worked FTE`) %>%
#select necessary columns
  select(Hospital, VP, Code, Name, `Key Volume`, `Standard Type`, EffDate,
         `Target WHpU`, `Target Worked FTE`, `Worked FTE`, `FTE Variance`,
         Volume, `Productivity Index`, `OT%`, `LE Index`) %>%
  
#Reporting Period Performance--------------------------------------------------
  #join reporting period performance table
  left_join(reportBuilder$department_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key Volume" = "Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("Target FTE", "FTE", "Vol", "Paid Hours", "Overtime Hours", 
                  "Target Labor Expense", "Labor Expense")
#Assign column names based on dates and data elements
for(i in seq(from = 23, to = ncol(breakdown_time_period), by = 7)){
  numbers <- seq(from = 23, to = ncol(breakdown_time_period), by = 7)
  for(j in 1:length(numbers)){
    if(numbers[j] == i){
      k = j
    }
  }
  colnames(breakdown_time_period)[i] <- paste(dates[k,1], dataElements[1])
  colnames(breakdown_time_period)[i+1] <- paste(dates[k,1], dataElements[2])
  colnames(breakdown_time_period)[i+2] <- paste(dates[k,1], dataElements[3])
  colnames(breakdown_time_period)[i+3] <- paste(dates[k,1], dataElements[4])
  colnames(breakdown_time_period)[i+4] <- paste(dates[k,1], dataElements[5])
  colnames(breakdown_time_period)[i+5] <- paste(dates[k,1], dataElements[6])
  colnames(breakdown_time_period)[i+6] <- paste(dates[k,1], dataElements[7])
}
#take necessary columns
breakdown_performance <- 
  breakdown_time_period[,c(1:15,23:ncol(breakdown_time_period))]
#create list for reporting period variance calculations
variance <- list()
#list element for baseline and reporting period stats for all reporting periods
index_sequence <- seq(from = 16, to = ncol(breakdown_performance)-6, by = 7)
for(i in 1:length(index_sequence)){
  variance[[i]] <- cbind(
    breakdown_performance[,8:15],
    breakdown_performance[,index_sequence[i]:(index_sequence[i]+6)])
}
#save column names in seperate list
columns <- list()
for(i in 1:length(variance)){
  columns[[i]] <- colnames(variance[[i]])
  colnames(variance[[i]]) <- c("Target_WHpU", "Baseline_Target_FTE",
                               "Baseline_FTE", "Baselint_FTE_Var",
                               "Baseline_Vol", "Baseline_PI", "Baseline_OT",
                               "Baseline_LEI", "Target_FTE","FTE", "Vol",
                               "Paid_Hours", "OT_Hours", "Target_LE", "LE")
}
#calculate reporting period Target FTE difference to baseline Target FTE
variance <- lapply(variance, transform, 
                   FTE_Variance  = FTE - Target_FTE)
# #calculate reporting period Target FTE difference to baseline Target FTE
# variance <- lapply(variance, transform, 
#                    Target_FTE_Var  = Target_FTE - Baseline_Target_FTE)
# #calculate reporting period FTE difference to baseline FTE
# variance <- lapply(variance, transform, 
#                    FTE_Var  = FTE - Baseline_FTE)
# #calculate FTE % change to baseline FTE
# variance <- lapply(variance, transform, 
#                    Var_Percent = FTE_Var/Baseline_FTE * 100)
# #calculate reporting period volume % change to baseline volume
# variance <- lapply(variance, transform, 
#                    Vol_Percent = (((Vol - Baseline_Vol)/Baseline_Vol) * 100))
#calculate reporting period PI
variance <- lapply(variance, transform,
                   PI = Target_FTE/FTE * 100)
# #calculate reporting period PI difference to baseline PI
# variance <- lapply(variance, transform,
#                    PI_Change = PI - Baseline_PI)
#calculate reporting period OT%
variance <- lapply(variance, transform,
                   OT = OT_Hours/Paid_Hours * 100)
# #calculate reporting period OT% difference to baseline OT%
# variance <- lapply(variance, transform,
#                    OT_Change = OT - Baseline_OT)
#calculate reporting period LE Index
variance <- lapply(variance, transform,
                   LE_Index = Target_LE/LE * 100)
# #calculate reporting period LE index difference to baseline LE index
# variance <- lapply(variance, transform,
#                    LE_Change = LE_Index - Baseline_LEI)
#rearange variance list elements and replace column names
for(i in 1:length(variance)){
  variance[[i]] <- variance[[i]][,c(9,10,16,11,17,18,19)]
  colnames(variance[[i]]) <- c(
    columns[[i]][9],
    # paste0("*",dates[i,1], " Target FTE Difference"),
    columns[[i]][10],
    # paste0("*",dates[i,1], " FTE Difference"),
    # paste0("*",dates[i,1], " FTE % Change"),
    paste0(dates[i,1], "FTE Variance"),
    columns[[i]][11],
    # paste0("*",dates[i,1], " Volume % Change"),
    paste0(dates[i,1], " Productivity Index"),
    # paste0("*",dates[i,1], " PI Difference"),
    paste0(dates[i,1], " Overtime %"),
    # paste0("*",dates[i,1], " OT Difference"),
    paste0(dates[i,1], " LE Index") 
    # paste0("*",dates[i,1], " LE Index Difference")
    )
}
#bind columns from breakdown_performance with variance list to create appendix
breakdown_performance_appendix <- cbind(
  breakdown_performance[,1:15],
  list.cbind(variance))
#select only previous and current distribution for main deliverable
breakdown_performance <- cbind(
  breakdown_performance[,1:15],
  variance[[previous_distribution_i]],
  variance[[distribution_i]])

#Watchlist Criteria------------------------------------------------------------
#Select past 6 time period productivity index and calculate average
Prod <- reportBuilder$productivity_index[,] %>%
  select(seq(from = ncol(reportBuilder$productivity_index) - 35, 
             to = ncol(reportBuilder$productivity_index) - 5, 
             by = 6)) %>%
  mutate(Prod = round(rowMeans(.), digits = 0))
#Select past 6 time period FTE Variances and calculate average
Var <- reportBuilder$productivity_index[,] %>%
  select(seq(from = ncol(reportBuilder$productivity_index) - 34, 
             to = ncol(reportBuilder$productivity_index) - 4, 
             by = 6)) %>%
  mutate(Prod = round(rowMeans(.), digits = 0))
#bind 6 time period averages to productivity_index list element
reportBuilder$productivity_index <- 
  cbind(
    reportBuilder$productivity_index[
      ,c(5,7,(ncol(reportBuilder$productivity_index) - 5):
           ncol(reportBuilder$productivity_index))], 
    Prod[,7], 
    Var[,7])
#logic for determining watchlist criteria
reportBuilder$productivity_index$Watchlist <- NA
for(i in 2:nrow(reportBuilder$productivity_index)){
  if(is.na(reportBuilder$productivity_index[i,9]) |
     is.na(reportBuilder$productivity_index[i,10])){
    reportBuilder$productivity_index$Watchlist[i] <- "Missing Data"
  } else if((reportBuilder$productivity_index[i,9]  > 110 | 
            reportBuilder$productivity_index[i,9]  < 95)  &
            abs(reportBuilder$productivity_index[i,10]) > 1){
    reportBuilder$productivity_index$Watchlist[i] <- "Watchlist"
  } else {
    reportBuilder$productivity_index$Watchlist[i] <- "Acceptable"
  }
}

#FYTD Calculations ------------------------------------------------------------
reportBuilder[[4]] <- reportBuilder[[4]] %>%
  mutate(`FTE Variance FYTD` =
           (`Actual Worked FTE - FYTD Avg` - 
              `Total Target Worked FTE - FYTD Avg`),
         `Productivity Index FYTD` =
           (`Total Target Worked FTE - FYTD Avg` / 
              `Actual Worked FTE - FYTD Avg`) * 100,
         `OT % FYTD` =
           (`Overtime Hours - FYTD Avg` / 
              `Total Paid Hours - FYTD Avg`) * 100,
         `LE Index FYTD` =
           (`Target Labor Expense - FYTD Avg` / 
              `Actual Labor Expense - FYTD Avg`) * 100)

#Turn productivity indexes into percentages
reportBuilder$productivity_index[,3] <- 
  paste0(reportBuilder$productivity_index[,3],
         "%") 
reportBuilder$productivity_index[,5] <- 
  paste0(reportBuilder$productivity_index[,5],
         "%")
reportBuilder$productivity_index[,7] <- 
  paste0(reportBuilder$productivity_index[,7],
         "%")
#join producticity index report builder
breakdown_index <- 
  left_join(breakdown_performance,
            reportBuilder$productivity_index[,c(1,2,11,3:8)],
            by=c("Code" = "Department.Reporting.Definition.ID",
                 "Key Volume" = "Key.Volume"))

#Comparison Calculations-------------------------------------------------------
breakdown_comparison <- breakdown_index %>%
  left_join(reportBuilder$FYTD_performance,
          by = c("Code" = "Department.Reporting.Definition.ID",
                 "Key Volume" = "Key.Volume")) 
breakdown_comparison <- breakdown_comparison %>%
  mutate(
    #Target FTE Calculations
    Target_FTE_FYTD = variance[[distribution_i]][,1] - 
      `Total Target Worked FTE - FYTD Avg`,
    Target_FTE_RP = variance[[distribution_i]][,1] - 
      variance[[previous_distribution_i]][,1],
    #FTE Calculations
    FTE_FYTD = variance[[distribution_i]][,2] -
      `Actual Worked FTE - FYTD Avg`,
    FTE_RP = variance[[distribution_i]][,2] -
      variance[[previous_distribution_i]][,2],
    #FTE Variance Calculations
    FTE_Var_FYTD = variance[[distribution_i]][,3] -
      `Actual Worked FTE - FYTD Avg`,
    FTE_Var_RP = variance[[distribution_i]][,3] -
      variance[[previous_distribution_i]][,3],
    #Volume Calculations
    Vol_FYTD = variance[[distribution_i]][,4] - 
      `Volume - FYTD Avg`,
    Vol_RP = variance[[distribution_i]][,4] - 
      variance[[previous_distribution_i]][,4],
    #Productivity Index Calculations
    PI_FYTD = variance[[distribution_i]][,5] - 
      `Productivity Index FYTD`,
    PI_RP = variance[[distribution_i]][,5] - 
      variance[[previous_distribution_i]][,5],
    #Overtime % Calculations
    OT_FYTD = variance[[distribution_i]][,6] -
      `OT % FYTD`,
    OT_RP = variance[[distribution_i]][,6] - 
      variance[[previous_distribution_i]][,6],
    #Labor Expense Index Calculations
    LE_FYTD = variance[[distribution_i]][,7] - 
      `LE Index FYTD`,
    LE_RP = variance[[distribution_i]][,7] - 
      variance[[previous_distribution_i]][,7])
#select necessary columns
breakdown_comparison <- breakdown_comparison %>%
  select(c(1:36,55:ncol(breakdown_comparison)))

#Auto Concatenation------------------------------------------------------------
breakdown_text <- breakdown_comparison %>% 
  #Calculate % change for volume and worked fte
  mutate(VCPn = round((Vol_RP / breakdown_comparison[,19]) * 100, 2),
         WFTECPn = round((FTE_RP / breakdown_comparison[,17]) * 100, 2)) %>%
  mutate(
    #determine direction of change
    VCPn_direction = case_when(
      (VCPn > threshold) ~ "up",
      (VCPn < -threshold) ~ "down",
      (is.na(VCPn)) ~ "unavailable",
      TRUE ~ "steady"),
    WFTECPn_direction = case_when(
      (WFTECPn > threshold) ~ "up",
      (WFTECPn < -threshold) ~ "down",
      (is.na(WFTECPn)) ~ "unavailable",
      TRUE ~ "steady")) %>%
  mutate(
    #determine text based on direction of change
    VCP_text = case_when(
      (VCPn_direction == "steady") ~ 
        paste0("Reporting Period volume is steady compared to the previous",
               " reporting period"),
      (VCPn_direction == "unavailable") ~ 
        paste0("Reporting Period volume % change is unavailable"),
      TRUE ~ 
        paste0("Reporting Period volume is ", VCPn_direction, " ", VCPn,
               "% compared to the previous reporting period")
    ),
    WFTECP_text = case_when(
      (WFTECPn_direction == "steady") ~ 
        paste0("Reporting Period FTEs are steady compared to the previous",
               " reporting period"),
      (WFTECPn_direction == "unavailable") ~ 
        paste0("Reporting Period FTEs are unavailable"),
      TRUE ~ paste0("Reporting Period FTEs are ", WFTECPn_direction, " ",
                    WFTECPn, "% compared to the previous reporting period"))
    ) %>%
  #concatenate text columns and delete helper columns
  mutate(auto_text = paste0(VCP_text, "; ", WFTECP_text)) %>%
  select(-c(VCPn, WFTECPn, VCPn_direction, WFTECPn_direction, VCP_text, 
            WFTECP_text))

#assign an empty vector to notes and bind it to the df
Notes <- vector(mode="character", length = nrow(breakdown_text))
breakdown_text <- cbind(breakdown_text, Notes)

#assign column names for productivity index columns
colnames(breakdown_text)[(ncol(breakdown_text)-21):ncol(breakdown_text)] <- c(
  "Productivity Index",
  "FTE Variance",
  "Productivity Index",
  "FTE Variance",
  "Productivity Index",
  "FTE Variance",
  "Target FTE Difference from FYTD",
  "Target FTE Difference from Previous Distribution Period",
  "FTE Difference from FYTD",
  "FTE Difference from Previous Distribution Period",
  "FTE Variance Difference from FYTD",
  "FTE Variance Difference from Previous Distribution Period",
  "Volume Difference from FYTD",
  "Volume Difference from Previous Distribution Period",
  "Productivity Index % Difference From FYTD",
  "Productivity Index % Difference From Previous Distribution Period",
  "Overtime % Difference From FYTD",
  "Overtime % Difference From Previous Distribution Period",
  "Labor Expense Index % Difference From FYTD",
  "Labor Expense Index % Difference From Previous Distribution Period",
  "Volume & Labor Trend",
  "Notes")

#sub NA for the watchlist colunn
breakdown_text$Watchlist <- NA

#VP Roll-Up--------------------------------------------------------------------
source(paste0(here(),"/VP_Roll_Up.R"))

#Formatting--------------------------------------------------------------------
source(paste0(here(),"/Formatting.R"))



#logic for determining what site(s) to output
if("MSHS" %in% output_site){
  output_index <- breakdown_text
  output_appendix <- breakdown_performance_appendix
  output_VP_roll <- VP_roll_comparison_calc
} else {
  output_index <- breakdown_text %>%
    filter(Hospital %in% output_site)
  output_VP_roll <- VP_roll_comparison_calc %>%
    filter(Hospital %in% output_site)
  output_appendix <- breakdown_performance_appendix %>%
    filter(Hospital %in% output_site)
}

#format date for save file
Date <- gsub("/","-",distribution)
#save dataframe
write.table(output_index,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/",
                   paste(output_site, collapse = " & "), 
                   "_Department Performance Breakdown_", Date, ".csv"), 
            row.names = F, sep = ",")
