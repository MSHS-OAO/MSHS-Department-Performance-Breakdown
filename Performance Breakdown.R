library(dplyr)
library(tidyr)
library(readxl)
library(rlist)
library(stringr)

#---------------------Establish Constants
distribution <- "2/27/2021"
previous_distribution <- "1/30/2021"
#Read in end dates file for column headers
dates <- read.csv(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                         "Productivity/Analysis/MSHS Department Breakdown/",
                         "End Dates/EndDates.csv"), 
                  header = F)
#calculate date index for distribution and previous distribution
for(i in 1:nrow(dates)){
  if(dates[i,1] == distribution){
    distribution_i <- i
  } else if(dates[i,1] == previous_distribution){
    previous_distribution_i <- i
  }
}

#---------------------Read in files
dir_breakdown <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Analysis/MSHS Department Breakdown/")

#Reporting definitions included in all hospital admin rollup reports
definitions <- read.csv(paste0(dir_breakdown,
                               "Reporting Definitions/",
                               "Reporting Definitions.csv"))
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
  filter(KeyVol == "Y") %>%
  mutate(EffDate = 
           paste0(
             substr(EffDate,1,2), "/",
             substr(EffDate,3,4), "/",
             substr(EffDate,5,8)),
         EffDate = as.Date(EffDate, format = "%m/%d/%Y")) 
#list for baseline, productivity performance and productivity index reports
reportBuilder <- list()
#text in parenthesis indicate saved report title
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

#function to format numeric column by column name "col_name" to remove
#text characters in the data table "df"
format_numbers <- function(df, col_name){
  df[2:nrow(df), col_name] <- df[2:nrow(df), col_name] %>%
    str_replace_all(c("," = "", " " = "", "\\$" = ""))
  #df[, col_name] <- df[, col_name] %>%
  #str_replace_all(c("," = "", " " = "", "\\$" = ""))
  #df[, col_name] <- as.numeric(df[, col_name])
}
#function to apply numeric formatting function to each "x" data table 
#in the list "lt"
format_list <- function(lt, x){
  lt[[x]][2:nrow(lt[[x]]), 10:ncol(lt[[x]])] <- 
    sapply(colnames(lt[[x]])[10:ncol(lt[[x]])],
           function(y) format_numbers(lt[[x]], y))
  #lt[[x]][, 10:ncol(lt[[x]])] <- 
  #sapply(colnames(lt[[x]])[10:ncol(lt[[x]])],
  #function(y) format_numbers(lt[[x]], y))
  return(lt[[x]])
}
#using functions to format numeric columns in all data tables in the list
reportBuilder <- sapply(1:length(reportBuilder), 
                        function(x) format_list(reportBuilder, x))
#apply names to each list element
names(reportBuilder) <- c("time_period_performance", "department_performance",
                          "productivity_index", "FYTD_performance")

#----------------------------Formatting and Calculations
###Labor Standards###############################
#join labor standards and baseline performance to definitions table
breakdown_targets <- 
  left_join(definitions, laborStandards, by = c("Code" = "Code")) %>%
  select(Hospital.x, Code, Name, Key.Volume,
         EffDate, `Standard Type`, `Target WHpU`) %>%
###Baseline_Performance##########################  
  left_join(reportBuilder$time_period_performance, 
            by = c("Code" = "Department.Reporting.Definition.ID", 
                   "Key.Volume" = "Key.Volume")) 
#take necessary columns
breakdown_targets <- 
  as.data.frame(breakdown_targets[,c(1:7,15:ncol(breakdown_targets))])
#convert data elements to numeric for average calculations
for(i in 8:ncol(breakdown_targets)){
  breakdown_targets[,i] <- as.numeric(breakdown_targets[,i])
}
#create list for time period averages
time_period <- list()
#place 26 time periods of each metric into each list object. 7 list objects
cadence <- seq(from = 8, to = ncol(breakdown_targets), by = 7)
for(i in 0:6){
  time_period[[i+1]] <- breakdown_targets[,cadence + i]
}
#calculate 26 time period avg for each metric
time_period <- lapply(time_period, function(x) {
  data.frame(x) %>%
    mutate(Avg = rowMeans(x, na.rm = T))})
#cbind metric averages
breakdown_time_period <- cbind(
  breakdown_targets[,1:7], time_period[[1]][,27], time_period[[2]][,27],
  time_period[[3]][,27], time_period[[4]][,27], time_period[[5]][,27], 
  time_period[[6]][,27], time_period[[7]][,27])
#Assign column names
colnames(breakdown_time_period)[c(1,8:14)] <- c("Hospital", "Target Worked FTE", 
                                             "Worked FTE", "Volume", 
                                             "Paid Hours", "OT Hours", 
                                             "Target LE", "LE")
#calculate PI, OT% and LE Index
breakdown_time_period <- breakdown_time_period %>%
  mutate(`Productivity Index` = (`Target Worked FTE`/`Worked FTE`) * 100,
         `OT%` = (`OT Hours`/`Paid Hours`) * 100,
         `LE Index` = (`Target LE`/LE) * 100) %>%
#select necessary columns
  select(Hospital, Code, Name, Key.Volume, `Target WHpU`, `Worked FTE`, 
         Volume, `Productivity Index`, `OT%`, `LE Index`, EffDate, 
         `Standard Type`) %>%
###Department_Performance#######################
  #join reporting period performance table
  left_join(reportBuilder$department_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key.Volume" = "Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("Target FTE", "FTE", "Vol", "Paid Hours", "Overtime Hours", 
                  "Target Labor Expense", "Labor Expense")
for(i in seq(from = 20, to = ncol(breakdown_time_period), by = 7)){
  numbers <- seq(from = 20, to = ncol(breakdown_time_period), by = 7)
  for(j in 1:length(numbers)){
    if(numbers[j] == i){
      k = j
    }
  }
  colnames(breakdown_time_period)[i] <- paste(dates$V1[k], dataElements[1])
  colnames(breakdown_time_period)[i+1] <- paste(dates$V1[k], dataElements[2])
  colnames(breakdown_time_period)[i+2] <- paste(dates$V1[k], dataElements[3])
  colnames(breakdown_time_period)[i+3] <- paste(dates$V1[k], dataElements[4])
  colnames(breakdown_time_period)[i+4] <- paste(dates$V1[k], dataElements[5])
  colnames(breakdown_time_period)[i+5] <- paste(dates$V1[k], dataElements[6])
  colnames(breakdown_time_period)[i+6] <- paste(dates$V1[k], dataElements[7])
}
#take necessary columns
breakdown_performance <- 
  breakdown_time_period[,c(1:12,20:ncol(breakdown_time_period))]
#convert data elements to numeric
for(i in 13:ncol(breakdown_performance)){
  breakdown_performance[,i] <- as.numeric(breakdown_performance[,i])
}
#create list for reporting period variance calculations
variance <- list()
#list element for baseline and reporting period stats for all reporting periods
index_sequence <- seq(from = 13, to = ncol(breakdown_performance)-6, by = 7)
for(i in 1:length(index_sequence)){
  variance[[i]] <- cbind(
    breakdown_performance[,5:10],
    breakdown_performance[,index_sequence[i]:(index_sequence[i]+6)])
}
#save column names in seperate list
columns <- list()
for(i in 1:length(variance)){
  columns[[i]] <- colnames(variance[[i]])
  colnames(variance[[i]]) <- c("Target_WHpU", "Baseline_FTE", "Baseline_Vol", 
                               "Baseline_PI", "Baseline_OT", "Baseline_LEI",
                               "Target_FTE", "FTE", "Vol", "Paid_Hours", 
                               "OT_Hours", "Target_LE", "LE")
}
#calculate reporting period FTE difference to baseline FTE
variance <- lapply(variance, transform, 
                   FTE_Var  = FTE - Baseline_FTE)
#calculate FTE % change to baseline FTE
variance <- lapply(variance, transform, 
                   Var_Percent = FTE_Var/Baseline_FTE * 100)
#calculate reporting period volume % change to baseline volume
variance <- lapply(variance, transform, 
                   Vol_Percent = (((Vol - Baseline_Vol)/Baseline_Vol) * 100))
#calculate reporting period PI
variance <- lapply(variance, transform,
                   PI = Target_FTE/FTE * 100)
#calculate reporting period PI difference to baseline PI
variance <- lapply(variance, transform,
                   PI_Change = PI - Baseline_PI)
#calculate reporting period OT%
variance <- lapply(variance, transform,
                   OT = OT_Hours/Paid_Hours * 100)
#calculate reporting period OT% difference to baseline OT%
variance <- lapply(variance, transform,
                   OT_Change = OT - Baseline_OT)
#calculate reporting period LE Index
variance <- lapply(variance, transform,
                   LE_Index = Target_LE/LE * 100)
#calculate reporting period LE index difference to baseline LE index
variance <- lapply(variance, transform,
                   LE_Change = LE_Index - Baseline_LEI)
#rearange variance list elements and replace column names
for(i in 1:length(variance)){
  variance[[i]] <- variance[[i]][,c(8,14,15,9,16:22)]
  colnames(variance[[i]]) <- c(
    columns[[i]][8],
    paste0("*",dates[i,1], " FTE Difference"),
    paste0("*",dates[i,1], " FTE % Change"),
    columns[[i]][9],
    paste0("*",dates[i,1], " Volume % Change"),
    paste0(dates[i,1], " Productivity Index"),
    paste0("*",dates[i,1], " PI Difference"),
    paste0(dates[i,1], " Overtime %"),
    paste0("*",dates[i,1], " OT Difference"),
    paste0(dates[i,1], " LE Index"),
    paste0("*",dates[i,1], " LE Index Difference"))
}
#bind necessary columns from old breakdown_performance with variance list
breakdown_performance <- cbind(
  breakdown_performance[,1:12],
  list.cbind(variance))

###Productivity_Index###########################
#NA the second row with text in it
reportBuilder$productivity_index[1,] <- NA
#convert % and variances to numeric
for(i in seq(from = 10, to = ncol(reportBuilder$productivity_index), by = 2)){
    reportBuilder$productivity_index[,i] <- 
      as.numeric(sub("%", "", reportBuilder$productivity_index[,i]))
    reportBuilder$productivity_index[,i+1] <- 
      as.numeric(reportBuilder$productivity_index[,i+1])
}
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
    reportBuilder$productivity_index[,c(5,7,(ncol(reportBuilder$productivity_index) - 5):ncol(reportBuilder$productivity_index))], 
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

###############################Anjelica FYTD performance calculations
# Preprocessing Data ------------------------------------------------------
#renaming columns
rb_fytd_colnames <- c(colnames(reportBuilder[[4]])[1:8],
                      paste(reportBuilder[[4]][1, 9:ncol(reportBuilder[[4]])]))
colnames(reportBuilder[[4]]) <- rb_fytd_colnames
#removing empty column/row from exported format
reportBuilder[[4]] <- reportBuilder[[4]] %>%
  mutate(Metrics = NULL) %>%
  slice(-1)

# Calculations ------------------------------------------------------------
reportBuilder[[4]] <- reportBuilder[[4]] %>%
  mutate(`Productivity Index FYTD` =
           (`Total Target Worked FTE - FYTD Avg` / `Actual Worked FTE - FYTD Avg`) * 100,
         `OT % FYTD` =
           (`Overtime Hours - FYTD Avg` / `Total Paid Hours - FYTD Avg`) * 100,
         `LE Index FYTD` =
           (`Target Labor Expense - FYTD Avg` / `Actual Labor Expense - FYTD Avg`) * 100)
#####################################################################



#Calculate the rep period to FYTD comaparison
FYTD_comparison <- reportBuilder[[3]][,5] - reportBuilder[[3]][,7]

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
rep_period_comparison <- paste0(round(rep_period_comparison*100,2),
                                "%")
reportBuilder[[3]]$FYTD_comparison <- paste0(round(FYTD_comparison,2),
                                "%")
Notes <- vector(mode="character", length=nrow(breakdown_performance))
#join productivity index element to breakdown_performance
breakdown_index <- 
  left_join(breakdown_performance,
            reportBuilder$productivity_index[,c(1,2,11,3:8,12)],
            by=c("Code" = "Department.Reporting.Definition.ID",
                 "Key.Volume" = "Key.Volume"))
breakdown_index <- cbind(breakdown_index[,1:ncol(breakdown_index)-1],
                         rep_period_comparison,
                         breakdown_index[,ncol(breakdown_index)],
                         Notes)
#assign column names for productivity index columns
colnames(breakdown_index)[(ncol(breakdown_index)-8):ncol(breakdown_index)] <- c(
  "Productivity Index",
  "FTE Variance",
  "Productivity Index",
  "FTE Variance",
  "Productivity Index",
  "FTE Variance",
  "Productivity Index % Difference From Previous Distribution Period",
  "Productivity Index % Difference From FYTD",
  "Notes")

Date <- gsub("/","-",distribution)
write.table(breakdown_index,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/",
                   "MSHS_Department Performance Breakdown_",
                   Date, ".csv"), row.names = F, sep = ",")
