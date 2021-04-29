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
#Reporting definitions included in all hospital admin rollup reports
definitions <- read.csv(paste0("J:/deans/Presidents/SixSigma/",
                               "MSHS Productivity/Productivity/Analysis/",
                               "MSHS Department Breakdown/",
                               "Reporting Definitions/",
                               "Reporting Definitions.csv"))



#labor standards for target information
laborStandards <- read.csv(paste0("J:/deans/Presidents/SixSigma/",
                                  "MSHS Productivity/Productivity/Analysis/",
                                  "MSHS Department Breakdown/Labor Standards/",
                                  "LaborStandards.csv"),
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
reportBuilder[[1]] <- read.csv(paste0("J:/deans/Presidents/SixSigma/",
                                      "MSHS Productivity/Productivity/",
                                      "Analysis/MSHS Department Breakdown/",
                                      "Report Builder/Time Period Performance/",
                                      "Time Period Performance.csv"))
#read productivity performance report (Department Performance Breakdown)
#message("select Department Performance Breakdown report")
reportBuilder[[2]] <- read.csv(paste0("J:/deans/Presidents/SixSigma/",
                                       "MSHS Productivity/Productivity/",
                                       "Analysis/MSHS Department Breakdown/",
                                       "Report Builder/",
                                       "Department Performance Breakdown/",
                                       "Report Builder.csv"))
#Read productivity index report (Productivity Index Report)
#message("select Productivity Index Report report")
reportBuilder[[3]] <- read.csv(paste0("J:/deans/Presidents/SixSigma/",
                                      "MSHS Productivity/Productivity/",
                                      "Analysis/MSHS Department Breakdown/",
                                      "Report Builder/",
                                      "Productivity Index Performance/",
                                      "Productivity.csv"))
#apply names to each list element
names(reportBuilder) <- c("time_period_performance", "department_performance",
                          "productivity_index")
#remove commas from report builders to convert character to numeric
for(k in 1:length(reportBuilder)){
  for(j in 10:ncol(reportBuilder[[k]])){
    for(i in 2:nrow(reportBuilder[[k]])){
      reportBuilder[[k]][i,j] <- gsub(" ", "", reportBuilder[[k]][i,j])
      reportBuilder[[k]][i,j] <- gsub("\\$", "", reportBuilder[[k]][i,j])
      reportBuilder[[k]][i,j] <- gsub(",", "", reportBuilder[[k]][i,j])
    }
  }
}

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
  select(Hospital, Code, Name, Key.Volume, EffDate, `Standard Type`, 
         `Target WHpU`, `Worked FTE`, Volume, `Productivity Index`, 
         `OT%`, `LE Index`)




# ###Department_Performance#######################
#   #join reporting period performance table
#   left_join(reportBuilder$department_performance,
#             by = c("Code" = "Department.Reporting.Definition.ID",
#                    "Key.Volume" = "Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("FTE", "Vol", "WHpU")
for(i in seq(from = 18, to = ncol(breakdown_performance), by = 3)){
  numbers <- seq(from = 18, to = ncol(breakdown_performance), by = 3)
  for(j in 1:length(numbers)){
    if(numbers[j] == i){
      k = j
    }
  }
  colnames(breakdown_performance)[i] <- paste(dates$V1[k], dataElements[1])
  colnames(breakdown_performance)[i+1] <- paste(dates$V1[k], dataElements[2])
  colnames(breakdown_performance)[i+2] <- paste(dates$V1[k], dataElements[3])
}
#take necessary columns
breakdown_performance <- 
  breakdown_performance[,c(1:10,18:ncol(breakdown_performance))]
#convert data elements to numeric
for(i in 11:ncol(breakdown_performance)){
  breakdown_performance[,i] <- as.numeric(breakdown_performance[,i])
}
#create list for reporting period variance calculations
variance <- list()
#list element for baseline and reporting period stats for all reporting periods
index_sequence <- seq(from = 11, to = ncol(breakdown_performance)-2, by = 3)
for(i in 1:length(index_sequence)){
  variance[[i]] <- cbind(
    breakdown_performance[,5:7],
    breakdown_performance[,index_sequence[i]:(index_sequence[i]+2)])
}
#save column names in seperate list
columns <- list()
for(i in 1:length(variance)){
  columns[[i]] <- colnames(variance[[i]])
  colnames(variance[[i]]) <- c("Baseline_FTE", "Baseline_Vol", "Baseline_WHpU", 
                               "FTE", "Vol", "WHpU")
}
#calculate reporting period FTE variance to baseline FTE
variance <- lapply(variance, transform, Var  = FTE - Baseline_FTE)
#calculate % of baseline variance to baseline FTE
variance <- lapply(variance, transform, Var_Percent = paste0(round(Var/Baseline_FTE*100,
                                                                   digits = 2),"%"))
#calculate reporting period volume variance to baseline volume
variance <- lapply(variance, transform, 
                   Vol_Percent = paste0(round((Vol - Baseline_Vol)/Baseline_Vol*100,
                                              digits = 2),"%"))
#calculate reporting period WHpU variance to baseline WHpU
variance <- lapply(variance, transform, 
                   WHPU_Percent = paste0(round((Baseline_WHpU - WHpU)/Baseline_WHpU*100,
                                              digits = 2),"%"))
#Calculate the distribution reporting period comparison
#rep_period_comparison <- 1 - (variance[[distribution_i]]$WHpU/variance[[previous_distribution_i]]$WHpU)
rep_period_comparison <- 
  (breakdown_performance$`Target WHpU`/variance[[distribution_i]]$WHpU) -
  (breakdown_performance$`Target WHpU`/variance[[previous_distribution_i]]$WHpU)
#rearange variance list elements and replace column names
for(i in 1:length(variance)){
  variance[[i]] <- variance[[i]][,c(4,7,8,5,9,6,10)]
  colnames(variance[[i]]) <- c(
    columns[[i]][4],
    paste0("*",dates[i,1], " FTE Difference"),
    paste0("*",dates[i,1], " FTE % Change"),
    columns[[i]][5],
    paste0("*",dates[i,1], " Volume % Change"),
    columns[[i]][6],
    paste0("*",dates[i,1], " Productivity % Change"))
}
#bind necessary columns from old breakdown_performance with variance list
breakdown_performance <- cbind(
  breakdown_performance[,1:10],
  list.cbind(variance))
colnames(breakdown_performance)[5:7] <- c("Baseline FTE", "Baseline Vol", "Baseline WHpU")
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
#Calculate the rep period to FYTD comaparison
#FYTD_comparison <- 1 - (reportBuilder[[3]][,7]/reportBuilder[[3]][,5])

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
