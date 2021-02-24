library(dplyr)
library(tidyr)
library(readxl)

#---------------------Read in files
#Reporting definitions included in all hospital admin rollup reports
definitions <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Reporting Definitions/Reporting Definitions.csv")
#Read in end dates file for column headers
dates <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/End Dates/EndDates.csv",
                  header = F)

#labor standards for target information
laborStandards <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Labor Standards"),
  header = F,colClasses = c(rep("character", 7),rep("numeric", 7)))
#change column headers for labor standards
colnames(laborStandards) <- c("Partner", "Hospital", "Code", "EffDate", "VolID",
                              "DepID", "StandardType", "TargetWHpU", "LEpU", 
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
#read baseline performance report (Baseline Performance)
message("select Baseline Performance report")
reportBuilder[[1]] <- read.csv(choose.files(default =
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Baseline Performance"))
#read productivity performance report (Department Performance Breakdown)
message("select Department Performance Breakdown report")
reportBuilder[[2]] <- read.csv(choose.files(default =
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Department Performance Breakdown"))
#Read productivity index report (Productivity Index Report)
message("select Productivity Index Report report")
reportBuilder[[3]] <- read.csv(choose.files(default =
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Productivity Index Performance"))
#apply names to each list element
names(reportBuilder) <- c("baseline_performance", "department_performance",
                          "productivity_index")
#remove commas from report builders to convert character to numeric
for(k in 1:length(reportBuilder)){
  for(j in 10:ncol(reportBuilder[[k]])){
    for(i in 2:nrow(reportBuilder[[k]])){
      reportBuilder[[k]][i,j] <- gsub(",", "", reportBuilder[[k]][i,j])
    }
  }
}


#----------------------------Formatting and Calculations
#join labor standards and baseline performance to definitions table
breakdown_targets <- 
  left_join(definitions, laborStandards, by = c("Code" = "Code")) %>%
  select(Code, Name, Key.Volume, EffDate, StandardType, TargetWHpU) %>%
  left_join(reportBuilder$baseline_performance, 
            by = c("Code" = "Department.Reporting.Definition.ID", 
                   "Key.Volume" = "Key.Volume")) 
#take necessary columns
breakdown_targets <- breakdown_targets[,c(1:6,14:ncol(breakdown_targets))]
#edit column names
colnames(breakdown_targets)[7:21] <- c("1.4.20_FTE","1.4.20_Vol","1.4.20_WHpU",
                               "1.18.20_FTE","1.18.20_Vol","1.18.20_WHpU",
                               "2.1.20_FTE","2.1.20_Vol","2.1.20_WHpU",
                               "2.15.20_FTE","2.15.20_Vol","2.15.20_WHpU",
                               "2.29.20_FTE","2.29.20_Vol","2.29.20_WHpU")
#convert data elements to numeric for average calculations
for(i in 7:ncol(breakdown_targets)){
  breakdown_targets[,i] <- as.numeric(breakdown_targets[,i])
}
#calculate baseline averages and drop all other columns
breakdown_performance <- breakdown_targets %>%
  mutate(Baseline_FTE = round(rowMeans(select(., contains("_FTE")),
                                       na.rm = T), digits = 2),
         Baseline_Vol = round(rowMeans(select(., contains("_Vol")),
                                       na.rm = T), digits = 2),
         Baseline_WHpU = round(rowMeans(select(., contains("_WHpU")),
                                        na.rm = T), digits = 4)) %>%
  #select baseline averages
  select(c(1:3, contains("Baseline_"), TargetWHpU, StandardType, EffDate)) %>%
  #join reporting period performance table
  left_join(reportBuilder$department_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key.Volume" = "Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("FTE", "Vol", "WHpU")
for(i in seq(from = 17, to = ncol(breakdown_performance), by = 3)){
  numbers <- seq(from =17, to = ncol(breakdown_performance), by = 3)
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
  breakdown_performance[,c(1:9,17:ncol(breakdown_performance))]
#convert data elements to numeric
for(i in 10:ncol(breakdown_performance)){
  breakdown_performance[,i] <- as.numeric(breakdown_performance[,i])
}
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
  mutate(Prod = round(rowMeans(.), digits = 2))
#Select past 6 time period FTE Variances and calculate average
Var <- reportBuilder$productivity_index[,] %>%
  select(seq(from = ncol(reportBuilder$productivity_index) - 34, 
             to = ncol(reportBuilder$productivity_index) - 4, 
             by = 6)) %>%
  mutate(Prod = round(rowMeans(.), digits = 2))
#bind 6 time period averages to productivity_index list element
reportBuilder$productivity_index <- 
  cbind(
    reportBuilder$productivity_index[,c(5,7,(ncol(reportBuilder$productivity_index) - 5):ncol(reportBuilder$productivity_index))], 
    Prod[,7], 
    Var[,7])
#logic for determining watchlist criteria
for(i in 2:nrow(reportBuilder$productivity_index)){
  if(is.na(reportBuilder$productivity_index[i,9]) |
     is.na(reportBuilder$productivity_index[i,10])){
    reportBuilder$productivity_index$Watchlist[i] <- "Missing Data"
  } else if(reportBuilder$productivity_index[i,9]  > 110 | 
            reportBuilder$productivity_index[i,9]  < 95  &
            reportBuilder$productivity_index[i,10] > 1){
    reportBuilder$productivity_index$Watchlist[i] <- "Watchlist"
  } else {
    reportBuilder$productivity_index$Watchlist[i] <- "Acceptable"
  }
}
################################################################################
#-------Gotten this far
################################################################################







#join in productivity index
breakdown_index <- 
  left_join(breakdown_performance,
            reportBuilder$productivity[,c(5,7,10:ncol(reportBuilder$productivity_index))],
            by=c("Code" = "Department.Reporting.Definition.ID",
                 "Key.Volume" = "Key.Volume")) 
#assign column names for productivity index columns
colnames(breakdown_index)[(ncol(breakdown_index)-5):ncol(breakdown_index)] <- c(
  paste(dates$V1[k],"Time Period Productivity Index"),
  paste(dates$V1[k],"Time Period FTE Variance"),
  paste(dates$V1[k],"Reporting Period Productivity Index"),
  paste(dates$V1[k],"Reporting Period FTE Variance"),
  paste(dates$V1[k],"FYTD Productivity Index"),
  paste(dates$V1[k],"FYTD FTE Variance"))







# repPer <- 
# breakdown <- breakdown %>%
#   mutate(`Reporting Period/FYTD Productivity Index` = breakdown)

