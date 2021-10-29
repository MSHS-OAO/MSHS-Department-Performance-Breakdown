library(dplyr)
library(tidyr)
library(readxl)
library(rlist)
library(stringr)
library(here)
library(openxlsx)

#Establish Constants-----------------------------------------------------------
#Site(s) user would like to produce department breakdown for
# enter "MSHS" for all sites
output_site <- c("MSHS")


#define current and previous distribution "mm/dd/yyyy"
distribution <- "08/28/2021"
previous_distribution <- "07/31/2021"

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
  select(SITE, VP, CORPORATE.SERVICE.LINE, DEFINITION.CODE, DEFINITION.NAME,
         KEY.VOLUME) %>%
  mutate(CORPORATE.SERVICE.LINE = replace_na(CORPORATE.SERVICE.LINE, "Not yet assigned")) %>%
  distinct()
colnames(definitions) <- c("Hospital", "VP", "Corporate Service Line", "Code",
                           "Name", "Key Volume")

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
#read productivity performance report (Department Performance Breakdown)
reportBuilder[[1]] <- read.csv(paste0(dir_breakdown,
                                      "Report Builder/",
                                      "Department Performance Breakdown/",
                                      "Report Builder.csv"),
                               as.is = T)
#Read Watchlist report
reportBuilder[[2]] <- read.csv(paste0(dir_breakdown,
                                      "Report Builder/Watchlist/",
                                      "Watchlist.csv"),
                               as.is = T)


#Format Files------------------------------------------------------------------
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
names(reportBuilder) <- c("department_performance", "watchlist")

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
  select(Hospital.x, VP, `Corporate Service Line`, Code, Name, `Key Volume`,
         EffDate, `Standard Type`, `Target WHpU`) %>%

#Reporting Period Performance--------------------------------------------------
  #join reporting period performance table
  left_join(reportBuilder$department_performance,
            by = c("Code" = "Department.Reporting.Definition.ID",
                   "Key Volume" = "Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("Target FTE", "FTE", "Volume", "Paid Hours",
                  "Target Labor Expense", "Labor Expense", "WHpU",
                  "Total Worked Hours", "Regular Hours", "Overtime Hours",
                  "Education Hours", "Orientation Hours", "Agency Hours",
                  "Other Worked Hours", "Education & Orientation %")
#Assign column names based on dates and data elements
for(i in seq(from = 17, to = ncol(breakdown_targets), by = length(dataElements))){
  numbers <- seq(from = 17, to = ncol(breakdown_targets), by = length(dataElements))
  for(j in 1:length(numbers)){
    if(numbers[j] == i){
      k = j
    }
  }
  colnames(breakdown_targets)[i] <- paste(dates[k,1], dataElements[1])
  colnames(breakdown_targets)[i+1] <- paste(dates[k,1], dataElements[2])
  colnames(breakdown_targets)[i+2] <- paste(dates[k,1], dataElements[3])
  colnames(breakdown_targets)[i+3] <- paste(dates[k,1], dataElements[4])
  colnames(breakdown_targets)[i+4] <- paste(dates[k,1], dataElements[5])
  colnames(breakdown_targets)[i+5] <- paste(dates[k,1], dataElements[6])
  colnames(breakdown_targets)[i+6] <- paste(dates[k,1], dataElements[7])
  colnames(breakdown_targets)[i+7] <- paste(dates[k,1], dataElements[8])
  colnames(breakdown_targets)[i+8] <- paste(dates[k,1], dataElements[9])
  colnames(breakdown_targets)[i+9] <- paste(dates[k,1], dataElements[10])
  colnames(breakdown_targets)[i+10] <- paste(dates[k,1], dataElements[11])
  colnames(breakdown_targets)[i+11] <- paste(dates[k,1], dataElements[12])
  colnames(breakdown_targets)[i+12] <- paste(dates[k,1], dataElements[13])
  colnames(breakdown_targets)[i+13] <- paste(dates[k,1], dataElements[14])
  colnames(breakdown_targets)[i+14] <- paste(dates[k,1], dataElements[15])
}
#take necessary columns
breakdown_performance <-
  breakdown_targets[,c(1:9,17:ncol(breakdown_targets))] %>%
  filter(duplicated(Code) == F)
#create list for reporting period variance calculations
variance <- list()
#list element for baseline and reporting period stats for all reporting periods
index_sequence <- seq(from = 10, to = ncol(breakdown_performance)-14, by = length(dataElements))
for(i in 1:length(index_sequence)){
  variance[[i]] <- cbind(
    breakdown_performance[,index_sequence[i]:(index_sequence[i]+14)])
}
#save column names in seperate list
columns <- list()
for(i in 1:length(variance)){
  columns[[i]] <- colnames(variance[[i]])
  colnames(variance[[i]]) <- c("Target_FTE", "FTE", "Vol", "Paid_Hours",
                               "Target_LE", "LE", "WHpU", "Worked_Hours",
                               "Regular_Hours", "Overtime_Hours",
                               "Education_Hours", "Orientation_Hours",
                               "Agency_Hours", "Other_Worked_Hours",
                               "Education_Orientation")
}
#calculate reporting period Target FTE difference to baseline Target FTE
variance <- lapply(variance, transform,
                   FTE_Variance  = FTE - Target_FTE)
#calculate reporting period PI
variance <- lapply(variance, transform,
                   PI = Target_FTE/FTE * 100)
#calculate reporting period OT%
variance <- lapply(variance, transform,
                   OT = Overtime_Hours/Paid_Hours * 100)
#calculate reporting period LE Index
variance <- lapply(variance, transform,
                   LE_Index = Target_LE/LE * 100)
#calculate below target, on target, above target
variance <- lapply(variance, transform,
                   target = case_when(
                     is.na(PI) ~ "",
                     PI < 95 ~ "Below Target",
                     PI > 110 ~ "Above Target",
                     TRUE ~ "On Target"))
#save full variance for roll up
variance_roll <- variance

#rearange variance list elements and replace column names
for(i in 1:length(variance)){
  variance[[i]] <- variance[[i]] %>%
    select(Target_FTE, FTE, FTE_Variance, Vol, PI, OT, LE_Index, WHpU,
           Worked_Hours, Regular_Hours, Overtime_Hours, Education_Hours,
           Orientation_Hours, Agency_Hours, Other_Worked_Hours,
           Education_Orientation, target)
  colnames(variance[[i]]) <- c(
    columns[[i]][1],
    columns[[i]][2],
    paste0(dates[i,1], " FTE Variance"),
    columns[[i]][3],
    paste0(dates[i,1], " Productivity Index"),
    paste0(dates[i,1], " Overtime %"),
    paste0(dates[i,1], " LE Index"),
    columns[[i]][7],
    columns[[i]][8],
    columns[[i]][9],
    columns[[i]][10],
    columns[[i]][11],
    columns[[i]][12],
    columns[[i]][13],
    columns[[i]][14],
    columns[[i]][15],
    paste0(dates[i,1], " Below Target/On Target/Above Target")
    )
}

#bind columns from breakdown_performance with variance list to create appendix
breakdown_performance_appendix <- cbind(
  breakdown_performance[,1:9],
  list.cbind(variance))
#select only previous and current distribution for main deliverable
breakdown_performance <- cbind(
  breakdown_performance[,1:9],
  variance[[previous_distribution_i]],
  variance[[distribution_i]])

#Watchlist Criteria------------------------------------------------------------
#select past 6 time period worked hours and calculate average
worked_hours <- reportBuilder$watchlist[,] %>%
  select(seq(from = ncol(reportBuilder$watchlist) - 59,
             to = ncol(reportBuilder$watchlist) - 9,
             by = 10)) %>%
  mutate(worked_hours = rowMeans(.))
#select past 6 time period volume and calculate average
volume <- reportBuilder$watchlist[,] %>%
  select(seq(from = ncol(reportBuilder$watchlist) - 58,
             to = ncol(reportBuilder$watchlist) - 8,
             by = 10)) %>%
  mutate(volume = rowMeans(.))
#select past 6 time period FTEs and calculate average
FTE <- reportBuilder$watchlist[,] %>%
  select(seq(from = ncol(reportBuilder$watchlist) - 57,
             to = ncol(reportBuilder$watchlist) - 7,
             by = 10)) %>%
  mutate(FTE = rowMeans(.))
#select past 6 time period target FTEs and calculate average
target_FTE <-  reportBuilder$watchlist[,] %>%
  select(seq(from = ncol(reportBuilder$watchlist) - 56,
             to = ncol(reportBuilder$watchlist) - 6,
             by = 10)) %>%
  mutate(target_FTE = rowMeans(.))
#calculate 6 time period whpu average (ratio of averages)
reportBuilder$watchlist <- cbind(reportBuilder$watchlist[,c(5,7,
                                                            (ncol(reportBuilder$watchlist) - 1):
                                                              ncol(reportBuilder$watchlist))],
                                 target_FTE$target_FTE - FTE$FTE,
                                 worked_hours$worked_hours/volume$volume) %>%
  #bring in target whpu
  left_join(breakdown_performance[,c(4,6,9)],
            by = c("Department.Reporting.Definition.ID" = "Code",
                   "Key.Volume" = "Key Volume")) %>%
  #calculate prod index for 6 time period average
  mutate(prod = `Target WHpU` / `worked_hours$worked_hours/volume$volume` * 100) %>%
  rename(FTE_var = `target_FTE$target_FTE - FTE$FTE`)
#logic for determining watchlist criteria
reportBuilder$watchlist$Watchlist <- NA
for(i in 2:nrow(reportBuilder$watchlist)){
  if(is.na(reportBuilder$watchlist[i,8]) |
     is.na(reportBuilder$watchlist[i,5])){
    reportBuilder$watchlist$Watchlist[i] <- "Missing Data"
  } else if((reportBuilder$watchlist[i,8]  > 110 |
             reportBuilder$watchlist[i,8]  < 95)  &
            abs(reportBuilder$watchlist[i,5]) > 1){
    reportBuilder$watchlist$Watchlist[i] <- "Watchlist"
  } else {
    reportBuilder$watchlist$Watchlist[i] <- "Acceptable"
  }
}

#Turn productivity indexes into percentages
reportBuilder$watchlist[,3] <-
  paste0(reportBuilder$watchlist[,3],
         "%")

#join producticity index report builder
breakdown_index <-
  left_join(breakdown_performance,
            reportBuilder$watchlist[,c(1,2,9,3,4)],
            by=c("Code" = "Department.Reporting.Definition.ID",
                 "Key Volume" = "Key.Volume")) %>%
  #remove duplicated codes (DUS_09)
  filter(duplicated(Code) == F)

# #hard code for DUS_09 time period averages
# #Target FTE, Worked FTE, FTE Variance, Volume, PI, OT%, LE Index
# dus_09 <- c(81.5, 73.98, -7.52 , 3893.46, 110.16397, 1.00436192, 108.05589)
#
# breakdown_index[breakdown_index$Code == "DUS_09",10:16] <- dus_09

#Comparison Calculations-------------------------------------------------------
# breakdown_comparison <- breakdown_index %>%
#   left_join(reportBuilder$FYTD_performance,
#           by = c("Code" = "Department.Reporting.Definition.ID",
#                  "Key Volume" = "Key.Volume"))
breakdown_comparison <- breakdown_index %>%
  mutate(
    #Target FTE Calculations
    Target_FTE_RP = variance[[distribution_i]][,1] -
      variance[[previous_distribution_i]][,1],
    #FTE Calculations
    FTE_RP = variance[[distribution_i]][,2] -
      variance[[previous_distribution_i]][,2],
    #FTE Variance Calculations
    FTE_Var_RP = variance[[distribution_i]][,3] -
      variance[[previous_distribution_i]][,3],
    #Volume Calculations
    Vol_RP = variance[[distribution_i]][,4] -
      variance[[previous_distribution_i]][,4],
    #Productivity Index Calculations
    PI_RP = variance[[distribution_i]][,5] -
      variance[[previous_distribution_i]][,5],
    #Overtime % Calculations
    OT_RP = variance[[distribution_i]][,6] -
      variance[[previous_distribution_i]][,6],
    #Labor Expense Index Calculations
    LE_RP = variance[[distribution_i]][,7] -
      variance[[previous_distribution_i]][,7])

#create and place columns for % Change in volume and FTEs compared to prev RP
breakdown_change <- breakdown_comparison %>%
  mutate(
    VCPn = (variance[[distribution_i]][,4] /
         variance[[previous_distribution_i]][,4]) - 1,
    WFTECPn = (variance[[distribution_i]][,2] /
         variance[[previous_distribution_i]][,2]) - 1) %>%
  mutate(WFTECPn = paste0(round(WFTECPn, 4) * 100, "%"),
         VCPn = paste0(round(VCPn, 4) * 100, "%")) %>%
  relocate(WFTECPn, .after = FTE_RP) %>%
  relocate(VCPn, .after = Vol_RP)

#assign an empty vector to notes and bind it to the df
Notes <- vector(mode="character", length = nrow(breakdown_change))
breakdown_change <- cbind(breakdown_change, Notes)

#assign column names for productivity index columns
colnames(breakdown_change)[c(1,7,(ncol(breakdown_change)-11):ncol(breakdown_change))] <- c(
  "Hospital",
  "Effective Date",
  "Productivity Index",
  "FTE Variance",
  "Target FTE Difference from Previous Distribution Period",
  "FTE Difference from Previous Distribution Period",
  "FTE % Change From Previous Distribution Period",
  "FTE Variance Difference from Previous Distribution Period",
  "Volume Difference from Previous Distribution Period",
  "Volume % Change From Previous Distribution Period",
  "Productivity Index % Difference From Previous Distribution Period",
  "Overtime % Difference From Previous Distribution Period",
  "Labor Expense Index % Difference From Previous Distribution Period",
  "Notes")

#sub NA for the watchlist colunn
#breakdown_text$Watchlist <- NA

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

work_book <- createWorkbook()

addWorksheet(work_book, sheetName = "Department Breakdown")
addWorksheet(work_book, sheetName = "Reports Removed")
addWorksheet(work_book, sheetName = "Department Breakdown Guidelines")
addWorksheet(work_book, sheetName = "VP Roll Up")
addWorksheet(work_book, sheetName = "Appendix")

writeData(work_book, "Department Breakdown", output_index)
writeData(work_book, "VP Roll Up", output_VP_roll)
writeData(work_book, "Appendix", output_appendix)

file_name <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
       "Productivity/Analysis/MSHS Department Breakdown/",
       "Department Breakdown/xlsx/",
       paste(output_site, collapse = " & "),
       "_Department Performance Breakdown_", Date, ".xlsx")
saveWorkbook(work_book, file = file_name, overwrite = FALSE)