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

output_site <-
  select.list(
    choices = c("MSHS", "MSB", "MSBI", "MSH", "MSM", "MSQ", "MSW"),
    title = "Select Output Site(s)",
    multiple = T,
    graphics = T,
    preselect = "MSHS"
  )

#Read in Files-----------------------------------------------------------------
dir_breakdown <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Analysis/MSHS Department Breakdown/")

#Read in pay cycle file
dates <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Universal Data/Mapping/",
                          "MSHS_Pay_Cycle.xlsx"))

#Table of distribution dates
dist_dates <- dates %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         #filter 3 weeks from run date (21 days) for data collection lag before run date
         END.DATE < as.POSIXct(Sys.Date() - 21))
#Selecting current and previous distribution dates
distribution <- format(dist_dates$END.DATE[nrow(dist_dates)],"%m/%d/%Y")
previous_distribution <- format(dist_dates$END.DATE[nrow(dist_dates)-1],"%m/%d/%Y")
#Confirming distribution dates
cat("Current distribution is", distribution,
    "\nPrevious distribution is", previous_distribution)
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Correct distribution?",
                      graphics = T)
if (answer == "No") {
  distribution <- select.list(choices =
                                format(sort.POSIXlt(dist_dates$END.DATE, decreasing = T),
                                       "%m/%d/%Y"),
                        multiple = F,
                        title = "Select current distribution",
                        graphics = T)
  which(distribution == format(dist_dates$END.DATE, "%m/%d/%Y"))
  previous_distribution <- format(dist_dates$END.DATE[which(distribution == format(dist_dates$END.DATE, "%m/%d/%Y"))-1],"%m/%d/%Y")
}


#Table of end dates used for column header names
dates <- dates %>%
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
  filter(CLOSED > as.POSIXct(distribution, format = "%m/%d/%Y") | is.na(CLOSED), 
         DEPARTMENT.BREAKDOWN %in% c(TRUE,1)) %>%
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
  select(Partner, Hospital, Code, EffDate, VolID, DepID, `Standard Type`,
         `Target WHpU`, LEpU, WHpU2, LEpU2, PHpU, MinStaff, FixStaff, KeyVol) %>%
  filter(KeyVol == "Y") 

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
#Determine date index for distribution and previous distribution
distribution_i <- which(dates == distribution)
previous_distribution_i <- which(dates == previous_distribution)

#Labor Standards---------------------------------------------------------------
#join labor standards and baseline performance to definitions table
breakdown_performance <-
  left_join(definitions, laborStandards, by = c("Code" = "Code")) %>%
  select(Hospital.x, VP, `Corporate Service Line`, Code, Name, `Key Volume`,
         `Standard Type`, `Target WHpU`) %>%

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
#keep the column names of all columns that do not contain pay period date data - columns containg "..." in name
col_names <- c(breakdown_performance %>% select(-contains("...")) %>% colnames(),
               #for each date up to current distribution create one column name for each data element
               sapply(dates$END.DATE[1:distribution_i],
                      function(x) paste(x, dataElements)))
#Quality check - does the number of columns in the data match the col_names
if(length(col_names) != ncol(breakdown_performance)){
  #finding most recent date in report builder
  recent_data <- colnames(breakdown_performance)[ncol(breakdown_performance)]
  recent_date <- as.Date(substring(recent_data, 15, 24), format = "%m.%d.%Y")
  #if most recent end date in report builder is less than current distribution
  if(recent_date < as.Date(distribution, format = "%m/%d/%Y")){
    stop("Missing Data; Report Builder does not include data for current distribution pay period")
  }else{
    stop("Missing Data; check data elements variable matches report builder")
  }
  #assign new column names if the # of columns match
} else { colnames(breakdown_performance) <- col_names}

#take necessary columns
breakdown_performance <- breakdown_performance %>%
  select(-c(Corporation.Code, Corporation.Name, Entity.Code, Entity,
            Department.Reporting.Definition.Name, Mapped.Facility.Cost.Centers,
            Entity.Time.Period.Desc))

#create list for reporting period variance calculations
variance <- list()
#list element for code, key volume and reporting period average stats for time periods
variance <- lapply(pull(dates[1:distribution_i,]), function(x) {
  breakdown_performance %>% 
    select(Code, `Key Volume`, contains(x)) %>%
    as.data.frame()
})
#calculate reporting period Target FTE difference to baseline Target FTE
variance <- lapply(variance, function(x){
  #calculate metrics for FTE Var, PI%, OT%, LE Index %, and target comparison 
  new_col <- x %>% 
    #if any columns are added to variance, then indexes need to be adjusted 
    select(Code, #Column 1
           `Key Volume`, #Column 2
           ends_with("Target FTE"), #Column 3
           ends_with("FTE"), #Column 4
           ends_with("Overtime Hours"), #Column 5
           ends_with("Paid Hours"), #Column 6
           ends_with("Target Labor Expense"), #Column 7
           ends_with("Labor Expense")) #Column 8
  initial_metrics <- ncol(new_col)
  new_col <- new_col %>%
    mutate(`FTE Variance` = new_col[,4] - new_col[,3],
           `Productivity Index` = round(new_col[,3]/new_col[,4] * 100, 2),
           `Overtime %` = round(new_col[,5]/new_col[,6] * 100, 2),
           `LE Index` = round(new_col[,7]/new_col[,8] * 100, 2)) %>%
    mutate(`Below Target/On Target/Above Target` = case_when(
      is.na(`Productivity Index`) ~ "",
      `Productivity Index` < 95 ~ "Below Target",
      `Productivity Index` > 110 ~ "Above Target",
      TRUE ~ "On Target"))
  #take first 10 characters of the first column of new_col to get date
  col_name_date <- substr(colnames(new_col)[3], 1, 10)
  #remove initial columns from variance element
  new_col <- new_col[(initial_metrics + 1):ncol(new_col)]
  #paste reporting period date to corresponding variance element
  colnames(new_col) <- sapply(colnames(new_col), function(x) {
      paste(col_name_date, x)
    })
  #bind all metrics together in correct order
  x <- cbind(x, new_col)
  #use select funtion to properly arrange columns
  x <- x %>% 
    select(Code,
           `Key Volume`,contains("FTE"),
           contains("Volume"),
           contains("Productivity"),
           contains("Overtime %"),
           contains("LE Index"),
           contains("WHpU"),
           contains("Hours"),
           contains("Education & Orientation"),
           contains("Below Target"),
           contains("Target Labor Expense"),
           contains("Labor Expense"))
})

#Watchlist Criteria------------------------------------------------------------
#create empty watchlist
watchlist <- list()
#4 metrics needed for watchlist criteria logic
#order of metrics dependent on report builder structure
watchlist_metrics <- c("worked_hours", "volume", "fte", "target_fte")
#create data frame where each row represents a watchlist_metric for 6 pay periods
last_six_dates <- sapply(pull(dates[(distribution_i-5):distribution_i,]),
                         function(x) {
                           #format past 6 time periods to report builder format
                           x <- paste0(substr(x, 1, 2), ".",
                                  substr(x, 4, 5), ".",
                                  substr(x, 7, 10))
                           #identify and paste 4 subscripts to each date
                           sub_scripts <- c("", ".1", ".2", ".3")
                           sapply(sub_scripts, function(y) {
                             paste0(x, y)
                           })
                           })
#create the 4 watchlist elements for each needed metric
watchlist <- apply(last_six_dates, 1, function(x) {
  reportBuilder$watchlist[,] %>% select(
    Department.Reporting.Definition.ID,
    Key.Volume,
    ends_with(x))
  })
#Apply watchlist_metric name to each respective watchlist element
names(watchlist) <- watchlist_metrics
#check that each watchlist element has 6 numeric columns for each pp end date
six_date_check <- sapply(watchlist, function(x) {
  #get number of numeric columns in watchlist element
  six_date_check <- ncol(x %>% select(., where(is.double)))
  #compare number of numeric columns in watchlist element to number of dates
  if(six_date_check != ncol(last_six_dates)) {
    #if they are not equal then get set difference from expected dates and watchlist element dates
    error <- setdiff(colnames(last_six_dates), 
                     colnames(x %>% select(., where(is.double))))
    #stop script and report to user what end date is missing in report builder
    stop(paste(error, "not found in watchlist report builder"))
  }
})
#calculate the 6 pp average for each department for each metric
watchlist <- lapply(seq_along(watchlist), function(x) {
  watchlist[[x]] <- watchlist[[x]] %>%
    mutate(!! paste0(names(watchlist)[x],"_average") := 
             round(rowMeans(select(., where(is.double))), 2))
  })
names(watchlist) <- watchlist_metrics
#restructure the watchlist report builder for determing watchlist criteria
colnames(reportBuilder$watchlist)[(ncol(reportBuilder$watchlist) - 1):ncol(reportBuilder$watchlist)] <- 
  c("Productivity Index", "FTE Variance")
reportBuilder$watchlist <- reportBuilder$watchlist %>%
  #select necessary columns from original report builder
  select(Department.Reporting.Definition.ID,
         Key.Volume,
         (ncol(reportBuilder$watchlist) - 1):ncol(reportBuilder$watchlist)) %>%
  left_join(breakdown_performance %>% select(Code, `Key Volume`, `Target WHpU`),
            by = c("Department.Reporting.Definition.ID" = "Code",
                   "Key.Volume" = "Key Volume")) %>%
  #join with each watchlist element
  #worked hours
  left_join(watchlist$worked_hours %>% select(Department.Reporting.Definition.ID,
                                              Key.Volume,
                                              contains("average"))) %>%
  #volume
  left_join(watchlist$volume %>% select(Department.Reporting.Definition.ID,
                                              Key.Volume,
                                        contains("average"))) %>%
  #fte
  left_join(watchlist$fte %>% select(Department.Reporting.Definition.ID,
                                        Key.Volume,
                                     contains("average"))) %>%
  #target fte
  left_join(watchlist$target_fte %>% select(Department.Reporting.Definition.ID,
                                     Key.Volume,
                                     contains("average"))) %>%
  #calculate 6 pp fte variance and WHpU averages
  mutate(fte_variance_average = target_fte_average - fte_average,
         whpu_average = worked_hours_average/volume_average) %>%
  #calculate 6 pp productivity index average
  mutate(productivity_average = `Target WHpU`/whpu_average * 100) %>%
  #logic for watchlist criteria
  mutate(Watchlist = case_when(
    is.na(fte_variance_average) | is.na(whpu_average) ~ "Missing Data",
    (whpu_average > 110 | whpu_average < 95) & 
      abs(fte_variance_average) > 1 ~ "Watchlist",
    TRUE ~ "Acceptable")) %>%
  mutate(`Productivity Index` = paste0(`Productivity Index`,"%"))

#Comparison Calculations-------------------------------------------------------
calculation_function <- function(df){
  #Target FTE Calculations
  df$`Target FTE Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "Target FTE"))) - 
           select(df, contains(paste(distribution, "Target FTE"))))
  #FTE Calculations
  df$`FTE Difference from Previous Distribution Period` <- 
    pull(select(df, ends_with(paste(previous_distribution, "FTE"))) - 
           select(df, ends_with(paste(distribution, "FTE"))))
  #FTE % Change Calculations
  df$`FTE % Change From Previous Distribution Period` <- 
   ((select(df, ends_with(paste(previous_distribution, "FTE"))) /
       select(df, ends_with(paste(distribution, "FTE")))) - 1) * 100
  df$`FTE % Change From Previous Distribution Period` <- 
    apply(df$`FTE % Change From Previous Distribution Period`,
          MARGIN = 2, function(x){paste0(round(x, 2), "%")})
  #FTE Variance Calculations
  df$`FTE Variance Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "FTE Variance"))) - 
           select(df, contains(paste(distribution, "FTE Variance"))))
  #Volume Calculations
  df$`Volume Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "Volume"))) - 
           select(df, contains(paste(distribution, "Volume"))))
  #Volume % Change Calculations
  df$`Volume % Change From Previous Distribution Period` <- 
    ((select(df, ends_with(paste(previous_distribution, "Volume"))) /
        select(df, ends_with(paste(distribution, "Volume")))) - 1) * 100
  df$`Volume % Change From Previous Distribution Period` <- 
    apply(df$`Volume % Change From Previous Distribution Period`,
          MARGIN = 2, function(x){paste0(round(x, 2), "%")})
  #Productivity Index Calculations
  df$`Productivity Index % Difference From Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "Productivity Index"))) - 
           select(df, contains(paste(distribution, "Productivity Index"))))
  #Overtime % Calculations
  df$`Overtime % Difference From Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "Overtime %"))) - 
           select(df, contains(paste(distribution, "Overtime %"))))
  #Labor Expense Index Calculations
  df$`Labor Expense Index % Difference From Previous Distribution Period` <- 
    pull(select(df, contains(paste(previous_distribution, "LE Index"))) - 
           select(df, contains(paste(distribution, "LE Index"))))
  #Creating notes calculation
  df <- df %>% mutate(Notes = "")
  return(df)
}
#Applying calculations
breakdown_comparison <- calculation_function(left_join(variance[[previous_distribution_i]],
                                                            variance[[distribution_i]]))

#VP Roll-Up--------------------------------------------------------------------
source(paste0(here(),"/Roll_Up.R"))

#Formatting--------------------------------------------------------------------
source(paste0(here(),"/Formatting.R"))

#removing unwanted columns
breakdown_text <- breakdown_change[, -grep(colnames(breakdown_change),
                                           pattern = "Target FTE")]
breakdown_performance_appendix <- breakdown_performance_appendix[, -grep(colnames(breakdown_change),
                                                                        pattern = "Target FTE")]
#logic for determining what site(s) to output
if("MSHS" %in% output_site){
  output_index <- breakdown_text
  output_appendix <- breakdown_performance_appendix
  output_VP_roll <- roll_up_list$vp
  output_corpservice_roll <- roll_up_list$corporate
} else {
  output_index <- breakdown_text %>%
    filter(Hospital %in% output_site)
  output_VP_roll <- roll_up_list$vp %>%
    filter(Hospital %in% output_site)
  output_appendix <- breakdown_performance_appendix %>%
    filter(Hospital %in% output_site)
  output_corpservice_roll <- roll_up_list$corporate %>%
    filter(Hospital %in% output_site)
}

#format date for save file
Date <- gsub("/","-",distribution)

# work_book <- createWorkbook()
#
# addWorksheet(work_book, sheetName = "Department Breakdown")
# addWorksheet(work_book, sheetName = "Reports Removed")
# addWorksheet(work_book, sheetName = "Department Breakdown Guidelines")
# addWorksheet(work_book, sheetName = "VP Roll Up")
# addWorksheet(work_book, sheetName = "Corporate Service Roll Up")
# addWorksheet(work_book, sheetName = "Appendix")
#
# writeData(work_book, "Department Breakdown", output_index)
# writeData(work_book, "VP Roll Up", output_VP_roll)
# writeData(work_book, "Corporate Service Roll Up", output_corpservice_roll)
# writeData(work_book, "Appendix", output_appendix)
#
# file_name <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
#        "Productivity/Analysis/MSHS Department Breakdown/",
#        "Department Breakdown/xlsx/",
#        paste(output_site, collapse = " & "),
#        "_Department Performance Breakdown_", Date, ".xlsx")
# saveWorkbook(work_book, file = file_name, overwrite = FALSE)

write.table(output_index,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/Breakdown/",
                   paste(output_site, collapse = " & "),
                   "_Department Performance Breakdown_", Date, ".csv"),
            row.names = F, sep = ",")

#save appendix
write.table(output_appendix,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/Appendix/",
                   paste(output_site, collapse = " & "),
                   "_Breakdown Appendix_", Date, ".csv"),
            row.names = F, sep = ",")

#save VP rollup
write.table(output_VP_roll,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/VP Rollup/",
                   paste(output_site, collapse = " & "),
                   "_VP Rollup_", Date, ".csv"),
            row.names = F, sep = ",")

#save Corporate rollup
write.table(output_corpservice_roll,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/Corporate Rollup/",
                   paste(output_site, collapse = " & "),
                   "_Corporate Rollup_", Date, ".csv"),
            row.names = F, sep = ",")
