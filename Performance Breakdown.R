library(dplyr)
library(tidyr)
library(readxl)
library(rlist)
library(stringr)
library(here)
library(openxlsx)
library(lubridate)

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
#Table of non-distribution dates
non_dist_dates <- dates %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(FALSE, 0),
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
  filter(END.DATE >= as.Date("01/01/2022", format = "%m/%d/%Y")) %>%
  mutate(END.DATE = paste0(
    substr(END.DATE, 6, 7), "/",
    substr(END.DATE, 9, 10), "/",
    substr(END.DATE, 1, 4))) %>%
  filter(END.DATE != "01/14/2023") %>%
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
                                     "Labor Standards/2.0/", "LaborStandards2.0.csv"),
                              header = T,colClasses = c(rep("character", 9),
                                                        rep("numeric", 18)))

#filter on key volume and turn effective date into date format
laborStandards <- laborStandards %>%
  select(Corporation.Code, Entity.Code, Department.Definition.Code, Effective.Start.Date, 
         Workload.Volume.Code, Standard.Type, Key.Volume, Primary.Worked.Hours.per.Unit.Target,
         Primary.Paid.Labor.Expense.per.Unit.Target, Secondary.Worked.Hours.per.Unit.Target,
         Secondary.Paid.Labor.Expense.per.Unit.Target, Primary.Paid.Hours.per.Unit.Target,
         Primary.WHpU.Min.Variable.Staffing.FTE, Primary.WHpU.Fixed.Staffing.or.FTE) %>%
  filter(Key.Volume == "Y") %>%
  mutate(Effective.Start.Date = as.Date(Effective.Start.Date, format = "%m/%d/%Y")) %>%
  rename(Code = Department.Definition.Code)

#list for baseline, productivity performance and productivity index reports
reportBuilder <- list()

reportBuilder[[1]] <- read.csv(paste0(dir_breakdown,
                                         "Report Builder/2.0/",
                                         "Department Performance Breakdown/",
                                         "Report Builder 2.0.csv"),
                                  as.is = T)
#Read Watchlist report
reportBuilder[[2]] <- read.csv(paste0(dir_breakdown,
                                         "Report Builder/2.0/Watchlist/",
                                         "Watchlist 2.0.csv"),
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
  select(Hospital, VP, `Corporate Service Line`, Code, Name, `Key Volume`,
         Standard.Type, Primary.Worked.Hours.per.Unit.Target) %>%

#Reporting Period Performance--------------------------------------------------
  #join reporting period performance table
left_join(reportBuilder$department_performance,
          by = c("Code" = "Department.CODE",
                 "Key Volume" = "Corp.Time.Period.Time.Period.End.Date"))
#clean up column headers based on data element and pp end date
dataElements <- c("Target FTE", "FTE", "Volume", "Paid Hours",
                  "Target Labor Expense", "Labor Expense", "WHpU",
                  "Total Worked Hours", "Regular Hours", "Overtime Hours",
                  "Education Hours", "Orientation Hours", "Agency Hours",
                  "Other Worked Hours", "Education & Orientation %")
#Assign column names based on dates and data elements
#keep the column names of all columns that do not contain pay period date data - columns containg "..." in name
col_names <- c(breakdown_performance %>% select(-contains("X")) %>% colnames(),
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
  select(-c(Facility.CODE, Facility.DESC, Department.DESC))

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
  #take first 10 characters of the third column in variance$x to get the date
  col_name_date <- substr(colnames(x)[3], 1, 10)
  #calculate metrics for FTE Var, PI%, OT%, LE Index %, and target comparison
  new_col <- x %>% 
    #if any columns are added to variance, then indexes need to be adjusted 
    select(Code, #Column 1
           `Key Volume`, #Column 2
           matches(paste(col_name_date, "Target FTE")), #Column 3
           matches(paste(col_name_date, "FTE")), #Column 4
           matches(paste(col_name_date,"Overtime Hours")), #Column 5
           matches(paste(col_name_date,"Paid Hours")), #Column 6
           matches(paste(col_name_date,"Target Labor Expense")), #Column 7
           matches(paste(col_name_date,"Labor Expense"))) #Column 8
  initial_metrics <- ncol(new_col) 
  #Check if the correct number of columns are selected from above
  if(initial_metrics != 8){
    stop("Unexpected number of columns selected in variance variable")}
  
  # Convert selected columns to numeric
  new_col <- new_col %>%
    mutate(across(everything(), as.numeric))
  
  new_col <- new_col %>%
    mutate(`FTE Variance` = new_col[,4] - new_col[,3],
           `Productivity Index` = round(new_col[,3]/new_col[,4] * 100, 2),
           `Overtime %` = round(new_col[,5]/new_col[,6] * 100, 2),
           `Labor Expense Index` = round(new_col[,7]/new_col[,8] * 100, 2)) %>%
    mutate(`Below Target/On Target/Above Target` = case_when(
      is.na(`Productivity Index`) ~ "",
      `Productivity Index` < 95 ~ "Below Target",
      `Productivity Index` > 110 ~ "Above Target",
      TRUE ~ "On Target"))
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
           contains("Labor Expense Index"),
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

# Function to add leading zeros to month and day if needed
add_leading_zeros <- function(colname) {
  # Use regular expressions to identify and modify the date format
  colname <- gsub("X(\\d)\\.(\\d)\\.(\\d{4})", "X0\\1.0\\2.\\3", colname)
  colname <- gsub("X(\\d{2})\\.(\\d)\\.(\\d{4})", "X\\1.0\\2.\\3", colname)
  colname <- gsub("X(\\d)\\.(\\d{2})\\.(\\d{4})", "X0\\1.\\2.\\3", colname)
  colname <- gsub("X(\\d{2})\\.(\\d{2})\\.(\\d{4})", "X\\1.\\2.\\3", colname)
  return(colname)
}

# Apply the function to the column names
colnames(reportBuilder$watchlist) <- sapply(colnames(reportBuilder$watchlist), add_leading_zeros)

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
    Department.CODE,
    Corp.Time.Period.Time.Period.End.Date,
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
             rowMeans(select(., where(is.double))))
  })
names(watchlist) <- watchlist_metrics
#restructure the watchlist report builder for determing watchlist criteria
colnames(reportBuilder$watchlist)[(ncol(reportBuilder$watchlist) - 1):ncol(reportBuilder$watchlist)] <- 
  c("Productivity Index", "FTE Variance")
reportBuilder$watchlist <- reportBuilder$watchlist %>%
  #select necessary columns from original report builder
  select(Department.CODE,
         Corp.Time.Period.Time.Period.End.Date,
         (ncol(reportBuilder$watchlist) - 1):ncol(reportBuilder$watchlist)) %>%
  left_join(breakdown_performance %>% select(Code, `Key Volume`, Primary.Worked.Hours.per.Unit.Target),
            by = c("Department.CODE" = "Code",
                   "Corp.Time.Period.Time.Period.End.Date" = "Key Volume")) %>%
  #join with each watchlist element
  #worked hours
  left_join(watchlist$worked_hours %>% select(Department.CODE,
                                              Corp.Time.Period.Time.Period.End.Date,
                                              contains("average"))) %>%
  #volume
  left_join(watchlist$volume %>% select(Department.CODE,
                                        Corp.Time.Period.Time.Period.End.Date,
                                        contains("average"))) %>%
  #fte
  left_join(watchlist$fte %>% select(Department.CODE,
                                     Corp.Time.Period.Time.Period.End.Date,
                                     contains("average"))) %>%
  #target fte
  left_join(watchlist$target_fte %>% select(Department.CODE,
                                            Corp.Time.Period.Time.Period.End.Date,
                                     contains("average"))) %>%
  #calculate 6 pp fte variance and WHpU averages
  mutate(fte_variance_average = target_fte_average - fte_average,
         whpu_average = worked_hours_average/volume_average) %>%
  #calculate 6 pp productivity index average
  mutate(productivity_average = Primary.Worked.Hours.per.Unit.Target/whpu_average * 100) %>%
  #logic for watchlist criteria
  mutate(Watchlist = case_when(
    is.na(fte_variance_average) | is.na(whpu_average) ~ "Missing Data",
    (productivity_average > 110 | productivity_average < 95) & 
      abs(fte_variance_average) > 1 ~ "Watchlist",
    TRUE ~ "Acceptable")) %>%
  mutate(`Productivity Index` = paste0(`Productivity Index`,"%"))

#Comparison Calculations-------------------------------------------------------
calculation_function <- function(df){
  #Target FTE Calculations
  df$`Target FTE: Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(distribution, "Target FTE"))) - 
           select(df, contains(paste(previous_distribution, "Target FTE"))))
  #FTE Calculations
  df$`FTE Difference: from Previous Distribution Period` <- 
    pull(select(df, ends_with(paste(distribution, "FTE"))) - 
           select(df, ends_with(paste(previous_distribution, "FTE"))))
  #FTE % Change Calculations
  df$`FTE % Change From Previous Distribution Period` <- 
   (pull(select(df, ends_with(paste(distribution, "FTE"))) /
       select(df, ends_with(paste(previous_distribution, "FTE")))) - 1) * 100
  #FTE Variance Calculations
  df$`FTE Variance: Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(distribution, "FTE Variance"))) - 
           select(df, contains(paste(previous_distribution, "FTE Variance"))))
  #Volume Calculations
  df$`Volume: Difference from Previous Distribution Period` <- 
    pull(select(df, contains(paste(distribution, "Volume"))) - 
           select(df, contains(paste(previous_distribution, "Volume"))))
  #Volume % Change Calculations
  df$`Volume % Change From Previous Distribution Period` <- 
    (pull(select(df, ends_with(paste(distribution, "Volume"))) /
        select(df, ends_with(paste(previous_distribution, "Volume")))) - 1) * 100
  #Productivity Index Calculations
  df$`Productivity Index: Difference From Previous Distribution Period` <- 
    pull(select(df, contains(paste(distribution, "Productivity Index"))) - 
           select(df, contains(paste(previous_distribution, "Productivity Index"))))
  #Overtime % Calculations
  df$`Overtime %: Difference From Previous Distribution Period` <- 
    pull(select(df, contains(paste(distribution, "Overtime %"))) - 
           select(df, contains(paste(previous_distribution, "Overtime %"))))
  #Labor Expense Index Calculations
  df$`Labor Expense Index: Difference From Previous Distribution Period` <- 
    pull(select(df, matches(paste(distribution, "Labor Expense Index"))) - 
           select(df, matches(paste(previous_distribution, "Labor Expense Index"))))
  #Creating notes calculation
  df <- df %>% mutate(Notes = "")
  
  return(df)
}
#Applying calculations
comparison_calculations <- calculation_function(
  left_join(variance[[previous_distribution_i]],
              variance[[distribution_i]])
  ) %>% select(Code, 
               `Key Volume`,
               contains("Previous"),
               Notes)

#VP Roll-Up--------------------------------------------------------------------
source(paste0(here(),"/Roll_Up.R"))

#Formatting--------------------------------------------------------------------
source(paste0(here(),"/Formatting.R"))


# format date for save file
distribution_date <- gsub("/","-",distribution)

# list of msmw cpt departments
msmw_cpt <- c("MSW_15", "MSM_42", "MSM_41")

# joining and filtering variance elements to identify NA reports
na_report <- variance[[previous_distribution_i]] %>% 
  anti_join(
    variance[[previous_distribution_i]] %>%
      filter(Code %in% msmw_cpt) %>%
      filter_all(all_vars(!is.na(.)))) %>%
  left_join(variance[[distribution_i]]) %>%
  filter_all(any_vars(is.na(.)))

# extra departments report
extra_dep_report <- anti_join(
  select(reportBuilder$department_performance,
         Department.CODE, 
         Department.DESC), 
  select(breakdown_performance, 
         Code, 
         Name), 
  by = c("Department.CODE" = "Code"))

# save NA report
write.xlsx(na_report, 
           paste0(dir_breakdown, "Error Reports/2.0/NA Reports/", "NA_Reports_", 
                  distribution_date, ".xlsx"),
           overwrite = T)

# save extra departments
write.xlsx(extra_dep_report, 
           paste0(dir_breakdown, "Error Reports/2.0/Extra Departments/",
                  "Extra_Departments_Report_", distribution_date, ".xlsx"),
           overwrite = T)

# Creating Deliverables ---------------------------------------------------
# Create final dataframes to be saved. 
# (Main department breakdown, Appendix department breakdown, VP roll up, Corporate
#   Service line roll up)

dept_breakdown <- reduce(list(
  definitions,
  laborStandards %>%
    left_join(select(definitions, c("Code", "Key Volume"))) %>%
    select(Code, `Key Volume`, Effective.Start.Date, Standard.Type, Primary.Worked.Hours.per.Unit.Target) %>%
    rename(`Effective Date` = Effective.Start.Date),
  select(variance[[previous_distribution_i]], 
         -contains(c("Paid Hours", "Target Labor Expense")), 
         -ends_with("Labor Expense")),
  select(variance[[distribution_i]],
         -contains(c("Paid Hours", "Target Labor Expense")), 
         -ends_with("Labor Expense")),
  select(reportBuilder$watchlist, Department.CODE, 
         Corp.Time.Period.Time.Period.End.Date, Watchlist, `Productivity Index`, `FTE Variance`) %>%
    rename(`Key Volume` = Corp.Time.Period.Time.Period.End.Date,
           Code = Department.CODE),
  comparison_calculations 
), left_join) %>% 
  select(-contains("Target FTE"))

appendix <- reduce(
  list(
    definitions,
    laborStandards %>%
      left_join(select(definitions, c("Code", "Key Volume"))) %>%
      select(Code, `Key Volume`, Effective.Start.Date, Standard.Type, Primary.Worked.Hours.per.Unit.Target) %>%
      rename(`Effective Date` = Effective.Start.Date),
    reduce(variance, left_join)
  ), left_join) %>%
  select(-starts_with(format(non_dist_dates$END.DATE, "%m/%d/%Y")))

# Selecting Site Output(s) ------------------------------------------------
if(!"MSHS" %in% output_site){
  dept_breakdown <- dept_breakdown %>%
    filter(Hospital %in% output_site)
  roll$vp <- roll$vp %>%
    filter(Hospital %in% output_site)
  roll$corporate <- roll$corporate %>%
    filter(Hospital %in% output_site)
  appendix <- appendix %>%
    filter(Hospital %in% output_site)
}

#save main deliverable                      
write.table(dept_breakdown,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/2.0/",
                   paste(output_site, collapse = " & "),
                   "_Department Performance Breakdown_", distribution_date, ".csv"),
            row.names = F, sep = ",")

#save appendix
write.table(appendix,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/2.0/",
                   paste(output_site, collapse = " & "),
                   "_Breakdown Appendix_", distribution_date, ".csv"),
            row.names = F, sep = ",")

#save VP rollup
write.table(roll$vp,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/2.0/",
                   paste(output_site, collapse = " & "),
                   "_VP Rollup_", distribution_date, ".csv"),
            row.names = F, sep = ",")

#save Corporate rollup
write.table(roll$corporate,
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/Analysis/MSHS Department Breakdown/",
                   "Department Breakdown/csv/2.0/",
                   paste(output_site, collapse = " & "),
                   "_Corporate Rollup_", distribution_date, ".csv"),
            row.names = F, sep = ",")
