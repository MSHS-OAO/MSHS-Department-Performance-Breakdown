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
  header = F,colClasses = c(rep("character",7),rep("numeric",7)))
colnames(laborStandards) <- c("Partner","Hospital","Code","EffDate","VolID","DepID","StandardType","TargetWHpU","LEpU","WHpU2","LEpU2","PHpU","MinStaff","FixStaff","KeyVol")
laborStandards <- laborStandards %>% 
  filter(KeyVol == "Y") %>%
  mutate(EffDate = 
           paste0(
             substr(EffDate,1,2),"/",
             substr(EffDate,3,4),"/",
             substr(EffDate,5,8)),
         EffDate = as.Date(EffDate,format="%m/%d/%Y")) 
reportBuilder <- list()
#Read in baseline performance report
reportBuilder[[1]] <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Baseline"))
#Read in report builder for 12/22/2019 - Present
reportBuilder[[2]] <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Performance"))
#Read in report builder for 5/10/2020 - Present
reportBuilder[[3]] <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Performance"))
names(reportBuilder) <- c("baseline","performance","productivity")
#remove commas from report builders to convert character to numeric
for(k in 1:length(reportBuilder)){
  for(j in 10:ncol(reportBuilder[[k]])){
    for(i in 2:nrow(reportBuilder[[k]])){
      reportBuilder[[k]][i,j] <- gsub(",","",reportBuilder[[k]][i,j])
    }
  }
}


#----------------------------Formatting and Calculations
#join labor standards and baseline performance to definitions table
breakdown <- left_join(definitions,laborStandards,by=c("Code"="Code")) %>%
  select(Code,Name,Key.Volume,EffDate,StandardType,TargetWHpU) %>%
  left_join(reportBuilder$baseline,by=c("Code"="Department.Reporting.Definition.ID","Key.Volume"="Key.Volume")) 
#take necessary columns
breakdown <- breakdown[,c(1:6,14:ncol(breakdown))]
#edit column names
colnames(breakdown)[7:21] <- c("1.4.20_FTE","1.4.20_Vol","1.4.20_WHpU",
                               "1.18.20_FTE","1.18.20_Vol","1.18.20_WHpU",
                               "2.1.20_FTE","2.1.20_Vol","2.1.20_WHpU",
                               "2.15.20_FTE","2.15.20_Vol","2.15.20_WHpU",
                               "2.29.20_FTE","2.29.20_Vol","2.29.20_WHpU")
#convert data elements to numeric for average calculations
for(i in 7:ncol(breakdown)){
  breakdown[,i] <- as.numeric(breakdown[,i])
}
#calculate baseline averages and drop all other columns
breakdown <- breakdown %>%
  mutate(Baseline_FTE = round(rowMeans(select(.,contains("_FTE")),na.rm = T),digits = 2),
         Baseline_Vol = round(rowMeans(select(.,contains("_Vol")),na.rm = T),digits = 2),
         Baseline_WHpU = round(rowMeans(select(.,contains("_WHpU")),na.rm = T),digits = 4)) %>%
  #select baseline averages
  select(c(1:3,contains("Baseline_"),TargetWHpU,StandardType,EffDate)) %>%
  #join reporting period performance table
  left_join(reportBuilder$performance,by=c("Code"="Department.Reporting.Definition.ID","Key.Volume"="Key.Volume"))
#clean up column headers based on data element and pp end date
dataElements <- c("FTE","Vol","WHpU")
for(i in seq(from=17,to=ncol(breakdown),by=3)){
  numbers <- seq(from=17,to=ncol(breakdown),by=3)
  for(j in 1:length(numbers)){
    if(numbers[j] == i){
      k=j
    }
  }
  colnames(breakdown)[i] <- paste(dates$V1[k],dataElements[1])
  colnames(breakdown)[i+1] <- paste(dates$V1[k],dataElements[2])
  colnames(breakdown)[i+2] <- paste(dates$V1[k],dataElements[3])
}
#take necessary columns
breakdown <- breakdown[,c(1:9,17:ncol(breakdown))]
#convert data elements to numeric
for(i in 10:ncol(breakdown)){
  breakdown[,i] <- as.numeric(breakdown[,i])
}
#join in productivity index
breakdown <- left_join(breakdown,reportBuilder$productivity[,c(5,7,10:ncol(reportBuilder$productivity))],by=c("Code"="Department.Reporting.Definition.ID","Key.Volume"="Key.Volume")) 
#assign column names for productivity index columns
colnames(breakdown)[(ncol(breakdown)-5):ncol(breakdown)] <- c(
  paste(dates$V1[k],"Time Period Productivity Index"),
  paste(dates$V1[k],"Time Period FTE Variance"),
  paste(dates$V1[k],"Reporting Period Productivity Index"),
  paste(dates$V1[k],"Reporting Period FTE Variance"),
  paste(dates$V1[k],"FYTD Productivity Index"),
  paste(dates$V1[k],"FYTD FTE Variance"))
repPer <- 
breakdown <- breakdown %>%
  mutate(`Reporting Period/FYTD Productivity Index` = breakdown)

