library(dplyr)
library(tidyr)
library(readxl)

#---------------------Read in files
#Reporting definitions included in all hospital admin rollup reports
definitions <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Reporting Definitions/Reporting Definitions.csv")
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
#Read in baseline performance report
baseline <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Baseline"))
#Read in report builder for 12/22/2019 - Present
reportBuilder <- read.csv(choose.files(default=
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/MSHS Department Breakdown/Report Builder/Performance"))

#----------------------------Formatting and Calculations
#join labor standards and baseline performance to definitions table
breakdown <- left_join(definitions,laborStandards,by=c("Code"="Code")) %>%
  select(Code,Name,Key.Volume,EffDate,StandardType,TargetWHpU) %>%
  left_join(baseline,by=c("Code"="Department.Reporting.Definition.ID","Key.Volume"="Key.Volume")) 
#take necessary columns
breakdown <- breakdown[,c(1:6,14:ncol(breakdown))]
#edit column names
colnames(breakdown)[7:21] <- c("1.4.20_FTE","1.4.20_Vol","1.4.20_WHpU",
                               "1.18.20_FTE","1.18.20_Vol","1.18.20_WHpU",
                               "2.1.20_FTE","2.1.20_Vol","2.1.20_WHpU",
                               "2.15.20_FTE","2.15.20_Vol","2.15.20_WHpU",
                               "2.29.20_FTE","2.29.20_Vol","2.29.20_WHpU")
#remove commas from numbers to convert character to numeric
for(j in 7:ncol(breakdown)){
  for(i in 1:nrow(breakdown)){
    breakdown[i,j] <- gsub(",","",breakdown[i,j])
  }
}
#force each column into numeric
breakdown[,7] <- as.numeric(breakdown[,7])
breakdown[,8] <- as.numeric(breakdown[,8])
breakdown[,9] <- as.numeric(breakdown[,9])
breakdown[,10] <- as.numeric(breakdown[,10])
breakdown[,11] <- as.numeric(breakdown[,11])
breakdown[,12] <- as.numeric(breakdown[,12])
breakdown[,13] <- as.numeric(breakdown[,13])
breakdown[,14] <- as.numeric(breakdown[,14])
breakdown[,15] <- as.numeric(breakdown[,15])
breakdown[,16] <- as.numeric(breakdown[,16])
breakdown[,17] <- as.numeric(breakdown[,17])
breakdown[,18] <- as.numeric(breakdown[,18])
breakdown[,19] <- as.numeric(breakdown[,19])
breakdown[,20] <- as.numeric(breakdown[,20])
breakdown[,21] <- as.numeric(breakdown[,21])
#calculate baseline averages and drop all other columns
breakdown <- breakdown %>%
  mutate(Baseline_FTE = round(rowMeans(select(.,contains("_FTE")),na.rm = T),digits = 2),
         Baseline_Vol = round(rowMeans(select(.,contains("_Vol")),na.rm = T),digits = 2),
         Baseline_WHpU = round(rowMeans(select(.,contains("_WHpU")),na.rm = T),digits = 2)) %>%
  select(c(1:3,contains("Baseline_"),TargetWHpU,StandardType,EffDate))


