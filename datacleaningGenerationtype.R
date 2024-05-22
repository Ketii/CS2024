

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PREAMBLE----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## a. CLEAN WORKSPACE, LOAD LIBRARIES, PATH, CORES ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



rm(list = ls())
library(tidyverse)
library(kableExtra)
library(lubridate)
library(readxl)
library(readODS)
library(data.table)
library(dplyr)

path <- getwd()
prefix <- paste0(path, "/Data")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A. 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#names of datasets
Daten = list.files(file.path(prefix, 'AggregatedGenerationPerType_16.1.B_C'))

#from 2015 01 to 2024 03 2:112
#first 2015 01
Data = fread(file = file.path(prefix, 'AggregatedGenerationPerType_16.1.B_C', Daten[2]))

table(Data$MapCode)
#choose Germany
Data <- Data[grep("DE", Data$MapCode), ]
Data <- Data[order(Data$DateTime, Data$ProductionType),]

#data collected every 15 minutes: aggregate hourly: from 00:00 to 00:45 -> 00, 01:00 to 01:45 -> 01:00
#production type: on depending on mapcode: biomass -> transnet, tennet... aggregate or choose one?
#for starters just DE

Data = Data[Data$MapCode == "DE", ]
Data = Data[, -c(2:4, 10)]



Data$DateTime <- as.POSIXct(Data$DateTime)
Data$Date <-  as.Date(Data$DateTime)
Data$Hour <- format(Data$DateTime, "%H")

#GenerationOutput and COnsumption in an hour would be sum of every 15 minutes
myAggreg <- Data %>%
    group_by(Date, Hour, ProductionType) %>%
    summarise(GenerationOutputSum = sum(ActualGenerationOutput, na.rm = TRUE),
              ConsumptionSum = sum(ActualConsumption, na.rm = TRUE))

myAggreg$DateTime <- as.POSIXct(paste(myAggreg$Date,  paste0(myAggreg$Hour,":00:00")), format = "%Y-%m-%d %H:%M:%S")



myAggreg <- subset(myAggreg, select = -c(Date, Hour))

myAggreg$Types <- NA

#group production types into 6 larger groups
myAggreg$Types[grep("Fossil", myAggreg$ProductionType)] <- "Fossil"
myAggreg$Types[grep("Hydro", myAggreg$ProductionType)] <- "Hydro"


myAggreg$Types[grep("Other|Solar|Waste|Wind|Nuclear", myAggreg$ProductionType)] <- "Other"
myAggreg$ProductionType <- ifelse(is.na(myAggreg$Types), myAggreg$ProductionType, myAggreg$Types)
myAggreg$Types <- NULL

#aggregate over new production types
myAggreg <- myAggreg %>%
    group_by(DateTime, ProductionType) %>%
    summarise(GenerationOutputSum = mean(GenerationOutputSum, na.rm = TRUE),
              ConsumptionSum = mean(ConsumptionSum, na.rm = TRUE))

#data in wide/panel format for compatibility with 
wide_data <- myAggreg %>%
    tidyr::pivot_wider(names_from = ProductionType,
                values_from = c(GenerationOutputSum, ConsumptionSum))

######################################################################################


#same procedure in a loop for all months
for (i in 3:112) {
    Data_i = fread(file = file.path(prefix, 'AggregatedGenerationPerType_16.1.B_C', Daten[i]))
    
    Data_i = Data_i[grep("DE", Data_i$MapCode), ]
    Data_i = Data_i[order(Data_i$DateTime, Data_i$ProductionType),]
    Data_i = Data_i[Data_i$MapCode == "DE", ]
    Data_i = Data_i[, -c(2:4, 10)]
    Data_i$DateTime <- as.POSIXct(Data_i$DateTime)
    Data_i$Date <-  as.Date(Data_i$DateTime)
    Data_i$Hour <- format(Data_i$DateTime, "%H")
    myAggreg_i <- Data_i %>%
        group_by(Date, Hour, ProductionType) %>%
        summarise(GenerationOutputSum = sum(ActualGenerationOutput, na.rm = TRUE),
                  ConsumptionSum = sum(ActualConsumption, na.rm = TRUE))
    
    #myAggreg_i$DateTime <- as.POSIXct(paste(myAggreg_i$Date,  paste0(myAggreg_i$Hour,":00:00")), format = "%Y-%m-%d %H:%M:%S")
    custom_tz = "Etc/GMT-1"
    myAggreg_i$DateTime = ymd_hms(paste(myAggreg_i$Date,  paste0(myAggreg_i$Hour,":00:00")))
    myAggreg_i$DateTime = force_tz(myAggreg_i$DateTime, custom_tz)
    
    
    myAggreg_i <- subset(myAggreg_i, select = -c(Date, Hour))

    
   #  if(length(grep("-03-", myAggreg_i$DateTime))>0){
   #  
   #  
   #  springforward <- grep("2015-03-29 01:00:00|2016-03-27 01:00:00|2017-03-26 01:00:00|2018-03-25 01:00:00|2019-03-31 01:00:00|2020-03-29 01:00:00|2021-03-28 01:00:00|2022-03-27 01:00:00|2023-03-26 01:00:00",
   #                        myAggreg_i$DateTime)+length(unique(myAggreg_i$ProductionType))
   #  
   # 
   # 
   #  
   # 
   # # myAggreg_i$DateTime <-  myAggreg_i$DateTime[springforward] +  3600
   #  
   #  ToDopl_i <- myAggreg_i[springforward, ]
   #  #year(myAggreg_i$DateTime)
   #  ToDopl_i$DateTime <- rep(myAggreg_i$DateTime[springforward] +  3600,
   #                         length(unique(myAggreg_i$ProductionType)))
   #  
   #  #ToDopl_i$DateTime <- as.POSIXct(strptime(ToDopl_i$DateTime, format = "%Y-%m-%d %H:%M:%S"))
   #  
   #  myAggreg_i <- rbind(myAggreg_i, ToDopl_i)
   #  
   #  myAggreg_i <- myAggreg_i[order(myAggreg_i$DateTime), ]
   #  }
    
    
    myAggreg_i$Types <- NA
    
    myAggreg_i$Types[grep("Fossil", myAggreg_i$ProductionType)] <- "Fossil"
    myAggreg_i$Types[grep("Hydro", myAggreg_i$ProductionType)] <- "Hydro"
    
    
    myAggreg_i$Types[grep("Other|Solar|Waste|Wind|Nuclear", myAggreg_i$ProductionType)] <- "Other"
    myAggreg_i$ProductionType <- ifelse(is.na(myAggreg_i$Types), myAggreg_i$ProductionType, myAggreg_i$Types)
    myAggreg_i$Types <- NULL
    myAggreg_i <- myAggreg_i %>%
        group_by(DateTime, ProductionType) %>%
        summarise(GenerationOutputSum = mean(GenerationOutputSum, na.rm = TRUE),
                  ConsumptionSum = mean(ConsumptionSum, na.rm = TRUE))
    
    
    
    wide_data_i <- myAggreg_i %>%
        tidyr::pivot_wider(names_from = ProductionType,
                           values_from = c(GenerationOutputSum, ConsumptionSum))
    
    
 
    wide_data = bind_rows(wide_data, wide_data_i)
}


#remove data after 15.03.2024

wide_data <- wide_data[wide_data$DateTime <= as.POSIXct("2024-03-15 23:00:00"),]




write.csv(wide_data, file = paste0(path,"/GenerationOutputPerType_clean.csv"))
writexl::write_xlsx(wide_data, path = paste0(path,"/GenerationOutputPerType_clean.xlsx"))


