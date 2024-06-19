
rm(list = ls())
library(tidyverse)
library(kableExtra)
library(lubridate)
library(readxl)
library(readODS)
library(data.table)
library(dplyr)

path <- getwd()
prefix <- paste0(path, "/New Data")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A. 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#names of datasets
Daten = list.files(file.path(prefix, 'DayAheadPrices_12.1.D'))

#from 2015 01 to 2024 03 2:112
#first 2015 01
Data = fread(file = file.path(prefix, 'DayAheadPrices_12.1.D', Daten[2]))

#table(Data$MapCode)

Data$DateTime <- as.POSIXct(Data$DateTime)
#First <- Data[Data$DateTime <= as.POSIXct("2015-01-04 23:00:00"), ]
#table(First$MapCode)

#choose Germany
Data <- Data[grep("DE", Data$MapCode), ]
Data <- Data[order(Data$DateTime),]

#custom_tz = "Etc/GMT-1"
#Data$DateTime = ymd_hms(Data$DateTime)
#Data$DateTime = with_tz(Data$DateTime, custom_tz)


#Data <- Data[-grep("PT15M", Data$ResolutionCode),]




######################################################################################


#same procedure in a loop for all months
for (i in 3:45) {
    Data_i = fread(file = file.path(prefix, 'DayAheadPrices_12.1.D', Daten[i]))
    
    Data_i = Data_i[grep("DE", Data_i$MapCode), ]
    Data_i = Data_i[order(Data_i$DateTime),]
    
    #custom_tz = "Etc/GMT-1"
    #Data_i$DateTime = ymd_hms(Data_i$DateTime)
    #Data_i$DateTime = force_tz(Data_i$DateTime, custom_tz)
    
    Data = bind_rows(Data, Data_i)
}

for (i in 46:112) {
    Data_i = fread(file = file.path(prefix, 'DayAheadPrices_12.1.D', Daten[i]))
    
    Data_i = Data_i[grep("DE", Data_i$MapCode), ]
    Data_i = Data_i[order(Data_i$DateTime),]
    Data_i = Data_i[grep("PT60M", Data_i$ResolutionCode),]
   
    #custom_tz = "Etc/GMT-1"
    #Data_i$DateTime = ymd_hms(Data_i$DateTime)
    #Data_i$DateTime = force_tz(Data_i$DateTime, custom_tz)

    Data = bind_rows(Data, Data_i)
}


#remove data after 15.03.2024

Data <- Data[Data$DateTime <= as.POSIXct("2024-03-16 00:00:00"),]




write.csv(Data, file = paste0(prefix,"/Prices.csv"))
writexl::write_xlsx(Data, path = paste0(prefix,"/Prices.xlsx"))


