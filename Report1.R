rm(list = ls())



install.packages("lubridate")
library(lubridate)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("zoo")
library(zoo)

install.packages("timeDate")
library(timeDate)


getwd()

############################################################ read data from Case Studies24 Data folder a)



######################################### Actual consumption


# contains start date, end date, total load, residual load, hydro pumped storage

# difference between total load and residual load:

# total load represents the overall electricity demand within a system, residual load specifically
# accounts for the remaining demand after subtracting the contribution of variable renewable energy
# sources




ActCon= read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/Actual_consumption_201501010000_202404012359_Hour.csv", 
                 header = T, sep = ";")





########################################################################################## ActCon 6)


# All data is in character format
# ill turn total load, residual load and hydro to float
# and startdate and enddate to datetime or sth




###################     chr to float

ActCon$Total..grid.load...MWh..Calculated.resolutions = as.numeric(gsub(",", "", 
                  ActCon$Total..grid.load...MWh..Calculated.resolutions))


ActCon$Residual.load..MWh..Calculated.resolutions = as.numeric(gsub(",", "", 
 ActCon$Residual.load..MWh..Calculated.resolutions))


ActCon$Hydro.pumped.storage..MWh..Calculated.resolutions = as.numeric(gsub(",", "", 
     ActCon$Hydro.pumped.storage..MWh..Calculated.resolutions))


############### chr to date-time with lubridate


ActCon$Start.date = mdy_hm(ActCon$Start.date)

ActCon$End.date = mdy_hm(ActCon$End.date)


######################## setting up german time zone


custom_tz = "Etc/GMT-1"

ActCon$CTZDate = force_tz(ActCon$Start.date, custom_tz)





################# filtering actcon for 01.01.15 - 15.03.24
################ Country is already Germany 



ActCon = subset(ActCon, subset = CTZDate <  as.POSIXct("2024-03-16 00:00:00"))




######### Dates of Zeitumstellung (from timeanddate):

######### 29.03.15 & 25.10.15
######### 27.03.16 & 30.10.16
######### 26.03.17 & 29.10.17
######### 25.03.18 & 28.10.18
######### 31.03.19 & 27.10.19
######### 29.03.20 & 25.10.20
######### 28.03.21 & 31.10.21
######### 27.03.22 & 30.10.22
######### 26.03.23 & 29.10.23

################################### first Adjustment: 2015

# manually including the missing hour from the “spring forward” (2 am of the respective date) 
# and duplicating the load value from the previous hour (1am);


### finding out which row to replicate and replicating it

FirstAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2015-03-29 01:00:00")))

### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

FirstAdjustment$CTZDate = FirstAdjustment$CTZDate + 3600





# binding FirstAdjustment and ActCon
ActCon <- bind_rows(FirstAdjustment,ActCon)


## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 7132

which(ActCon$CTZDate > as.POSIXct("2015-10-25 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2015-10-25 03:00:00"))[2]

## manually deleting the duplicate hour

### dropping row 7132

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2015-10-25 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2015-10-25 03:00:00"))[2])


################################### second Adjustment: 2016



### finding out which row to replicate and replicating it

SecondAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2016-03-27 01:00:00")))

### adding an hour to turn "2016-03-27 01:00:00 +01" into "2016-03-27 02:00:00 +01"

SecondAdjustment$CTZDate = SecondAdjustment$CTZDate + 3600




# binding FirstAdjustment and ActCon
ActCon <- bind_rows(SecondAdjustment,ActCon)


## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)




#### finding out which row to drom for "fall back" -> its 16036

which(ActCon$CTZDate > as.POSIXct("2016-10-30 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2016-10-30 03:00:00"))[2]




### dropping row 16036

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2016-10-30 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2016-10-30 03:00:00"))[2])



################################### third Adjustment: 2017



###### finding out which row is to be duplicated
###### its 19562


### finding out which row to replicate and replicating it

ThirdAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2017-03-26 01:00:00")))

### adding an hour to turn "2016-03-27 01:00:00 +01" into "2016-03-27 02:00:00 +01"

ThirdAdjustment$CTZDate = ThirdAdjustment$CTZDate + 3600




# binding FirstAdjustment and ActCon
ActCon <- bind_rows(ThirdAdjustment,ActCon)


## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)



#### finding out which row to drom for "fall back" -> its 24772

which(ActCon$CTZDate > as.POSIXct("2017-10-29 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2017-10-29 03:00:00"))[2]



### dropping row 24772

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2017-10-29 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2017-10-29 03:00:00"))[2])


################################### fourth Adjustment: 2018



### finding out which row to replicate and replicating it

FourthAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2018-03-25 01:00:00")))

### adding an hour to turn "2016-03-27 01:00:00 +01" into "2016-03-27 02:00:00 +01"

FourthAdjustment$CTZDate = FourthAdjustment$CTZDate + 3600



# binding FirstAdjustment and ActCon
ActCon <- bind_rows(FourthAdjustment,ActCon)


## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)





#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2018-10-28 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2018-10-28 03:00:00"))[2]




### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2018-10-28 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2018-10-28 03:00:00"))[2])



################################### fifth Adjustment: 2019

### finding out which row to replicate and replicating it

FifthAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2019-03-31 01:00:00")))

### adding an hour 

FifthAdjustment$CTZDate = FifthAdjustment$CTZDate + 3600


# binding FirstAdjustment and ActCon
ActCon <- bind_rows(FifthAdjustment,ActCon)

## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2019-10-27 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2019-10-27 03:00:00"))[2]


### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2019-10-27 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2019-10-27 03:00:00"))[2])


################################### sixth Adjustment: 2020


### finding out which row to replicate and replicating it

SixthAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2020-03-29 01:00:00")))

### adding an hour 

SixthAdjustment$CTZDate = SixthAdjustment$CTZDate + 3600


# binding FirstAdjustment and ActCon
ActCon <- bind_rows(SixthAdjustment,ActCon)

## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2020-10-25 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2020-10-25 03:00:00"))[2]


### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2020-10-25 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2020-10-25 03:00:00"))[2])


################################### seventh Adjustment: 2021

### finding out which row to replicate and replicating it

SeventhAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2021-03-28 01:00:00")))

### adding an hour 

SeventhAdjustment$CTZDate = SeventhAdjustment$CTZDate + 3600


# binding FirstAdjustment and ActCon
ActCon <- bind_rows(SeventhAdjustment,ActCon)

## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2021-10-31 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2021-10-31 03:00:00"))[2]


### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2021-10-31 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2021-10-31 03:00:00"))[2])

################################### eigth Adjustment: 2022

### finding out which row to replicate and replicating it

EigthAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2022-03-27 01:00:00")))

### adding an hour 

EigthAdjustment$CTZDate = EigthAdjustment$CTZDate + 3600


# binding FirstAdjustment and ActCon
ActCon <- bind_rows(EigthAdjustment,ActCon)

## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2022-10-30 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2022-10-30 03:00:00"))[2]


### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2022-10-30 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2022-10-30 03:00:00"))[2])


################################### Nineth Adjustment: 2023


### finding out which row to replicate and replicating it

NinethAdjustment = slice(ActCon, which(ActCon$CTZDate == as.POSIXct("2023-03-26 01:00:00")))

### adding an hour 

NinethAdjustment$CTZDate = NinethAdjustment$CTZDate + 3600


# binding FirstAdjustment and ActCon
ActCon <- bind_rows(NinethAdjustment,ActCon)

## rearranging df wrt CTZDate

ActCon = ActCon %>%
  arrange(CTZDate)


#### finding out which row to drom for "fall back" -> its 

which(ActCon$CTZDate > as.POSIXct("2023-10-29 02:00:00") & 
        ActCon$CTZDate < as.POSIXct("2023-10-29 03:00:00"))[2]


### dropping row 

ActCon = slice(ActCon, -which(ActCon$CTZDate > as.POSIXct("2023-10-29 02:00:00") & 
                                ActCon$CTZDate < as.POSIXct("2023-10-29 03:00:00"))[2])




plot( ActCon$CTZDate, ActCon$Total..grid.load...MWh..Calculated.resolutions, type = "l")



############################# task c)


## mean
mean(ActCon$Total..grid.load...MWh..Calculated.resolutions)

## variace
var(ActCon$Total..grid.load...MWh..Calculated.resolutions)

## std.dev.

sqrt(var(ActCon$Total..grid.load...MWh..Calculated.resolutions))

## ACF

acf(ActCon$Total..grid.load...MWh..Calculated.resolutions, lag.max = 50, type = "correlation")

## was sind die blauen linien?
## signifikanzniveau: 1.96/ sqrt(n)



pacf(ActCon$Total..grid.load...MWh..Calculated.resolutions, lag.max = 50)

## all lags seem to be sigificant 


####################### task d)





p = 1:50

results <- data.frame(lag_order = integer(), AIC = numeric(), stringsAsFactors = FALSE)


# Loop over different AR orders
for (p in 1:50) {
  # Fit the AR model with the current order
  
  fit = ar(ActCon$Total..grid.load...MWh..Calculated.resolutions, aic = FALSE, 
           method = "ols", order.max = p, intercept = TRUE,demean = FALSE)
  
  # Extract the AIC value
  aic_value =   
(length(fit$resid) - fit$order) * log(sum((fit$resid[(fit$order + 1) :length(fit$resid)])^2) / (length(fit$resid) - fit$order)) + 2 * (fit$order)
  
  # Store the order and AIC in the results dataframe
  results <- rbind(results, data.frame(lag_order = p, AIC = aic_value))
}



ggplot(results, aes(x = lag_order, y = AIC)) +
  geom_point(color = 'blue', size = 3) +
  labs(title = "Akaike Information Criterion of AR(p) fit ",
       x = "Lag Order p",
       y = "Akaike Information Criterion")






########### e)

ar_model =  ar(ActCon$Total..grid.load...MWh..Calculated.resolutions, aic = FALSE, 
                method = "ols", order.max = 1, intercept = TRUE, demean = FALSE)



ar_model$x.intercept
ar_model$ar


predict = as.numeric(0)

for (i in 1: length(ActCon$Total..grid.load...MWh..Calculated.resolutions)){
  
  predict[i+1] = ar_model$x.intercept +
    ar_model$ar * ActCon$Total..grid.load...MWh..Calculated.resolutions[i]
  
  
}

# der erste forecast value ist für 2015-01-01 01:00:00 aus 2015-01-01 00:00:00
# deshalb muss predict mit 0 afangen
# weil es für 2015-01-01 00:00:00 keinen forecast gibt


# der letzte forecast ist für 2024-03-16 00:00:00 aus 2024-03-15 23:00:00
# da habe ich aber nicht der actual value (mit entsprechendem start date)
# um den MSFE zu berechen
# daher muss der letzte DAtenpunkt auch raus

predict =data.frame(predict[(ar_model$order + 1):length(ActCon$Total..grid.load...MWh..Calculated.resolutions) -1 ])


predict$dateUTC = ActCon$CTZDate[2:length(ActCon$CTZDate)]


testdataframe = data.frame(predict, ActCon[2:length(ActCon$Total..grid.load...MWh..Calculated.resolutions), "Total..grid.load...MWh..Calculated.resolutions"])

testdataframe_2 = subset(testdataframe, dateUTC >= as.POSIXct("2023-04-07 07:00:00")
                         & dateUTC <= as.POSIXct("2023-04-14 07:00:00"))



ggplot(testdataframe_2, aes(x = dateUTC, y = predict..ar_model.order...1..length.ActCon.Total..grid.load...MWh..Calculated.resolutions....)) +
  geom_line(color = 'blue', linewidth = 1,linetype = "dashed") +
  geom_line(aes(y = ActCon.2.length.ActCon.Total..grid.load...MWh..Calculated.resolutions...), 
            color = 'black', linewidth = 1, alpha = 0.2) +
  labs(title = " AR(1) Forecast ",
       x = "time",
       y = "Load")











ar_model_2 =  ar.ols(ActCon$Total..grid.load...MWh..Calculated.resolutions, aic = FALSE, 
                   method = "ols", order.max = 5, intercept = TRUE, demean = FALSE)

# ob ar.ols oder ar(method = "ols"), der intercept bleibt derselbe 








############################################################################### my group work part 

####################################### gfs weather


Weather = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/gfs weather.csv", 
                   header = T, sep = ",")



# filter for DE

DEWeather = subset(Weather, Country == "DE")

str(DEWeather)

# oprun is num

########################################## converting trading day to date data type


DEWeather$TradingDateUtc = ymd_hms(DEWeather$TradingDateUtc)

######################## setting up german time zone


DEWeather$CTZDate = force_tz(DEWeather$TradingDateUtc, custom_tz)




########### 2017-04-15, 2017-04-16 , 2015-03-28 und andere sind NA
########### weil ich zb in DEWeather 0prun(2017-04-18) = 4.03224
########## und eine Zeile darunter 0prun(2017-04-18) = NA
########### der durchschnitt wird NA
########### weil ein NA in die Summe kommt
########## ich muss entweder DAten droppen oder NA mit 0 ersetzten, aber ganz am Anfang, bevor
########## ich die summen bilde



DEWeather <- DEWeather %>% 
  mutate(OperationalRun = ifelse(is.na(OperationalRun), 0, OperationalRun))




# for the same trading day there are multiple average temperatures
# group by trading day and compute average of the averages for each day


newWeather = DEWeather %>%
  group_by(CTZDate) %>%
  summarise(average_temperature = mean(OperationalRun))


## All dates are refering to 12:00:00
## if i just want to replicate each datapoint for all days,
## i need the data points to reference 00:00:00

newWeather$CTZDate = newWeather$CTZDate - 43200


###################################### now i have to replicate each daily observation
###################################### 24 times to harmonize it with my hourly data
###################################### each replicated datapoint needs to be an hour later than the one 
###################################### before for each day



newWeatherExpanded = newWeather %>%
  uncount(24) %>%
  group_by(CTZDate) %>%
  mutate(hourly_time = CTZDate + hours(0:23)) %>%
  ungroup()

# newWeatherExpanded$hourly_time[129240] == ActCon$CTZDate[80688]

# ActCon$CTZDate und newWeatherExpanded$hourly_time have the same format



# sum(is.na(newWeatherExpanded$average_temperature))
# no NAs left




######################################## eex_eua_futures

# EEX EUA (European Union Allowance) futures are financial contracts traded on the 
# European Energy Exchange (EEX) that allow participants to buy or sell allowances to emit 
# one metric ton of carbon dioxide equivalent (CO2e) 



EUAFutures = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/eex_eua_futures.csv", 
                      header = T, sep = ",")







#str(EUAFutures)

# X is int
# Mean is num 
# dates are chr
# i need to turn them to date time 


########################################## converting date columns to date data type




EUAFutures$TRADING_DATE = ymd(EUAFutures$TRADING_DATE)

EUAFutures$DELIVERY_DATE = ymd(EUAFutures$DELIVERY_DATE)



######################## setting up german time zone


EUAFutures$TRADING_DATE = force_tz(EUAFutures$TRADING_DATE, custom_tz)



# i need every trading day only once


EUAFuturesUnique = EUAFutures %>% 
  distinct(TRADING_DATE, .keep_all = TRUE)






### to replicate each row 24 times
### each row must have a timestamp like "yyyy-mm-dd 00:00:00"
### then i can work with +hours(0:23)

## it is already the case

## i need to rearrange the df s.t. it starts with the smallest date and ends with the biggest one


EUAFuturesUnique <- EUAFuturesUnique %>% 
  arrange(TRADING_DATE)


# replicating each row 24 times

EUAFuturesUniqueExpanded = EUAFuturesUnique %>%
  uncount(24) %>%
  group_by(TRADING_DATE) %>%
  mutate(hourly_time = TRADING_DATE + hours(0:23)) %>%
  ungroup()


##### any NA?

# sum(is.na(EUAFuturesUniqueExpanded$MEAN))
# no NAs


# ActCon$CTZDate[80688] == EUAFuturesUniqueExpanded$hourly_time[80688]

# ActCon$CTZDate und EUAFuturesUniqueExpanded$hourly_time have the same format



############################################################################ eex_gas_futures




GasFutures = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/eex_gas_futures.csv", 
                      header = T, sep = ",")

unique(GasFutures$Sub.Region)

unique(GasFutures$Commodity)



# Wikipedia:
# Bis zum 1. Oktober 2021 betrieb THE die beiden bis dato bestehenden Marktgebiete 
# NetConnect Germany und Gaspool; seit dem 1. Oktober 2021 betreibt sie das gesamtdeutsche 
# Marktgebiet Trading Hub Europe.


# I'll drop Gaspool

GasFutures = subset(GasFutures, Sub.Region == "NCG (formerly EGT_H)" | Sub.Region == "THE (Trading Hub Europe)")


########################################## converting trading day to date data type

GasFutures$TradingDateUtc = ymd(GasFutures$TradingDateUtc)


######################## setting up german time zone


GasFutures$TradingDateUtc = force_tz(GasFutures$TradingDateUtc, custom_tz)



## any na?

sum(is.na(GasFutures$SettlementClearing))
which(is.na(GasFutures$SettlementClearing))

# yes!

GasFutures <- GasFutures %>% 
  mutate(SettlementClearing = ifelse(is.na(SettlementClearing), 0, SettlementClearing))

GasFutures[2317 , "SettlementClearing"]


# No NAs left!

################## average over trading day

newGasFutures = GasFutures %>%
  group_by(TradingDateUtc) %>%
  summarise(average_settlement = mean(SettlementClearing))




### to replicate each row 24 times
### each row must have a timestamp like "yyyy-mm-dd 00:00:00"
### then i can work with +hours(0:23)

# newGasFutures$TradingDateUtc[8] +3600

## it is already the case



## i need to rearrange the df s.t. it starts with the smallest date and ends with the biggest one
## it is already the case



# replicating each row 24 times

newGasFuturesExpanded = newGasFutures %>%
  uncount(24) %>%
  group_by(TradingDateUtc) %>%
  mutate(hourly_time = TradingDateUtc + hours(0:23)) %>%
  ungroup()


sum(is.na(newGasFuturesExpanded$average_settlement))


############################################################## cross-border physical flows



CbPhF = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/Cross-border_physical_flows_201501010000_202404012359_Hour.csv", 
                 header = T, sep = ";")


## almost all data is in chr

## there is a lot of "-" --> i'll replace them with 0
## this needs to be done first because otherwise i'll get NAs


# Replace "-" with "0" in all columns

CbPhF = CbPhF %>%
  mutate_all(~ ifelse(. == "-", "0", .))

# now i can turn 3,140.50 to 3140.50 in all columns except of start.date and end.date



CbPhF = CbPhF %>%
  mutate_at(vars(-Start.date, -End.date), ~ as.numeric(gsub(",", "", .)))

# total export is sum of all exports

CbPhF$TotalExp = 
CbPhF$Netherlands..export...MWh..Calculated.resolutions +
CbPhF$Switzerland..export...MWh..Calculated.resolutions +
CbPhF$Denmark..export...MWh..Calculated.resolutions +
CbPhF$Czech.Republic..export...MWh..Calculated.resolutions +
CbPhF$Luxembourg..export...MWh..Calculated.resolutions +
CbPhF$Sweden..export...MWh..Calculated.resolutions +
CbPhF$Austria..export...MWh..Calculated.resolutions +
CbPhF$France..export...MWh..Calculated.resolutions +
CbPhF$Poland..export...MWh..Calculated.resolutions +
CbPhF$Norway..export...MWh..Calculated.resolutions +
CbPhF$Belgium..export...MWh..Calculated.resolutions 

# total import is sum of all imports

CbPhF$TotalImp = 
CbPhF$Netherlands..import...MWh..Calculated.resolutions+
  CbPhF$Switzerland..import...MWh..Calculated.resolutions+
  CbPhF$Denmark..import...MWh..Calculated.resolutions+
  CbPhF$Czech.Republic..import...MWh..Calculated.resolutions+
  CbPhF$Luxembourg..import...MWh..Calculated.resolutions+
  CbPhF$Sweden..import...MWh..Calculated.resolutions +
  CbPhF$Austria..import...MWh..Calculated.resolutions+
  CbPhF$France..import...MWh..Calculated.resolutions +
  CbPhF$Poland..import...MWh..Calculated.resolutions +
  CbPhF$Norway..import...MWh..Calculated.resolutions +
  CbPhF$Belgium..import...MWh..Calculated.resolutions


# TotalNetExp is difference of TotalExp and TotalImp

CbPhF$TotalNetExp = CbPhF$TotalExp + CbPhF$TotalImp


########################################## converting start day to date data type


CbPhF$Start.date = mdy_hm(CbPhF$Start.date)


CbPhF$Start.date[8000]
str(CbPhF)

######################## setting up german time zone


CbPhF$TradingDateUtc = force_tz(CbPhF$Start.date, custom_tz)



## any na?

sum(is.na(CbPhF$TotalNetExp))

## NO !


################################################################################# Meteologica




Meteo = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/meteologica.csv", 
                header = T, sep = ",")


str(Meteo)

### zuerst solar

Solar = subset(Meteo, Technology == "solar")

DESolar = subset(Solar, Area == "DE")


# there are some means = 0 for some days; i think this indicates bad weather:
# if there is no sun, there is no solar energy

# compute averages of mean; average over trading day



################## average over trading day

newDESolar = DESolar %>%
  group_by(TradingDateUtc) %>%
  summarise(average_mean = mean(Mean))

str(newDESolar)

# any na?

sum(is.na(newDESolar$average_mean))

# no NA!

########################################## converting TradingDateUtc to date data type



newDESolar$TradingDateUtc = ymd_hms(newDESolar$TradingDateUtc)


######################## setting up german time zone


newDESolar$CTZ = force_tz(newDESolar$TradingDateUtc, custom_tz)


## All dates are refering to 12:00:00
## if i just want to replicate each datapoint for all days,
## i need the data points to reference 00:00:00

newDESolar$CTZ = newDESolar$CTZ - 43200


# replicating each daily observstion 24 times

newDESolarExpanded = newDESolar %>%
  uncount(24) %>%
  group_by(CTZ) %>%
  mutate(hourly_time = CTZ + hours(0:23)) %>%
  ungroup()





## hier wind

wind = subset(Meteo, Technology == "wind")

# we are supposed to work with german companies

#unique(wind$Area)

# 50hertz is german
# transnetbw is german
# tennet is dutch
# amprion is german
# NL and FR arent DE

DEWind = subset(wind, Area == "50hertz" | Area == "transnetbw" | Area == "amprion")


########################################## converting TradingDateUtc to date data type

DEWind$TradingDateUtc = ymd_hms(DEWind$TradingDateUtc)



######################## setting up german time zone


DEWind$CTZ = force_tz(DEWind$TradingDateUtc, custom_tz)



################## average over fixed_date

 newDEWind = DEWind %>%
   group_by(TradingDateUtc) %>%
   summarise(average_mean = mean(Mean))
 
 
 newDEWind[1:4,]
 
 
######## averaging again over hours as well and keeping first time stamp
 
 newnewWind = newDEWind %>%
   mutate(daydate = date(TradingDateUtc)) %>%   
   group_by(daydate) %>%                        
   summarise(
     TradingDateUtc = first(TradingDateUtc),  
     average_average_mean = mean(average_mean)        
   ) %>%
   ungroup() %>%                              
   select(TradingDateUtc, average_average_mean)       
 
 

### all averages need to have the same date time
 

 nnnWind = newnewWind %>%
   mutate(date = date(TradingDateUtc)) %>%   
   group_by(date) %>%                        
   summarise(
     CTZDate = ymd_hms(paste0(date, " 00:00:00")),  
     average_mean = first(average_average_mean)                      
   ) %>%
   ungroup() %>%                             
   select(CTZDate, average_mean)       
 
 
 # replicating each daily observstion 24 times
 
 nnnWindExpanded = nnnWind %>%
   uncount(24) %>%
   group_by(CTZDate) %>%
   mutate(hourly_time = CTZDate + hours(0:23)) %>%
   ungroup()
 
 
 # any na?
  sum(is.na(nnnWindExpanded$average_mean))
  
  # no NA!
  
  
  
  
  
############################################################### file added by paul
  
  
  
  
WS = read.csv(file = "C:/Users/Acer/Desktop/CS2024/Data/WindSolar.csv", 
                   header = T, sep =";")
  
  
str(WS)

## 47,776.75 zu 47776.75
## auf NAs überprüfen


########################################## converting start day to date data type


WS$Start.date = mdy_hm(WS$Start.date)


######################## setting up german time zone


WS$TradingDateUtc = force_tz(WS$Start.date, custom_tz)


### replace "-" by 0, id the "-" stands alone representing missing data
## and not a minus sign



WS = WS %>%
  mutate(across(-c(Start.date, End.date, TradingDateUtc), ~ ifelse(. == "-", "0", .)))



## alles außer start und date zu numeric und  47,776.75 zu 47776.75


WS = WS %>%
  mutate_at(vars(-Start.date, -End.date, -TradingDateUtc), ~ as.numeric(gsub(",", "", .)))



# any NA?

sum(is.na(WS$Photovoltaics.and.wind..MWh..Calculated.resolutions))

# no NA


WS$WindOnOffShore = WS$Wind.offshore..MWh..Calculated.resolutions + WS$Wind.onshore..MWh..Calculated.resolutions



WS$Daydate = as.Date(WS$Start.date)

################## average over dayate

newWS = WS %>%
  group_by(Daydate) %>%
  summarise(average = mean(WindOnOffShore))



newWS$CTZ = ymd_hms(paste0(newWS$Daydate, " 00:00:00"))

# replicating each daily observstion 24 times

newWSExpanded = newWS %>%
  uncount(24) %>%
  group_by(CTZ) %>%
  mutate(hourly_time = CTZ + hours(0:23)) %>%
  ungroup()

## connecting newWsExpanded and nnnWindExpanded

newWSExpanded = data.frame(newWSExpanded$hourly_time, newWSExpanded$average)



nnnWindExpanded = data.frame(nnnWindExpanded$hourly_time, nnnWindExpanded$average_mean)

colnames(newWSExpanded) = c("time", "avergae")

colnames(nnnWindExpanded) = c("time", "avergae")


Windtotal = bind_rows(newWSExpanded, nnnWindExpanded)


## rearranging df wrt CTZDate

Windtotal = Windtotal %>%
  arrange(time)






############## DST of CBPHF


######### Dates of Zeitumstellung (from timeanddate):

######### 29.03.15 & 25.10.15
######### 27.03.16 & 30.10.16
######### 26.03.17 & 29.10.17
######### 25.03.18 & 28.10.18
######### 31.03.19 & 27.10.19
######### 29.03.20 & 25.10.20
######### 28.03.21 & 31.10.21
######### 27.03.22 & 30.10.22
######### 26.03.23 & 29.10.23

################################### first Adjustment: 2015

# manually including the missing hour from the “spring forward” (2 am of the respective date) 
# and duplicating the load value from the previous hour (1am);


### finding out which row to replicate and replicating it

FirstAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2015-03-29 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

FirstAdjustmentCBF$TradingDateUtc = FirstAdjustmentCBF$TradingDateUtc + 3600



# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(FirstAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)



#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2015-10-25 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2015-10-25 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2015-10-25 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2015-10-25 03:00:00"))[2]
)

##second adjustment: 2016



### finding out which row to replicate and replicating it

SecondAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2016-03-27 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

SecondAdjustmentCBF$TradingDateUtc = SecondAdjustmentCBF$TradingDateUtc + 3600



# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(SecondAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)



#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2016-10-30 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2016-10-30 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2016-10-30 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2016-10-30 03:00:00"))[2]
)



### third adjustment: 2017



### finding out which row to replicate and replicating it

ThirdAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2017-03-26 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

ThirdAdjustmentCBF$TradingDateUtc = ThirdAdjustmentCBF$TradingDateUtc + 3600



# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(ThirdAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)



#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2017-10-29 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2017-10-29 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2017-10-29 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2017-10-29 03:00:00"))[2]
)



## fourth adjustment: 2018



### finding out which row to replicate and replicating it

FourthAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2018-03-25 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

FourthAdjustmentCBF$TradingDateUtc = FourthAdjustmentCBF$TradingDateUtc + 3600



# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(FourthAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)




#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2018-10-28 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2018-10-28 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2018-10-28 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2018-10-28 03:00:00"))[2]
)

######################################## fifth adjustment: 2019


### finding out which row to replicate and replicating it

FifthAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2019-03-31 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

FifthAdjustmentCBF$TradingDateUtc = FifthAdjustmentCBF$TradingDateUtc + 3600


# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(FifthAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)



#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2019-10-27 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2019-10-27 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2019-10-27 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2019-10-27 03:00:00"))[2]
              
)






######################################################### sixth adjustment: 2020


### finding out which row to replicate and replicating it

SixthAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2020-03-29 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

SixthAdjustmentCBF$TradingDateUtc = SixthAdjustmentCBF$TradingDateUtc + 3600


# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(SixthAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)




#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2020-10-25 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2020-10-25 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2020-10-25 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2020-10-25 03:00:00"))[2]
              
              
)

############################# seventh adjustment 2021



### finding out which row to replicate and replicating it

SeventhAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2021-03-28 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

SeventhAdjustmentCBF$TradingDateUtc = SeventhAdjustmentCBF$TradingDateUtc + 3600


# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(SeventhAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)




#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2021-10-31 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2021-10-31 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2021-10-31 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2021-10-31 03:00:00"))[2]
              
              
)


############### eight adjustment: 2022



### finding out which row to replicate and replicating it

EigthAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2022-03-27 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

EigthAdjustmentCBF$TradingDateUtc = EigthAdjustmentCBF$TradingDateUtc + 3600


# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(EigthAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)




#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2022-10-30 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2022-10-30 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2022-10-30 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2022-10-30 03:00:00"))[2]
              
              
)

########################### nineth adjustment: 2023


### finding out which row to replicate and replicating it

NinethAdjustmentCBF = slice(CbPhF, which(CbPhF$TradingDateUtc == as.POSIXct("2023-03-26 01:00:00")))


### adding an hour to turn "2015-03-29 01:00:00 +01" into "2015-03-29 02:00:00 +01"

NinethAdjustmentCBF$TradingDateUtc = NinethAdjustmentCBF$TradingDateUtc + 3600


# binding FirstAdjustmentCBF and CbPhF
CbPhF <- bind_rows(NinethAdjustmentCBF,CbPhF)


## rearranging df wrt CTZDate

CbPhF = CbPhF %>%
  arrange(TradingDateUtc)




#### finding out which row to drom for "fall back" -> its 

which(CbPhF$TradingDateUtc > as.POSIXct("2023-10-29 02:00:00") & 
        CbPhF$TradingDateUtc < as.POSIXct("2023-10-29 03:00:00"))[2]


## manually deleting the duplicate hour



CbPhF = slice(CbPhF, -which(CbPhF$TradingDateUtc > as.POSIXct("2023-10-29 02:00:00") & 
                              CbPhF$TradingDateUtc < as.POSIXct("2023-10-29 03:00:00"))[2]
              
              
)




########################################## cleaning dates and columns

####### Load


## date already done
## columns: CTZDAte and TotalLoad


TotalLoad = data.frame(ActCon$CTZDate, ActCon$Total..grid.load...MWh..Calculated.resolutions)

colnames(TotalLoad) = c("date", "TotalLoad")

##### gfs weather

TotalWeather = data.frame(newWeatherExpanded$hourly_time, newWeatherExpanded$average_temperature)

TotalWeather = subset(TotalWeather, newWeatherExpanded.hourly_time <= as.POSIXct("2024-03-15 23:00:00") & 
                        newWeatherExpanded.hourly_time >= as.POSIXct("2015-01-01 00:00:00"))

colnames(TotalWeather) = c("date", "mean_temp")


#### co2  futures


TotalCO2Futures = data.frame(EUAFuturesUniqueExpanded$hourly_time, EUAFuturesUniqueExpanded$MEAN)

colnames(TotalCO2Futures) = c("date", "Mean_CO2")


TotalCO2Futures = subset(TotalCO2Futures, date <= as.POSIXct("2024-03-15 23:00:00") & 
                          date >= as.POSIXct("2015-01-01 00:00:00"))



### gas futures

TotalGasFutures = data.frame(newGasFuturesExpanded$hourly_time, newGasFuturesExpanded$average_settlement)


colnames(TotalGasFutures) = c("date", "Mean_Gas")


TotalGasFutures = subset(TotalGasFutures, date <= as.POSIXct("2024-03-15 23:00:00") & 
                           date >= as.POSIXct("2015-01-01 00:00:00"))


## cbphf



TotalCB = data.frame(CbPhF$TradingDateUtc, CbPhF$TotalNetExp)


colnames(TotalCB) = c("date", "TotalNetExp")



TotalCB = subset(TotalCB, date <= as.POSIXct("2024-03-15 23:00:00") & 
                           date >= as.POSIXct("2015-01-01 00:00:00"))





### solar

TotalSolar = data.frame(newDESolarExpanded$hourly_time, newDESolarExpanded$average_mean)



colnames(TotalSolar) = c("date", "Mean_Solar")


TotalSolar = subset(TotalSolar, date <= as.POSIXct("2024-03-15 23:00:00") & 
                   date >= as.POSIXct("2015-01-01 00:00:00"))



### wind

TotalWind = subset(Windtotal, time <= as.POSIXct("2024-03-15 23:00:00") & 
                     time >= as.POSIXct("2015-01-01 00:00:00"))


colnames(TotalWind) = c("date", "average_wind")


########################################## imputation gas with actcon


Final = merge(x = TotalLoad, y = TotalGasFutures, by = "date", all.x = TRUE)

## for the very first date of TotalGasFutures i cant use the previous value,
## because there is no data for 2015-01-01
# solution: ill insert the value of 2015-01-02 for 2015-01-01

Final[1:24, "Mean_Gas"] = Final[25, "Mean_Gas"] 


## fill in the previous value

Final$Mean_Gas = na.locf(Final$Mean_Gas)


########################################## imputation gas with actcon with Solar



Final_second = merge(x = Final, y = TotalSolar, by = "date", all.x = TRUE)

is.na(Final_second$Mean_Solar)

sum(is.na(Final_second$Mean_Solar))

which(is.na(Final_second$Mean_Solar))

## erster datapoint ist kein  problem


Final_second$Mean_Solar = na.locf(Final_second$Mean_Solar)


########################################## imputation gas with actcon with Solar with wind



Final_third = merge(x = Final_second, y = TotalWind, by = "date", all.x = TRUE)

is.na(Final_third$average_wind)

sum(is.na(Final_third$average_wind))

which(is.na(Final_third$average_wind))

## für 2015-01-01 00:00:00 gibt es keine daten
## ich befülle mit 2015-01-01 01:00:00


Final_third[1, "average_wind"] = Final_third[2, "average_wind"] 




Final_third$average_wind = na.locf(Final_third$average_wind)


##########################################################


## binding the rest, which is: TotalWeather, TotalCO2Futures, TotalCB



Final_fourth = merge(x = Final_third, y = TotalWeather, by = "date", all.x = TRUE)


Final_fifth = merge(x = Final_fourth, y = TotalCO2Futures, by = "date", all.x = TRUE)



Final_sixth = merge(x = Final_fifth, y = TotalCB, by = "date", all.x = TRUE)

sum(is.na(Final_sixth))


# write.csv(Final_sixth, file = "Lev.csv", row.names = TRUE)



# ttttest = read.csv("Lev.csv")


################################################ dummy variable for holidays

years = c(2015,2016,2017,2018,2019,2020,2021,2022,2023)


Easter = as.Date(Easter(years))

GoodFriday = Easter -2

EasterMonday = Easter +1

AscensionDay = Easter +39

WhitMonday = Easter + 50


NewYearsDay = as.Date(NewYearsDay(years))

ChristmasDay = as.Date(ChristmasDay(years))

BoxingDay = as.Date(BoxingDay(years))


# arbeit 01.05


LabourDay = as.Date(paste0(years, "-05-01"))



# deutsche einheit 03.10

TDDE = as.Date(paste0(years, "-10-03"))


Holidays = data.frame(Easter,GoodFriday, EasterMonday, AscensionDay,  WhitMonday,
                  NewYearsDay, ChristmasDay,BoxingDay, LabourDay,  TDDE)

Final_sixth$date[799]

str(Final_sixth)





is_holiday = function(date) {
     any(date == unlist(Holidays))
   }


Final_sixth$holiday_dummy = as.integer(sapply(Final_sixth$Day, is_holiday))





Final_sixth[ Final_sixth$Day == as.POSIXct("2023-12-27"),]


Final_sixth$Day[8:19]

################################ weekend days


Final_sixth$weekdayNumber = wday(Final_sixth$Day)

# 1 = Sonntag
# 7 = Samstag


Final_sixth$is_weekend = ifelse(Final_sixth$weekdayNumber %in% c(1, 7), 1, 0)


# write.csv(Final_sixth, file = "Lev.csv", row.names = TRUE)



# ttttest = read.csv("Lev.csv")


