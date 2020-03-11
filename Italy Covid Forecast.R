#data linked to from https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv

library(data.table)
library(forecast)
library(tidyverse)
library(dygraphs)
library(lubridate)
library(zoo)
library(RColorBrewer)

italy <- fread('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv')

head(italy)
str(italy)
summary(italy)

#convert time data to time format with lubridate
italy$data <- ymd_hms(italy$data, tz = 'Europe/Vienna')

#build time series of total cases
italy_ts <- ts(italy$totale_casi)
dygraph(italy_ts)

options(scipen = 999)

decompose(italy_ts)

#ARIMA Model
italy_arima <- auto.arima(italy_ts, trace = T, 
                          stepwise = F, 
                          approximation = FALSE, 
                          stationary = F,
                          seasonal = F)
arima_fore <- forecast(italy_arima, h = 90)
dygraph(arima_fore$mean)
autoplot(arima_fore)

#ets model
italy_ets <- ets(italy_ts,damped = TRUE)
ets_fore <- forecast(italy_ets, h = 90)
autoplot(ets_fore)

#holt model; use holt model since data is not seasonal
italy_holt <- holt(italy_ts, h = 90, damped = TRUE)
autoplot(italy_holt)

#create date series for forecasts
mydates <-  seq.Date(from = as.Date(max(italy$data) + days(1)),
                     to = as.Date(max(italy$data) + days(90)),
                     by = 1)
mydates

#combine forecasts to be used with dygraph; combine into a df then convert to a zoo
forecasts <- data.frame('Date' = mydates, 'ARIMA' = arima_fore$mean, 'ETS' = ets_fore$mean)#, 'Holt' = italy_holt$mean)
z.forecast <- zoo(x = forecasts[,-1], order.by = forecasts[,1])

#dygraph forecasts
dygraph(data = z.forecast, main = 'Forecasts of COVID-19 cases in Italy over 90 days', ylab = 'Total Cases') %>% 
  dyOptions(colors = brewer.pal(n = 3, 'Dark2')) %>% 
  dyRangeSelector(height = 20)
