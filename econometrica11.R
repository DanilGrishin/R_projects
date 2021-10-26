library(tidyverse)
library(datasets)
library(lubridate)
library(zoo)
library(forecast)
library(urca)
library(tseries)
library(ggplot2)
# Преобразуем считанные данные во ВР
rus_data <- read.csv("rus_data.csv")
class(rus_data) #определим тип данных
rus_data <- select(rus_data, -time)
rus_data_ts <- ts(rus_data, start = c(1995, 1), 
                  frequency =  12)
autoplot(rus_data_ts[,1:3], facets = FALSE)
d1 <- select(rus_data, real_income)

d1_ts<-ts(d1, start = c(1995, 1),frequency =  4)
autoplot(d1_ts)


ggAcf(d1_ts)
ggPacf(d1_ts)
ggtsdisplay(d1_ts)

str(rus_data)
