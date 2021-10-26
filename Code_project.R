# Укажем путь к рабочей папке
setwd("C:/Rlib/Opt_portfolio_project_2")

library(quantmod)

from = "2010-01-01"
to = "2021-05-31"
period = "weekly"

# Нельзя одновременно выкачивать данные по российскому и американскому фондовым рынкам
# из-за различий в днях возникают ошибки в таблице, в которой сводятся все показатели
tickers_rus <- c('SBER.ME','GAZP.ME','AFLT.ME','AMEZ.ME','MOEX.ME')
tickers_amer <- c('MSFT','^GSPC')

tickers <- tickers_rus

DF_list <- list()

for (i in seq_along(tickers)){
  DF_list[[i]] <- getSymbols(tickers[i],from=from, to=to, periodicity = period, auto.assign=FALSE)
  names(DF_list)[i] <- tickers[i]
}

library(tidyverse)
library(rio)
library(skimr)
library(dplyr)

max_data <- c()

for (i in seq_along(DF_list)){
  max_data[i] <- length(DF_list[[names(DF_list)[i]]])
}

ccc <- order(-max_data)

DF_ts_list <- list()
DF_ts_list[[1]] <- DF_list[[names(DF_list)[ccc[1]]]]


for (i in seq_along(ccc[-1])){
  DF_ts_list[[i+1]] <- merge(DF_ts_list[[i]],DF_list[[names(DF_list)[ccc[i+1]]]],join = 'left',fill='NA')
}

DF_ts_data <- as.data.frame(DF_ts_list[[length(DF_ts_list)]])

col_names <- tibble(mn = colnames(DF_ts_data)) %>% 
  slice(str_which(mn, "Close")) # Поиск столбцов, в названии которых содержится "Close"

col_names_indexs <- c()
for (i in seq_along(col_names$mn)){
  col_names_indexs[i] <- which(colnames(DF_ts_data)==as.character(col_names[i,1]))
}

data <- DF_ts_data[col_names_indexs[1]]
for (i in seq_along(col_names_indexs[-1])){
  data <- cbind(data,DF_ts_data[col_names_indexs[i+1]])
}

ll <- length(colnames(data))
for (i in seq_along(colnames(data))){
  data[i+ll] <- (data[i]/lag(data[i]))-1
  colnames(data)[i+ll] <- paste0(colnames(data)[i],"_ret")
}

data <- na.omit(data)

model1 <- lm(data = data, SBER.ME.Close_ret ~ MOEX.ME.Close_ret)
summary(model1)

# Альтернативный способ рассчитать коэффициент бета (НЕ УДАЛЯТЬ!!!)
# cov(data$SBER.ME.Close_ret, data$MOEX.ME.Close_ret)/var(data$MOEX.ME.Close_ret) # (НЕ УДАЛЯТЬ!!!)

library(rugarch)
library(rmgarch)

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)),variance.model = list(garchOrder = c(1,1),
                          model = "sGARCH"),distribution.model = "norm")

spec1 = dccspec(uspec = multispec( replicate(2,uspec) ),dccOrder = c(1,1),distribution = "mvnorm")

# dcc.garch11.spec

index <- data$MOEX.ME.Close_ret
asset <- data$AFLT.ME.Close_ret
data2 <- data.frame(index,asset)
rownames(data2) <- rownames(data)
data2 <- as.xts(data2)

dcc_fit = dccfit(spec1, data2)

# class(dcc.fit)
# slotNames(dcc.fit)
# names(dcc.fit@mfit)
# names(dcc.fit@model)

dcc_fit
plot(dcc_fit)

dcc.fcst = dccforecast(dcc_fit, n.ahead=100)

# class(dcc.fcst)
# slotNames(dcc.fcst)
# class(dcc.fcst@mforecast)
# names(dcc.fcst@mforecast)

dcc.fcst

plot(dcc.fcst)
