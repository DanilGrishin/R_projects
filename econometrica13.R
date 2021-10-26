ARIMA Красиво рисуем
library(forecast)#
library(urca)#
library(tseries)#
library(ggplot2)
library(rio)
library (tseries)
library(lmtest)
library (car)
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')
plot(diff(data),ylab='Differenced Tractor Sales')
plot(log10(data),ylab='Log (Tractor Sales)')
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')
#Удобно смотреть
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')
#Строим модель
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
#Прогнозируем
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
#Красиво рисуем
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')
#Остатки
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')

