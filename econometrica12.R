# Модель  ARIMA(p,d,q)
library(forecast)#
library(urca)#
library(tseries)#
library(ggplot2)
library(rio)
library (tseries)
library(lmtest)
library (car)
# Открыть Файл Rezerv.xlsx

m_mnk<-lm(data= Rezerv,Rezerv_RF~week)# Линейный тренд
summary(m_mnk)
Trend<-m_mnk$fitted.values
Rezerv$S1<-Rezerv$Rezerv_RF-Trend #Удалим детерминированный тренд
plot(Rezerv$Rezerv_RF,type = 'l')
plot(Rezerv$S1,col="red",type = 'l')

data_Rezerv <- ts(Rezerv$Rezerv_RF, start = c(2017, 44), 
                  frequency =  52)
data_ts <- ts(Rezerv$S1, start = c(2017, 44), 
              frequency =  52)
autoplot(data_Rezerv) # Исходный ряд
autoplot(data_ts)     # Выровненный ряд
tsdisplay(data_Rezerv)
tsdisplay(data_ts)

my_time_series <- tsclean(data_ts)# Удалим аномальные значения
autoplot(my_time_series)
ggtsdisplay(my_time_series)# Ряд нестационарный Почему?
adf.test(my_time_series)#Проверим на стационарность тестом Дики-Фуллера
# p-value - должно быть меньше чем 0,05 - это значит, что ряд стационарный
plot(decompose(my_time_series)) #Есть сезоннсть и стохастический тренд
ggtsdisplay(diff(my_time_series))# Возьмем первую конечную разность и посмотрим
plot(decompose(diff(my_time_series)))
adf.test(diff(my_time_series))#Проверим на стационарность
#Оценим несколько конкурирующих моделей:
#ARIMA(2,1,2) ARIMA(1,1,1) ARIMA(2,1,0)  ARIMA(0,1,2) ARIMA(1,1,2) ARIMA(2,1,1)
M212<-Arima(my_time_series,order=c(2,1,2))
coeftest(M212)#
M111<-Arima(my_time_series,order=c(1,1,1))
coeftest(M111)
M210<-Arima(my_time_series,order=c(2,1,0))
coeftest(M210)#
M012<-Arima(my_time_series,order=c(0,1,2))
coeftest(M012)#
M112<-Arima(my_time_series,order=c(1,1,2))
coeftest(M112)
M211<-Arima(my_time_series,order=c(2,1,1))
coeftest(M211)
M310<-Arima(my_time_series,order=c(3,1,0))
coeftest(M310)#
M013<-Arima(my_time_series,order=c(0,1,3))
coeftest(M013)
#Сравним модели с помощью информационных критериев
AIC(M212,M210,M012,M310)
BIC(M111,M210,M012,M310)
#Еще раз значимость
coeftest(M310)#
coeftest(M210)#

pacf(residuals(M210))
pacf(residuals(M310))

# Ошибки модели
accuracy(M210) 
accuracy(M310) 

# Анализ остатков
# На отсутствие автокорреляции
Box.test(residuals(M310), type="Ljung-Box")

#На нормальность остатков
qqPlot(residuals(M310),distribution = "norm")

xfit <- seq(min(residuals(M310)), max(residuals(M310)), length = 100) # Координаты по оси X 
yfit <- dnorm(xfit, mean = mean(residuals(M310)), sd = sd(residuals(M310))) # Вычисление координат по оси Y
hist(residuals(M310), freq = FALSE)
lines(density(residuals(M310)), col = "red") # Накладываем кривую плотностей вероятности
lines(xfit, yfit, col = "blue") # Накладываем «нормальную» кривую </syntaxhighlight> 

shapiro.test(residuals(M310))
jarque.bera.test(residuals(M310))

autoplot(residuals(M310))
#Прогноз
prog_M310<-forecast(M310,h=1)  
autoplot(forecast(M310,h=1))
xprognoz<-data.frame(week=133)
predict(m_mnk, xprognoz)
(Result<-prog_M310$mean+predict(m_mnk, xprognoz))

#Подарок для тех, кто дошел до конца:)
auto.arima(my_time_series,seasonal = TRUE)
