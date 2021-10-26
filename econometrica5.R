#Создадим рабочую папку (название папки на англ. языке)
#Session-Set working directory- Choose directory

library("lmtest") # линейные регрессии 
library("ggplot2") # графики
library("dplyr") # манипуляции с таблицами
library("glmnet") # линейные регрессии 
library("tidyverse")# обработка данных
library("car")# для vif

Data <- read.csv("concrete.csv",sep = ";",header = TRUE) # тип .csv, но открывается в Excel
M1 <- lm(data = Data, q~l)
M1
summary(M1)
qplot(data = Data,l,q)
qplot(data = Data,l,q) + stat_smooth(method = "lm")   
#Прогнозирование
nd <- data.frame(l = c(min(Data$l)))
nd
(predict(M1, nd)) 
nd <- data.frame(l = c(min(Data$l) + 0.2*mean(Data$l),max(Data$l) - 0.25*mean(Data$l)))
nd
(predict(M1, nd))
confint(M1) # доверительные интервалы
vcov(M1) 


M2 <- lm(data = Data, q~l+k)
summary(M2)


coef(M2)
vcov(M2)
residuals(M2)
fitted(M2)
help(fitted)
#(RSS <- deviance(M2))
(RSS <- deviance(M2))
(TSS <- sum((Data$q - mean(Data$q))^2))
(ESS <- TSS - RSS)
(R2 <- ESS/TSS)
(r_2 <- summary(M2)$r.squared)

summary(M2)
qplot(data = Data, l,q) + stat_smooth(method = "lm")

nd <- data.frame(l = min(Data$l) + 0.2*mean(Data$l),k = 12000)
nd
predict(M2, nd,interval = "prediction")
confint(M2)

Matr <- as.matrix(Data)
cor(Matr) # корреляция
M2.vif <- lm(data = Data, l~k)
summary(M2.vif)
r2 <- summary(M2.vif)$r.squared
vif <- 1/(1 - r2)
vif
vif(M2)


# Загрузка файла .csv  НЕ просматривается в Excel в виде таблицы

# Открыть файл clothing.csv с помощью Блокнота
# Посмотреть, какие разделители, есть ли заголовок, 
# Сохранить файл в формате txt (с разделителями- запятая)

data1 <- read.csv("clothing.txt",sep = ",",header = TRUE)

# !!! замечание rm (data)- удалить конкретные данные data

(M3 <- lm(data = data1, tsales~.))
summary(M3)
vif(M3)
data2 <- data1[,-8]                 
M4 <- lm(data = data2, tsales~.)
summary(M4)
vif(M4)# если меньше 10, то хорошо. Больше 10 плохо, больше 20 - очень плохо

data3 <- data2[,-3]
data3 <- data3[,-8]
data3 <- data3[,-8]
data3 <- data3[,-9]

M5 <- lm(data = data3, tsales~.)
summary(M5)
vif(M5)# если меньше 10, то хорошо. Больше 10 плохо, больше 20 - очень плохо

data3 <- data3[,-3]
M6 <- lm(data = data3, tsales~.)
summary(M6)
vif(M6)# если меньше 10, то хорошо. Больше 10 плохо, больше 20 - очень плохо


Загрузка данных из файлов 
Создадим рабочую папку (название папки на англ. языке)
Session→Set working directory→Choose directory

Сохранить файл в формате txt (с разделителями табуляции)
Выгрузить файл в R
f<-read.csv("nazvanie faila.txt", sep = "\t", dec = ".", header = TRUE)	

