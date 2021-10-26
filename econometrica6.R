#R Studio: Тест Рамсея

library(lmtest) # линейные регрессии 
library("ggplot2") # графики
library("dplyr") # манипуляции с таблицами
library(glmnet) # линейные регрессии 
library(tidyverse)# обработка данных
library(car)# для vif

Data <- read.csv("concrete.csv",sep = ";",header = TRUE)
M1 <- lm(data = Data, q~l+k)
summary(M1)
(r_2 <- summary(M1)$r.squared)

fitted(M1)
Data$y2<-M1$fitted.values^2 # fitted.values Остатки нашей регрессии
Data$y3<-M1$fitted.values^3
M2 <- lm(data = Data, q~.)
summary(M2)

RSS_R <- deviance(M1)
RSS_UnR <- deviance(M2)

q<-2# число ограничений y2 и y3
n<-nrow(Data) # число наблюдений
k<-5# число переменных в длинной модели + свободный член уравнения
F_Ramsey<-((RSS_R-RSS_UnR)/q)/(RSS_UnR/(n-k))

help("resettest")

(resettest(M1,power=2:3)) # будет в кр
qf(0.95,q,n-k) # F-табличное

 # P-value - вероятность того, что F табличное больше, чем F расчетное
# H0 - модель специфицирована верно, нет пропущ переме
# H1 - есть пропущенные переменные
# Если п валюе меньше чем 0.05, H0 отвергается, есть пропущенные переменные
# Если п валюе больше чем 0,05, то принимается
# Если Эф расч меньше, чем Эф табл, то H0 принимается, нет пропущенных переменных
