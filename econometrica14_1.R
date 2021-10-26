library(knitr)
library("tidyverse")
library(ggplot2)
library(plm)





# install.packages('plm') # может быть нужно установить пакеты...
#Берём встроенный в пакет plm набор данных Grunfeld
data(Grunfeld)
head(Grunfeld)
#Указываем R,какая переменная отвечает за i(объекты), какая — за t(время)

h <- pdata.frame(Grunfeld,
                 index = c("firm", "year"),
                 row.names = TRUE)

#Посмотрим на часть таблички:
Grunfeld[12:16, 3:5]
h[12:16, 3:5]
# Добавим лагированные инвестиции
h$linv <- lag(h$inv)
head(h)
# Оценим три модели:


m.pooled <- plm(inv ~ capital + value + linv, data = h, model = "pooling")
summary(m.pooled)
m.re <- plm(inv ~ capital + value + linv, data = h, model = "random")
#  Посмотрим отчёт по модели со случайным эффектами:
summary(m.re)
m.fe <- plm(inv ~ capital + value + linv, data = h, model = "within")
summary(m.fe)

# Константа равна нуля в последней модели, потому что она строится на отклонениях от среднего
#  На самом деле модель сквозной регрессии и 
#  модель с фиксированными эффектами оцениваются обычным МНК. 
# Эти результаты можно воспроизвести без пакета plm своими руками:

m.pooled.lm <- lm(inv ~ capital + value, data = h)
m.fe.lm <- lm(inv ~ capital + value + factor(firm), data = h)

#  Сравним коэффициенты:

coefficients(m.pooled)
coefficients(m.pooled.lm)

coefficients(m.fe)
coefficients(m.fe.lm)

# Проведём три теста на сравнение моделей.

#Фиксированные эффекты против сквозной регрессии.
# Обычный F-тест.
pFtest(m.fe, m.pooled) #P-value < 0 - 
#!!Здесь нулевая гипотеза о верной сквозной модели отвергается в пользу модели с фиксированными эффектами.

#Фиксированные эффекты против случайных эффектов. 
# Тест Хаусмана.
phtest(m.fe, m.re) #P-value > 0 - 
#!!Здесь нулевая гипотеза о состоятельности коэффициентов в обеих моделях (FE и RE) отвергается в пользу гипотезы о том, что в RE модели коэффициенты несостоятельны.

#Случайные эффекты против сквозной регрессии.
#Тест Бройша-Пагана

plmtest(m.re, type = "bp") #P-value < 0 - 
#!!Здесь нулевая гипотеза о верности сквозной регресси, то есть о том, что ci тождественно равны 0, отвергается.

# Для RE-model
#Чем больше z-value, тем больше фактор влияет на нашу зависимую переменную
