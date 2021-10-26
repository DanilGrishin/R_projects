library(knitr)
library("tidyverse")
library(ggplot2)
library(plm)
#File – Import data set – From Excel – Guns.xls
head(Guns)

Guns_panel <- pdata.frame(Guns,
                          index = c("stateid", "year"),
                          row.names = TRUE)
Guns_panel$l_vio<-log(Guns_panel$vio) # Логарифмируем, что сгладить, стабилизировать дисперсию
m.pooled <- plm(l_vio ~ shall + incarc_rate+density+avginc+pop+pw1064+pb1064, 
                data = Guns_panel, model = "pooling")
summary(m.pooled)

m.fe <- plm(l_vio ~ shall +incarc_rate+density+avginc+pop+pw1064+pb1064, 
            data = Guns_panel, model = "within")
summary(m.fe)
m.re <- plm(l_vio ~ shall + incarc_rate+density+avginc+pop+pw1064+pb1064, 
            data = Guns_panel, model = "random")
summary(m.re)
# Тест Фишера
pFtest(m.fe, m.pooled)
# Тест Хаусмана
phtest(m.fe, m.re)
#Тест Бройша-Пагана
plmtest(m.re, type = "bp")
