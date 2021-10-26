library(mfx)  # расчет предельных эффектов
library(vcd)  # графики для качественных данных
library(reshape2)  # манипуляции с данными
library(skimr) # описательные статистики
library(AUC)  # для ROC кривой
library(rio) # импорт файлов разных форматов
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc
library(dplyr) # манипуляции с таблицами
library (ggplot2) 
library(glmnet)
search() # показывает, какие библиотеки загружены

# при загрузке файлов R автоматом переделывает все строковые переменные в
# факторные эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- import("titanic3.csv")

glimpse(t)
#Если не работает, то используем str
#str(t)

# объясняем R, какие переменные считать факторными
# mutate_at не всегда работает
t <- dplyr::mutate_at(t, vars(sex, pclass, survived, embarked), factor)
# или так
t$sex<-as.factor(t$sex)
t$pclass<-as.factor(t$pclass)
t$survived<-as.factor(t$survived)
t$embarked<-as.factor(t$embarked)
str(t)


summary(t)
# мозаичный график shade = TRUE - цветной
#cоотношение между качественными переменными в наборе данных
mosaic(data = t, ~sex + pclass + survived, shade = TRUE)
# график-виолончель
#cоотношение между количественными переменными в наборе данных
qplot(data = t, x = survived, y = age, geom = "violin")


#y=... посчитает сам количество наблюдений, fill =закрасим выжил/ не выжил
#см справа на кончике графика, пожилой пассажир выжил, 
#stack - плотность по верикали 
ggplot(data = t, aes(x = age, y = ..count.., fill = survived)) + geom_density(position = "stack")

# fill -условная вероятность, соотношение по вертикали
ggplot(data = t, aes(x = age, y = ..count.., fill = survived)) + geom_density(position = "fill")




# Оценивание логит и пробит моделей
m_logit <- glm(data = t, survived ~ sex + age + pclass + fare, family = binomial(link = "logit"),
               x = TRUE)
# x = TRUE сохраним исходные регрессоры в оцененной модели 
m_probit <- glm(data = t, survived ~ sex + age + pclass + fare, family = binomial(link = "probit"),
                x = TRUE)
# отчеты об оценке моделей
summary(m_logit)
summary(m_probit)

# оценка ковариационной матрицы оценок коэффициентов
vcov(m_logit)

# создаём новый массив данных для прогнозирования
newdata <- data.frame(age = seq(from = 5, to = 100, length = 100), sex = "male",
                      pclass = "2nd", fare = 100)

# посмотрим на начало этой таблички
head(newdata)

# прогнозируем по логит модели
pr_logit <- predict(m_logit, newdata, se = TRUE)
# соединим прогнозы и новый массив данных в единую табличку:
newdata_pr <- cbind(newdata, pr_logit)
head(newdata_pr)  # глянем на начало таблички

# применив логистическую функцию распределения получим границы доверительного
# интервала
newdata_pr <- mutate(newdata_pr, prob = plogis(fit), left_ci = plogis(fit - 1.96 *
                                                                        se.fit), right_ci = plogis(fit + 1.96 * se.fit))
# 1.96 - откуда берется??? Это z-статистика для 95% доверительного интервала

head(newdata_pr)  # глянем на результат

# посмотрим на графике как меняется доверительный интервал для вероятности
qplot(data = newdata_pr, x = age, y = prob, geom = "line") + geom_ribbon(aes(ymin = left_ci,
                                                                             ymax = right_ci), alpha = 0.2)


#  LR тест сравнение вложенных моделей: нужно включать в модель финансовые переменные?
# создадим набор данных t2 без пропущенных значений и на нем оценим короткую и
# длинную модели H0: beta(pclass)=0, beta(fare)=0
t2 <- dplyr::select(t, sex, age, pclass, survived, fare) %>% na.omit()
# если команда select не работает, возможно подключен пакет, переопределяющий команду select
# чтобы решить проблему, вместо select() явно пишем dplyr::select()

# оцениваем ограниченную модель
m_logit2 <- glm(data = t2, survived ~ sex + age, 
                family = binomial(link = "logit"), x = TRUE)
summary(m_logit2)

# проводим LR тест
lrtest(m_logit, m_logit2)
# предельные эффекты для среднестатистического пассажира
logitmfx(data = t, survived ~ sex + age + pclass + fare)
# усредненные предельные эффекты по всем пассажирам
logitmfx(data = t, survived ~ sex + age + pclass + fare, atmean = FALSE)
# обычный МНК
m_ols <- lm(data = t, as.numeric(survived) ~ sex + age + pclass + fare)
summary(m_ols)
# прогнозы по обычному МНК
pr_ols <- predict(m_ols, newdata)
head(pr_ols)

#Сравним прогнозы для конкретного человека
m_logit3 <- glm(data = t2, survived ~ sex + age+pclass+fare, 
                family = binomial(link = "logit"))
nd<-data.frame(sex="female",age=55,pclass="2nd",fare=mean(t2$fare))
# Метод  Мах правдоподобия
r_logit3 <- (predict(m_logit3, nd, se = TRUE))
r_logit3$fit         # Предсказанная скрытая переменная
plogis(r_logit3$fit) # Вероятность survived=1
# Метод наименьших квадратов 
(pr_ols <-predict(m_ols, nd))
1/(1+exp(-pr_ols))  # Вероятность survived=1
######################################
#Сравним модели
Log_M<-predict(m_logit3, t2, se = TRUE)
MNK_M<-predict(m_ols,t2)
data<-data.frame(Y=t2$survived,LOGIT=plogis(Log_M$fit),MNK=1/(1+exp(-MNK_M)))
library(pROC) # для построения порога отсечения
ROC.LOGIT <- roc(data$Y,data$LOGIT)
ROC.MNK <- roc(data$Y,data$MNK)
ROC.LOGIT$auc
ROC.MNK$auc

ROC.LOGIT <- roc(data$Y,data$LOGIT,plot = TRUE,percent=TRUE,
                 print.auc=TRUE,
                 col="#377eb8",lwd=4)
plot.roc(data$Y,data$MNK ,percent=TRUE, print.auc=TRUE,
         col="#4daf4a",lwd=4, add=TRUE,print.auc.y=40)
legend("bottomright", legend=c("Logit-regression","MNK-regression"),
       col=c("#377eb8","#4daf4a"),lwd=4)
####################
# Выберем порог отсечения
ROC.LOGIT.info <- roc(data$Y,data$LOGIT)
str(ROC.LOGIT.info)

ROC.df<-data.frame(Sens=ROC.LOGIT.info$sensitivities*100,
                   Spec=(1-ROC.LOGIT.info$specificities)*100,
                   cutoffs=ROC.LOGIT.info$thresholds)

Сutoff<-as.data.frame(ROC.df[ROC.df$Sens>80 & ROC.df$Spec<60,])
max(Сutoff$cutoffs)
