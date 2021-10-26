
library("fGarch")
library("FinTS")
library("quantmod")
library("tseries")

#Получить данные котировки Старбакс с 2006 года.
library(quantmod)
#getSymbols –  умеет сама скачивать котировки с сайта Yahoo
getSymbols("SBUX", from="2006-01-01", to="2015-01-01")

p <- as.numeric(Ad(SBUX)) #Ad - столбец Adjusted в таблице SBUX

plot(p, type="l", main="SBUX price", xlab="Price")

#Движение котировок подчиняются логнормальному процессу, то есть 
#приращения распределены нормально. 
# На этом строится вся наука о ценообразовании опционов
L <- length(p)
ret <- p[2:L]/p[1:(L-1)] - 1 #доходность
hist(ret, breaks = 40)

#Волатильность (в финансовом контексте) – мера разброса, вариация 
#доходностей, дисперсия распределения доходностей
#Чем больше разброс, тем выше волатильность
#и тем труднее нам делать предположение о цене в будущем. 

var(ret)  #дисперсия (вариация)
sd(ret)   #среднеквадратическое отклонение, корень из дисперсии

#Сравнивать акции на основе стандартного отклонения 
#можно при равных доходностях.
#А чтобы сравнивать разные акции
#нужно использовать комбинированные метрики.
#Например, Коэффициент Шарпа: отношение средней ожидаемой доходности 
# к ее стандартному отклонению:

mean(ret)/sd(ret)


vol <- c()
for (i in 31:length(ret)) {
  vol <- c(vol, sd(ret[(i-30):i]))
  RR<- sd(ret[(i-30):i]) }
plot(vol, type="l", main="Плато волатильности")
#Совершенно явные плато, видимые зависимости.
#Скорее всего и ACF будет значима.
acf(vol)

#Квадраты доходностей иногда используют как меру волатильности. 
#сами доходности не автокоррелированы!!
acf(ret)
acf(ret^2)
gfit <- garchFit(data=ret, trace=FALSE)
summary(gfit)

gfit <- garchFit(formula= ~garch(5,5),data=ret, trace=FALSE)
gfit

gfit <- garchFit(formula= ~arma(1,1) + garch(1,1), data=ret, trace=FALSE)
summary(gfit)

gfit <- garchFit(formula= ~arma(1,1) + garch(1,1), data=ret, trace=FALSE, include.delta=TRUE)
predict(gfit, 50, plot=TRUE, conf=0.95) #50-число точек прогноза

#VaR Value-at-Risk
#Какой максимальный убыток я могу ожидать в течение 
#определенного отрезка времени с заданным уровнем вероятности
#(доверия)например, 5%

#это  квантиль на уровне 5%. 
#То есть это такая доходность, что в 95% случаев
#у нас дела будут лучше. 
quantile(ret, 0.05)
#VaR - это лучшее из 5% худших случаев. А что лежит в тех 5%?…
#ES (Expected shortfall) - Это среднее значение по всем тем точкам, 
#что оказались левее VaR, То есть среднее по худшим 5%.
mean(ret[ret <= quantile(ret, 0.05)])

#Нижняя линия – 95% доверительный интервал, это и есть VaR.
predict(gfit, 1)[1]
VaR <- as.numeric(predict(gfit, 1)[1] - 1.96*predict(gfit, 1)[3])
VaR

N <- 500 # Для больших моделей нужно больше точек
test <- ret[(N+1):length(ret)]
VaR <- rep(0, length(test))
for (i in (N+1):length(ret)){
  #cat("\r", i-N, "of", length(ret)-N)
  train <- ret[(i-N):(i-1)]
  gfit <- garchFit( formula= ~arma(1,1) + garch(1,1), data=train, trace=FALSE, include.delta=TRUE)
  VaR[i-N] <- as.numeric(predict(gfit, 1)[1] - 1.96*predict(gfit, 1)[3])
}
plot(test, type="l", main = "Кривая VaR при моделировании ~ARMA(1,1) + GARCH(1,1)")
lines(VaR, col="red")
