library (glmnet)
library (car)
library (ridge)
wine =read.csv("winequality-red.csv",sep = ",",header = TRUE)
names(wine)

#(wine.pred.names = names(wine)[1: length(names(wine)) -1])
#wine.pred =wine[,wine.pred.names]
#rm(wine.pred)

wine.pred <- subset(wine,select = -quality)

set.seed(1)
(trainIndex=sample(1:nrow(wine.pred), nrow(wine.pred)/2))# индексы случайно выбранных полвины данных

x<-wine.pred[trainIndex, ] # training data На этих данных строим модель
x.test<-wine.pred[-trainIndex, ] # test data На этих будем прогнозировать

y =wine$quality[trainIndex]
y.test=wine$quality[-trainIndex]

lm.mod= lm(data = x,y~.)
summary(lm.mod)
vif (lm.mod)


lambda.seq = 10^seq(5, -2, length=100)
xTab<-as.matrix(x)#!!!
ridge.mod=glmnet(xTab,y,alpha=0,lambda=lambda.seq)
plot(ridge.mod,xvar='lambda')

lasso.mod=glmnet(xTab,y,alpha=1,lambda=lambda.seq)
plot( lasso.mod,xvar='lambda')

set.seed(1)
ridge.cv.out=cv.glmnet(xTab,y,alpha=0)
set.seed(1)
lasso.cv.out=cv.glmnet(xTab,y,alpha=1)

plot(ridge.cv.out)
plot( lasso.cv.out)

(ridge.bestlam=ridge.cv.out$lambda.min)
(ridge.lam1se=ridge.cv.out$lambda.1se)
(lasso.bestlam=lasso.cv.out$lambda.min)
(lasso.lam1se=lasso.cv.out$lambda.1se)


ridge.mod.best=glmnet(xTab,y,alpha=0,lambda=ridge.bestlam)
coef(ridge.mod.best)
ridge.mod.1se = glmnet(xTab,y,alpha=0,lambda=ridge.lam1se)
coef(ridge.mod.1se)
lasso.mod.best=glmnet(xTab,y,alpha=1,lambda=lasso.bestlam)
coef(lasso.mod.best)
lasso.mod.1se = glmnet(xTab,y,alpha=1,lambda=lasso.lam1se)
coef(lasso.mod.1se)

linRidgeMod <- linearRidge(data = x,y~.)
summary(linRidgeMod)


RSS_ridge.mod.best <- deviance(ridge.mod.best)
RSS_lasso.mod.best <- deviance(lasso.mod.best)
RSS_lm.mod <- deviance(lm.mod)

predicted.linRidgeMod <- predict(linRidgeMod, x.test)
x.test.matrix<-as.matrix(x.test)
predicted.RidgeModBest <-c(predict(ridge.mod.best, x.test.matrix))
predicted.Lassomodbest <-c(predict(lasso.mod.best, x.test.matrix))
predicted.lm.mod<-c(predict(lm.mod, x.test))

compare <- cbind (Y=y.test, linRidgeMod=predicted.linRidgeMod,
                  RidgeModBest=predicted.RidgeModBest,
                  Lassomodbest=predicted.Lassomodbest,
                  lm.mod=predicted.lm.mod) 
Compare <-as.data.frame(compare)

(sum(Compare$Y-Compare$linRidgeMod)^2)/nrow(Compare)
(sum(Compare$Y-Compare$RidgeModBest)^2)/nrow(Compare)
(sum(Compare$Y-Compare$Lassomodbest)^2)/nrow(Compare)
(sum(Compare$Y-Compare$lm.mod)^2)/nrow(Compare)

compare1 <- cbind (Y=y.test, linRidgeMod=predicted.linRidgeMod)  # combine
(S_linRidgeMod<-(mean (apply(compare1, 1, min)/apply(compare1, 1, max))))

compare2 <- cbind (Y=y.test, RidgeModBest=predicted.RidgeModBest)  # combine
(S_RidgeModBest<-(mean (apply(compare2, 1, min)/apply(compare2, 1, max))))

compare3 <- cbind (Y=y.test, Lassomodbest=predicted.Lassomodbest)  # combine
(S_Lassomodbest<-(mean (apply(compare3, 1, min)/apply(compare3, 1, max))))

compare4 <- cbind (Y=y.test, lm.mod=predicted.lm.mod)  # combine
(S_lm.mod<-(mean (apply(compare4, 1, min)/apply(compare4, 1, max))))
