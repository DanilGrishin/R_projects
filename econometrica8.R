library(HSAUR)
library(dplyr)
library(psych)
library(memisc)
library("car")# для vif


p <- main
str(p)
y <- p$Income
mean(y)
sd(y)
help(scale)
y_st <- scale(y) # стандартизуем
mean(y_st)
sd(y_st)
p_st<-mutate_each(p,"scale")

p <- subset(p,select = -Income)
p <- p[,-1]

p.PC <- prcomp(p,scale = TRUE)
plot(p.PC)

summary(p.PC)
pca1 <- p.PC$x[,1]
pca2 <- p.PC$x[,2]
pca3 <- p.PC$x[,3]
pca4 <- p.PC$x[,4]

v1 <- p.PC$rotation[,1]
v1
p.PC$rotation

x <- model.matrix(data=main, Income~0+Height+Weight+Hair+Shoes+Age+Beer+Wine+ Sex+Strength+Region+IQ)
cor(x)
newp <- data.frame(y_st,pca1,pca2,pca3,pca4)
xpc <- model.matrix(data=newp, y_st~0+pca1+pca2+pca3+pca4)
cor(xpc)

#m1<-lm(data=main, Income~Height+Weight+Hair+Shoes+Age+Beer+Wine+ Sex+Strength+Region+IQ)
main <- main[,-1]
m1<-lm(data=main,Income~.)
coef(m1)
vif(m1)
summary(m1)

m2.mgk<-lm(data=newp, y_st~pca1+pca2+pca3+pca4)
coef(m2.mgk)
vif(m2.mgk)

p_st <- p_st[,-1]
m3.st<-lm(data=p_st, Income~.)
coef(m3.st)
vif(m3.st)

summary(m1)
summary(m2.mgk)
summary(m3.st)

point<-data.frame(pca1=2,pca2=min(pca2),pca3=max(pca3),pca4=mean(pca4))
point<-data.frame(pca1=1,pca2=1,pca3=1,pca4=1)
point<-data.frame(pca1=mean(pca1),pca2=mean(pca2),pca3=mean(pca3),pca4=mean(pca4))
point<-data.frame(pca1=max(pca1)-mean(pca1),pca2=max(pca1)-mean(pca2),pca3=max(pca1)-mean(pca3),pca4=mean(pca4))

z<-(predict(m2.mgk,point))
z
abs(z)*sd(y)+mean(y)
