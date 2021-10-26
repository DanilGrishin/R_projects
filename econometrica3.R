library("psych")  
library("dplyr")  
library("ggplot2")  
library("GGally") 
library("memisc")
library("lmtest")
library("sjPlot")
library("sgof")
library("foreign")
library("car")
library("hexbin")


str(Work_table)
Work_table$Blood_group<-as.factor(Work_table$Blood_group) 

Work_table$Rhesus_factor<-as.factor(Work_table$Rhesus_factor) 

Work_table$Age<-as.integer(Work_table$Age)

Ivan<-list("Ivan",21,180,75,"2","+")

Oleg<-list("Oleg",26,200,85,"1","-")

Work_table<-rbind(Work_table,Ivan,Oleg)


n<-length(Work_table$Name)
n

ID<-c(1:n)
Work_table<-cbind(Work_table,ID)
Work_table$IMB<-Work_table$Weight/((Work_table$Height)*0.01)^2

trash1<-Work_table[,-7]
trash2<-Work_table[-2, ]
trash3<-Work_table[-c(1:5),] # Удалим первые пять строк
Work_table[1,"Name"]<-"super_vanya"

Final<-Work_table[Work_table$Age<=23,]
Final2<-Work_table[Work_table$Age>21,]

Find<-Final[order(Final$Name),]
Find
