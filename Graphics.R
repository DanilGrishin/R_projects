setwd("C:/Rlib/Opt_portfolio_project_2")

library(dplyr)
library(ggplot2)
library(readxl)
library(tseries)
library(zoo)
library(tidyr)
library(plotly)

options(scipen = 6)

library(quantmod)

from = "2010-01-01"
to = "2021-10-31"
period = "monthly"

getSymbols(c('SBER.ME','MOEX.ME'),from=from, to=to, periodicity = period, auto.assign=TRUE)

d <- merge(SBER.ME$SBER.ME.Close,MOEX.ME$MOEX.ME.Close,join = 'left',fill='NA')
d <- na.omit(as.data.frame(d))

colnames(d) <- c("sber","moex")

d['sber_ret'] <- (d$sber/lag(d$sber,n=12)-1)*100
d['moex_ret'] <- (d$moex/lag(d$moex,n=12)-1)*100
d['Difference'] <- d$sber_ret - d$moex_ret
d['liniya'] <- 0

d <- na.omit(d)
d["Date"] <- as.Date(rownames(d))

Date_c = c(d$Date,d$Date,d$Date)
y_c = c(d$sber_ret,d$moex_ret,d$Difference)
Names_c = c('Сбербанк','Индекс мосбиржи','Спрэд')
length_names_c = c(length(d$Date),length(d$Date),length(d$Date))

df <- data.frame(Date=Date_c,Values=y_c,Legend=rep(Names_c,length_names_c))



# df_1 <- filter(df,Legend== c('sber_ret','moex_ret'))
# df_2 <- filter(df,Legend=='Difference')
# 
# ggplot(df_1) + 
#   geom_line(mapping = aes(x = Date,y = Values,colour=Legend),size=1.5) + 
#   geom_area(data = df_2,mapping = aes(x = Date,y = Values,fill=Legend),colour="#CC6666",alpha=0.3) + 
#   geom_line(data = d, mapping =  aes(x = Date,y = liniya),colour = "black",size=1,alpha=0.15)

df_1 <- filter(df,Legend== 'Сбербанк')
df_2 <- filter(df,Legend== 'Индекс мосбиржи')
df_3 <- filter(df,Legend=='Спрэд')

ggplot(df_1)+
  geom_line(mapping = aes(x=Date,y=Values,colour=Legend),size=0.8,alpha=0.8) +
  geom_point(mapping = aes(x=Date,y=Values,colour=Legend),size=1,alpha=0.8) +
  geom_line(data = df_2,mapping = aes(x=Date,y=Values,colour=Legend),size=0.8,alpha=1) +
  geom_point(data = df_2,mapping = aes(x=Date,y=Values,colour=Legend),size=1,alpha=1) +
  geom_area(data = df_3,mapping = aes(x=Date,y=Values,fill=Legend),alpha=0.2) +
  geom_line(data = d,mapping = aes(x=Date,y=Difference),colour='steelblue',size=0.2,alpha=0.2) +
  #geom_line(data = d, mapping =  aes(x = Date,y = liniya),colour = "black",size=0.2) +
  scale_color_manual(name = "",values = c('#3366FF','forestgreen')) +
  scale_fill_manual(name = "",values = c('steelblue')) +
  theme_classic() +
  xlab('Месяц') +
  ylab('% прироста г/г') +
  ggtitle("Доходности ценных бумаг") +
  theme_classic() +
  theme(
    plot.title = element_text(face="bold",colour = "black"),
    panel.grid.major = element_line(colour = "grey80",size = 0.5),
    panel.grid.minor = element_line(colour = "grey80",size = 0.5),
    legend.text = element_text(size = 14),
    #legend.title = element_text(size = 14,face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.margin = margin(2, -115, 2, 2) # Вторая цифра ответчает за то, чтобы изменять правую границу графика
  )

library(plotly)
ggplotly(ggplot(df_1)+
           geom_line(mapping = aes(x=Date,y=Values,colour=Legend),size=0.8,alpha=0.8) +
           geom_point(mapping = aes(x=Date,y=Values,colour=Legend),size=1,alpha=0.8) +
           geom_line(data = df_2,mapping = aes(x=Date,y=Values,colour=Legend),size=0.8,alpha=1) +
           geom_point(data = df_2,mapping = aes(x=Date,y=Values,colour=Legend),size=1,alpha=1) +
           geom_area(data = df_3,mapping = aes(x=Date,y=Values,fill=Legend),alpha=0.2) +
           geom_line(data = d,mapping = aes(x=Date,y=Difference),colour='steelblue',size=0.2,alpha=0.2) +
           #geom_line(data = d, mapping =  aes(x = Date,y = liniya),colour = "black",size=0.2) +
           theme_classic() +
           scale_color_manual(name = "",values = c('#3366FF','forestgreen')) +
           scale_fill_manual(name = "",values = c('steelblue')) +
           xlab('Месяц') +
           ylab('% прироста г/г') +
           ggtitle("Доходности ценных бумаг") +
           theme_classic() +
           theme(
             plot.title = element_text(face="bold",colour = "black"),
             panel.grid.major = element_line(colour = "grey80",size = 0.5),
             panel.grid.minor = element_line(colour = "grey80",size = 0.5),
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 14,face = "bold"),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14),
             plot.margin = margin(2, -145, 2, 2) # Вторая цифра ответчает за то, чтобы изменять правую границу графика
             
             )
           
          )


 







